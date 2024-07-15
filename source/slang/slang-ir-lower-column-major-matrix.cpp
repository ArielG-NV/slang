#include "slang-ir-legalize-image-subscript.h"

#include "slang-ir.h"
#include "slang-ir-insts.h"
#include "slang-ir-util.h"
#include "slang-ir-clone.h"
#include "slang-ir-specialize-address-space.h"
#include "slang-parameter-binding.h"
#include "slang-ir-legalize-varying-params.h"

namespace Slang
{
    struct LowerColumnMajorMatrixContext
    {
        HashSet<IRInst*> alreadyProcessed;
        IRModule* m_module;
        IRBuilder& m_builder;
        DiagnosticSink* m_sink;

        IRInst* rowMajorInst;
        LowerColumnMajorMatrixContext(IRModule* module, IRBuilder& builder, DiagnosticSink* sink) : 
            m_module(module), m_builder(builder), m_sink(sink)
        {
            rowMajorInst = builder.getIntValue(builder.getIntType(), MatrixLayoutMode::kMatrixLayoutMode_RowMajor);
        }

        IRMatrixType* getMatrixType(IRType* type)
        {
            if (auto matrixType = as<IRMatrixType>(type))
                return matrixType;
            return nullptr;
        }

        IRVectorType* getVectorType(IRType* type)
        {
            if (auto vectorType = as<IRVectorType>(type))
                return vectorType;
            return nullptr;
        }

        IRMatrixType* getRowMajorFromColumnMajor(IRMatrixType* matrixType)
        {
            return m_builder.getMatrixType(matrixType->getElementType(), matrixType->getColumnCount(), matrixType->getRowCount(), rowMajorInst);
        }

        bool isColumnMajor(IRMatrixType* matrixType)
        {
            if (!matrixType)
                return false;
            auto layoutType = as<IRIntLit>(matrixType->getLayout());
            // short circuits if layoutType is nullptr
            return layoutType && layoutType->getValue() == MatrixLayoutMode::kMatrixLayoutMode_ColumnMajor;
        }

        void legalizeUses(IRInst* inst, IRMatrixType* matrixType, bool initialPass)
        {
            IRUse* nextUse = inst->firstUse;
            for (auto currentUse = nextUse; nextUse; currentUse = nextUse)
            {
                auto user = currentUse->getUser();
                nextUse = currentUse->nextUse;
                legalizeInst(user, matrixType, initialPass);
            }
        }

        void legalizeVariableLikeUse(IRInst* inst, IRMatrixType* matrixType)
        {
            // legalize uses of a variable-like-type
            legalizeUses(inst, matrixType, false);
        }

        void legalizeFuncType(IRFuncType* inst, IRMatrixType* matrixType)
        {
            List<IRType*> paramTypeList;
            paramTypeList.reserve(inst->operandCount);
            IRType* resultType = inst->getResultType();
            for (auto param : inst->getParamTypes())
            {
                if (param == matrixType)
                    paramTypeList.add(getRowMajorFromColumnMajor(matrixType));
                else
                    paramTypeList.add(param);
            }
            if (resultType == matrixType)
                resultType = matrixType;

            inst->replaceUsesWith(m_builder.getFuncType(paramTypeList, resultType));
        }

        void legalizeMultiply(IRInst* inst, IRMatrixType* matrixType)
        {
            SLANG_UNUSED(matrixType);

            auto leftInst = inst->getOperand(0);
            auto rightInst = inst->getOperand(1);

            auto leftInstDataType = leftInst->getDataType();
            auto rightInstDataType = rightInst->getDataType();

            auto leftMatrixType = getMatrixType(leftInstDataType);
            auto rightMatrixType = getMatrixType(rightInstDataType);

            int numberOfColumnMajorOps = 0;
            if (isColumnMajor(leftMatrixType))
                numberOfColumnMajorOps++;
            if (isColumnMajor(rightMatrixType))
                numberOfColumnMajorOps++;

            if (numberOfColumnMajorOps == 2 
                || numberOfColumnMajorOps == 1 && (getVectorType(leftInstDataType) || getVectorType(rightInstDataType)))
            {
                inst->setOperand(0, rightInst);
                inst->setOperand(1, leftInst);
                return;
            }
        }
        
        enum class LegalizeGetElementObjOptions
        {
            GetElement = 0,
            GetElementPtr = 1,
        };
        template<LegalizeGetElementObjOptions legalizeGetElementObjOptions, typename InstType>
        void legalizeGetElementObj(InstType* inst, IRMatrixType* matrixType)
        {
            // Get column vector from a row matrix through the logic of adding every element in the vector posiotion of `X` for the given matrix: `M[X][COLUMN]`
            m_builder.setInsertBefore(inst);
            
            List<IRInst*> vectorElements;
            
            IRIntLit* rowCountInst = as<IRIntLit>(matrixType->getRowCount());
            SLANG_ASSERT(rowCountInst);
            auto rowCount = rowCountInst->getValue();

            vectorElements.reserve(rowCount);
            for (auto i = 0; i < rowCount; i++)
            {
                vectorElements.add(
                    m_builder.emitElementExtract(
                        m_builder.emitElementExtract(inst->getBase(), m_builder.getIntValue(m_builder.getIntType(), i)),
                        inst->getIndex())
                    );
            }

            IRInst* newGetElement = nullptr;
            auto vecType = m_builder.getVectorType(matrixType->getElementType(), rowCount);
            if constexpr (legalizeGetElementObjOptions == LegalizeGetElementObjOptions::GetElement)
            {
                newGetElement = m_builder.emitMakeVector(vecType, vectorElements);
            }
            else
            {
                newGetElement = m_builder.emitGetAddress(
                    m_builder.getPtrType(vecType),
                    m_builder.emitMakeVector(vecType, vectorElements)
                );
            }
            inst->replaceUsesWith(newGetElement);
            inst->removeAndDeallocate();
        }
        void legalizeGetElement(IRGetElement* inst, IRMatrixType* matrixType)
        {
            legalizeGetElementObj<LegalizeGetElementObjOptions::GetElement>(inst, matrixType);
        }
        void legalizeGetElementPtr(IRGetElementPtr* inst, IRMatrixType* matrixType)
        {
            legalizeGetElementObj<LegalizeGetElementObjOptions::GetElementPtr>(inst, matrixType);
        }
        void legalizeMakeMatrix(IRInst* inst, IRMatrixType* matrixType)
        {
            // 1. Directly translate float4x4(e1,e2,e3,e4,e5...) into a matrix would be faster
            // 2. Directly translate 'float4x4 into float3x3' or '(vec3,vec3,vec3) into float3x3'
            // will likely be simmilar to the performance of a transpose.

            // 4 cases:
            auto firstOpDataType = inst->getOperand(0)->getDataType();
            auto isFirstOpVec = as<IRVectorType>(firstOpDataType);
            auto isFirstOpMat = as<IRMatrixType>(firstOpDataType);
            if (!isFirstOpVec && !isFirstOpMat)
            {
                // case 1. All scalar: swap all elements assuming the linear list of operands as a matrix
                // [0][3] swaps with [3][0], diagonal of matrix is untouched.
                m_builder.setInsertBefore(inst);
                auto columnCount = as<IRIntLit>(matrixType->getColumnCount())->getValue();
                auto rowCount = as<IRIntLit>(matrixType->getRowCount())->getValue();
                for (auto row = 0; row < rowCount/2; row++)
                {
                    for (auto column = 0; column < columnCount/2; column++)
                    {
                        if (column == row)
                            continue;

                        auto rowElem = row * rowCount + column;
                        auto colElem = column * columnCount + row;

                        inst->setOperand(rowElem, inst->getOperand(colElem));
                        inst->setOperand(colElem, inst->getOperand(rowElem));
                    }
                }
            }
            else
            {
                // To legalize we will "get a transpose" of the result matrix. Other methods would likely
                // not be much faster (or slower).
                // case 2. all vector.
                // case 3. larger matrix.
                // case 4. vector and matrix.
                m_builder.setInsertAfter(inst);
                auto transposedVar = m_builder.emitVar(inst->getDataType());
                m_builder.emitStore(transposedVar, m_builder.emitTranspose(inst));
                inst->replaceUsesWith(transposedVar);
            }
        }
        void legalizeInst(IRInst* inst, IRMatrixType* matrixType, bool initialPass)
        {
            if (alreadyProcessed.contains(inst))
                return;
            alreadyProcessed.add(inst);
            switch(inst->getOp())
            {
                case kIROp_FuncType:
                    legalizeFuncType(as<IRFuncType>(inst), matrixType);
                    break;
                case kIROp_ExplicitMul:
                case kIROp_Mul:
                    legalizeMultiply(inst, matrixType);
                    break;
                case kIROp_GetElement:
                    legalizeGetElement(as<IRGetElement>(inst), matrixType);
                    break;
                case kIROp_GetElementPtr:
                    legalizeGetElementPtr(as<IRGetElementPtr>(inst), matrixType);
                    break;
                case kIROp_MakeMatrix:
                    legalizeVariableLikeUse(inst, matrixType);
                    // DXC does not rearange based on constructor.
                    // `row_major float4x4(x)` is the same as `column_major float4x4(y)`
                    // DXC treats a column_major as a row_major when multiplying,
                    // transpose'ing is up to the user
                    //legalizeMakeMatrix(inst, matrixType);
                    break;
                default:
                    break;
            }
            if (initialPass)
                legalizeVariableLikeUse(inst, matrixType);
        }

        void legalizeModule()
        {
            /*
            TODO: only legalize global stores and loads since this is what DXC does (to minimize computation of row_major to column_major

            Currently CPU/CUDA/Metal ignore the matrix layout and just assume all matrixes are `row_major matrix`.

            If a Matrix identifies as `maybeColumnMajorMatrixType->getLayout() == kMatrixLayoutMode_ColumnMajor`
            we require to legalize this matrix and all immediate derivative uses of the matrixType/Variable
            */

            // Find the global kIROp_MatrixType inst that needs legalization of uses
            for (auto globalInst : m_module->getModuleInst()->getChildren())
            {
                auto maybeColumnMajorMatrixType = as<IRMatrixType>(globalInst);
                if (!maybeColumnMajorMatrixType)
                    continue;
                if (!isColumnMajor(maybeColumnMajorMatrixType))
                    continue;

                legalizeUses(maybeColumnMajorMatrixType, maybeColumnMajorMatrixType, true);
                globalInst->replaceUsesWith(getRowMajorFromColumnMajor(maybeColumnMajorMatrixType));
            }
        }
    };

    void lowerColumnMajorMatrix(IRModule* module, DiagnosticSink* sink)
    {
        // We need to fix the 'uses' of a matrix<> type if we cannot specify
        // `column_major` on a target
        //
        // This means we must find all var like types `KIROp_GlobalVar, KIROp_GlobalParam, KIROp_Var,
        // KIROp_GlobalConstant, KIROp_Param` and legalize any direct use of them or the given MatrixType
        //
        IRBuilder builder(module);
        LowerColumnMajorMatrixContext context = LowerColumnMajorMatrixContext(module, builder, sink);
        context.legalizeModule();
    }
}

