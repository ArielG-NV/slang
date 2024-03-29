// slang-ir-hlsl-legalize.cpp
#include "slang-ir-glsl-legalize.h"

#include <functional>

#include "slang-ir.h"
#include "slang-ir-insts.h"
#include "slang-ir-inst-pass-base.h"
#include "slang-ir-specialize-function-call.h"
#include "slang-ir-util.h"
#include "../../external/spirv-headers/include/spirv/unified1/spirv.h"

namespace Slang
{

void searchChildrenForForceVarIntoStructTemporarily(IRModule* module, IRInst* inst)
{
    for(auto child : inst->getChildren())
    {
        switch(child->getOp())
        {
        case kIROp_Block:
        {
            searchChildrenForForceVarIntoStructTemporarily(module, child);
            break;
        }
        case kIROp_Call:
        {
            auto call = as<IRCall>(child);
            for(int i = 0; i < call->getArgCount(); i++)
            {
                auto arg = call->getArg(i);
                if(arg->getOp() != kIROp_ForceVarIntoStructTemporarily)
                    continue;
                auto forceStructArg = arg->getOperand(0);
                auto forceStructBaseType = as<IRType>(forceStructArg->getDataType()->getOperand(0));
                if(forceStructBaseType->getOp() == kIROp_StructType)
                {
                    arg->replaceUsesWith(arg->getOperand(0));
                    arg->removeAndDeallocate();
                    continue;   
                }

                // Replace all non struct inputs with a pattern:
                // {NonStructToStructCopy; {pass as parameter to kIROp_Call}; StructToNonStructCopy;}
                IRBuilder builder(call);

                // emit struct before function use                
                builder.setInsertBefore(call->getCallee());
                auto structType = builder.createStructType();
                StringBuilder structName;
                builder.addNameHintDecoration(structType, UnownedStringSlice("ForceVarIntoStructTemporarily_t"));

                auto elementBufferKey = builder.createStructKey();
                builder.addNameHintDecoration(elementBufferKey, UnownedStringSlice("data"));
                auto _dataField = builder.createStructField(structType, elementBufferKey, forceStructBaseType);

                // emit copy to struct
                builder.setInsertBefore(call);
                auto structVar = builder.emitVar(structType);
                builder.addNameHintDecoration(structVar, UnownedStringSlice("forceVarIntoStructTemporarily"));
                builder.emitStore(
                    builder.emitFieldAddress(builder.getPtrType(_dataField->getFieldType()), structVar, _dataField->getKey()),
                    builder.emitLoad(forceStructArg)
                    );

                // replace kIROp_ForceVarIntoStructTemporarily with struct created
                arg->replaceUsesWith(structVar);
                arg->removeAndDeallocate();

                // emit copy to non-struct
                builder.setInsertAfter(call);
                builder.emitStore(
                    forceStructArg,
                    builder.emitFieldAddress(builder.getPtrType(_dataField->getFieldType()), structVar, _dataField->getKey())
                    );
            }
            break;
        }
        }
    }
}

void legalizeNonStructParameterToStructAndBackHLSL(IRModule* module)
{
    for(const auto globalInst : module->getGlobalInsts())
    {
        if(globalInst->getOp() == kIROp_Func)
        {
            searchChildrenForForceVarIntoStructTemporarily(module, globalInst);
        }
    }
}

} // namespace Slang
