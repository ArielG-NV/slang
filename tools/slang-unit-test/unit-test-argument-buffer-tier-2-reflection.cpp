// unit-test-argument-buffer-tier-2-reflection.cpp

#include "../../source/core/slang-io.h"
#include "../../source/core/slang-process.h"
#include "slang-com-ptr.h"
#include "slang.h"
#include "unit-test/slang-unit-test.h"

#include <stdio.h>
#include <stdlib.h>

using namespace Slang;

// Test metal argument buffer tier2 layout rules.

SLANG_UNIT_TEST(metalArgumentBufferTier2Reflection)
{
    const char* userSourceBody = R"(
        struct A
        {
          float3 one;
          float3 two;
          float three;
        }

        struct Args{
          ParameterBlock<A> a;
        }
        ParameterBlock<Args> argument_buffer;
        RWStructuredBuffer<float> outputBuffer;

        [numthreads(1,1,1)]
        void computeMain()
        {
            outputBuffer[0] = argument_buffer.a.two.x;
        }
        )";

    auto moduleName = "moduleG" + String(Process::getId());
    String userSource = "import " + moduleName + ";\n" + userSourceBody;
    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK(slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);
    slang::TargetDesc targetDesc = {};
    targetDesc.format = SLANG_SPIRV;
    targetDesc.profile = globalSession->findProfile("spirv_1_5");
    slang::SessionDesc sessionDesc = {};
    sessionDesc.targetCount = 1;
    sessionDesc.targets = &targetDesc;
    ComPtr<slang::ISession> session;
    SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

    ComPtr<slang::IBlob> diagnosticBlob;
    auto module = session->loadModuleFromSourceString(
        "m",
        "m.slang",
        userSourceBody,
        diagnosticBlob.writeRef());
    SLANG_CHECK(module != nullptr);

    auto layout = module->getLayout();

    auto type = layout->findTypeByName("A");
    auto typeLayout = layout->getTypeLayout(type, slang::LayoutRules::MetalArgumentBufferTier2);
    SLANG_CHECK(typeLayout->getFieldByIndex(0)->getOffset() == 0);
    SLANG_CHECK(typeLayout->getFieldByIndex(0)->getTypeLayout()->getSize() == 16);
    SLANG_CHECK(typeLayout->getFieldByIndex(1)->getOffset() == 16);
    SLANG_CHECK(typeLayout->getFieldByIndex(1)->getTypeLayout()->getSize() == 16);
    SLANG_CHECK(typeLayout->getFieldByIndex(2)->getOffset() == 32);
    SLANG_CHECK(typeLayout->getFieldByIndex(2)->getTypeLayout()->getSize() == 4);
}


static slang::TypeLayoutReflection* _unwrapParameterGroups(
    slang::TypeLayoutReflection* typeLayout)
{
    for (;;)
    {
        if (!typeLayout->getType())
        {
            if (auto elementTypeLayout = typeLayout->getElementTypeLayout())
                typeLayout = elementTypeLayout;
        }
        switch (typeLayout->getKind())
        {
        case slang::TypeReflection::Kind::Array:
            typeLayout = typeLayout->getElementTypeLayout();
            return typeLayout;
        case slang::TypeReflection::Kind::Resource:
            {
                if (typeLayout->getResourceShape() != SLANG_STRUCTURED_BUFFER)
                    break;
                typeLayout = typeLayout->getElementTypeLayout();
            }
            return typeLayout;
        case slang::TypeReflection::Kind::ConstantBuffer:
        case slang::TypeReflection::Kind::ParameterBlock:
            typeLayout = typeLayout->getElementTypeLayout();
            continue;
        default:
            return typeLayout;
        }
    }
}

SLANG_UNIT_TEST(metalArgumentBufferTier2NestedReflection)
{
    const char* userSourceBody = R"(
        struct CB
        {
            uint4 value;
        }

        struct MaterialSystem
        {
            CB cb;
            StructuredBuffer<uint4> data;
        }

        struct Scene
        {
            CB sceneCb;
            StructuredBuffer<uint4> data;
            ParameterBlock<MaterialSystem> material;
        }

        struct PerView
        {
            uint4 value;
        }

        ConstantBuffer<PerView> perView;

        ParameterBlock<Scene> scene;

        RWStructuredBuffer<uint4> resultBuffer;

        [shader("compute")]
        [numthreads(1,1,1)]
        void computeMain(uint3 sv_dispatchThreadID : SV_DispatchThreadID)
        {
            resultBuffer[sv_dispatchThreadID.x] = perView.value.x + scene.sceneCb.value.x + scene.data[0].x + scene.material.cb.value.x + scene.material.data[0].x;
        }
        )";

    auto moduleName = "moduleG" + String(Process::getId());
    String userSource = "import " + moduleName + ";\n" + userSourceBody;
    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK(slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);
    slang::TargetDesc targetDesc = {};
    targetDesc.format = SLANG_METAL;
    slang::SessionDesc sessionDesc = {};
    sessionDesc.targetCount = 1;
    sessionDesc.targets = &targetDesc;
    ComPtr<slang::ISession> session;
    SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

    ComPtr<slang::IBlob> diagnosticBlob;
    auto module = session->loadModuleFromSourceString(
        "m",
        "m.slang",
        userSourceBody,
        diagnosticBlob.writeRef());
    SLANG_CHECK(module != nullptr);

    auto layout = module->getLayout();
    SLANG_ASSERT(layout);

    // Walk the entire layout to ensure nothing is broken
    auto globalParamsVarLayout = layout->getGlobalParamsVarLayout();
    SLANG_ASSERT(globalParamsVarLayout);

    auto globalParamsTypeLayout = globalParamsVarLayout->getTypeLayout();
    SLANG_ASSERT(globalParamsVarLayout);

    auto unwrappedGlobalParamsTypeLayout = _unwrapParameterGroups(globalParamsTypeLayout);
    SLANG_ASSERT(globalParamsVarLayout);

    auto outerParamBlockLayout = unwrappedGlobalParamsTypeLayout->getBindingRangeLeafTypeLayout(1);
    SLANG_ASSERT(outerParamBlockLayout);

    auto outerParamBlockKind = unwrappedGlobalParamsTypeLayout->getBindingRangeType(1);

    auto unwrappedOuterParamBlockLayout =
        _unwrapParameterGroups(outerParamBlockLayout);
    SLANG_ASSERT(unwrappedOuterParamBlockLayout);

    unwrappedOuterParamBlockLayout = session->getTypeLayout(
        unwrappedOuterParamBlockLayout->getType(),
        0,
        slang::LayoutRules::MetalArgumentBufferTier2);
    SLANG_ASSERT(unwrappedOuterParamBlockLayout);

    auto vectorOfOuterParamBlock = unwrappedOuterParamBlockLayout->getBindingRangeLeafTypeLayout(0);
    SLANG_ASSERT(vectorOfOuterParamBlock);

    auto innerParamBlockLayout = unwrappedOuterParamBlockLayout->getBindingRangeLeafTypeLayout(1);
    SLANG_ASSERT(innerParamBlockLayout);

    auto elementLayoutOfInnerParamBlockLayout = innerParamBlockLayout->getElementTypeLayout();
    SLANG_ASSERT(elementLayoutOfInnerParamBlockLayout);
    
    auto cbField = elementLayoutOfInnerParamBlockLayout->getFieldByIndex(0);
    SLANG_ASSERT(cbField);

    auto cbFieldLayout = cbField->getTypeLayout();
    SLANG_ASSERT(cbFieldLayout);
    
    auto valueField = cbFieldLayout->getFieldByIndex(0);
    SLANG_ASSERT(valueField);

    auto valueFieldLayout = valueField->getTypeLayout();
    SLANG_ASSERT(valueFieldLayout);
    SLANG_ASSERT(valueFieldLayout->getSize() == 16);
}
