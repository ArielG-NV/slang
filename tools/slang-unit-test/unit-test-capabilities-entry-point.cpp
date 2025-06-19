// unit-test-cuda-compile.cpp

#include "../../source/core/slang-io.h"
#include "../../source/core/slang-process.h"
#include "slang-com-ptr.h"
#include "slang.h"
#include "unit-test/slang-unit-test.h"

#include <stdio.h>
#include <stdlib.h>

using namespace Slang;

// Test that the compilation API can be used to produce CUDA source.

SLANG_UNIT_TEST(capabilitiesEntryPoint)
{
    const char* userSourceBody = R"(
[require(spvRayQueryKHR)]
[shader("compute")]
[numthreads(16, 16, 4)] // u, v, batch index
void main(uint3 DTid: SV_DispatchThreadID)
{
}
        )";

    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK(slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);

    slang::SessionDesc desc = {};
    slang::TargetDesc targetDesc = {};
    targetDesc.format = SLANG_SPIRV;
    targetDesc.profile = globalSession->findProfile("spirv_1_6");
    targetDesc.flags = SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY;

    std::vector<slang::CompilerOptionEntry> targetOptionEntries;
    targetOptionEntries.push_back(slang::CompilerOptionEntry{
        slang::CompilerOptionName::MacroDefine,
        {.stringValue0 = "RAY_QUERY", .stringValue1 = "1"}});
    targetOptionEntries.push_back(slang::CompilerOptionEntry{
        slang::CompilerOptionName::Capability,
        {.intValue0 = globalSession->findCapability("spvRayQueryKHR")}});

    if (!targetOptionEntries.empty())
    {
        targetDesc.compilerOptionEntryCount = static_cast<uint32_t>(targetOptionEntries.size());
        targetDesc.compilerOptionEntries = targetOptionEntries.data();
    }

    desc.targets = &targetDesc;
    desc.targetCount = 1;

    ComPtr<slang::ISession> session;
    SLANG_CHECK(globalSession->createSession(desc, session.writeRef()) == SLANG_OK);

    ComPtr<ISlangBlob> diagnosticBlob = {};
    auto module = session->loadModuleFromSourceString(
        "m",
        "m.slang",
        userSourceBody,
        diagnosticBlob.writeRef());
    SLANG_CHECK(module != nullptr);
    SLANG_CHECK(!diagnosticBlob.get() || diagnosticBlob.get()->getBufferSize() == 0)
    ComPtr<slang::IComponentType> linkedProgram;
    module->link(linkedProgram.writeRef(), diagnosticBlob.writeRef());
    SLANG_CHECK(!diagnosticBlob.get() || diagnosticBlob.get()->getBufferSize() == 0)
    SLANG_CHECK(linkedProgram != nullptr);

    ComPtr<slang::IBlob> code;
    linkedProgram->getTargetCode(0, code.writeRef(), diagnosticBlob.writeRef());
    SLANG_CHECK(!diagnosticBlob.get() || diagnosticBlob.get()->getBufferSize() == 0)
    SLANG_CHECK(code != nullptr);
    SLANG_CHECK(code->getBufferSize() != 0);
    String text = String((char*)code->getBufferPointer());
}
