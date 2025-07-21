//TEST:SIMPLE(filecheck=SPIRV):-stage compute -entry computeMain -target spirv
//TEST(compute):COMPARE_COMPUTE(filecheck-buffer=CHECK):-vk -output-using-type -emit-spirv-directly

// Ensure SPIRV emits coherent operations here
// SPIRV: MakePointerVisible
// SPIRV: MakePointerAvailable

// CHECK: 0
// CHECK-NEXT: 2

//TEST_INPUT:ubuffer(data=[1 2 3], stride=4):name=inputBuffer
uniform int32_t* inputBuffer;

//TEST_INPUT:ubuffer(data=[0 0 0], stride=4):out,name=outputBuffer
uniform int32_t* outputBuffer;

[numthreads(32, 1, 1)]
void computeMain()
{
    int32_t* ptrIn = inputBuffer;
    int32_t* ptrOut = outputBuffer;

    coherentStore(ptrOut, coherentLoad(&ptrIn[1], 4, MemoryScope::Device), 4, MemoryScope::Device);
}