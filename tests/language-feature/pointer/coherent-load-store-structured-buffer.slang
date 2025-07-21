//TEST:SIMPLE(filecheck=SPIRV):-stage compute -entry computeMain -target spirv
//TEST(compute):COMPARE_COMPUTE(filecheck-buffer=CHECK):-vk -output-using-type -emit-spirv-directly

// Tests if we pass-through and handle buffers correctly.
// Ensure SPIRV emits coherent operations here
// SPIRV: MakePointerVisible
// SPIRV: MakePointerAvailable

// CHECK: 0
// CHECK-NEXT: 2

//TEST_INPUT:ubuffer(data=[1 2 3], stride=4):name=inputBuffer
RWStructuredBuffer<int> inputBuffer;

//TEST_INPUT:ubuffer(data=[0 0 0], stride=4):out,name=outputBuffer
RWStructuredBuffer<int> outputBuffer;

[numthreads(32, 1, 1)]
void computeMain()
{
    Ptr<int[]> ptrIn = __getStructuredBufferPtr(inputBuffer);
    Ptr<int[]> ptrOut = __getStructuredBufferPtr(outputBuffer);

    coherentStore(ptrOut, coherentLoad(&ptrIn[1], 4, MemoryScope::Device), 4, MemoryScope::Device);
}