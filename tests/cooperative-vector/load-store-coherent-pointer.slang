//TEST:SIMPLE(filecheck=SPIRV):-stage compute -entry computeMain -target spirv
//TEST(compute):COMPARE_COMPUTE(filecheck-buffer=CHECK):-vk -render-feature cooperative-vector -emit-spirv-directly

//TEST_INPUT: set inputBuffer = ubuffer(data=[1 2 3 4 5 6 7 8 9 10 11 12], stride=4);
uniform int32_t* inputBuffer;

//TEST_INPUT: set outputBuffer = out ubuffer(data=[0 0 0 0 0 0 0 0], stride=4);
uniform int32_t* outputBuffer;

//SPIRV-DAG: OpCooperativeVectorStoreNV{{.*}}MakePointerAvailable
//SPIRV-DAG: OpCooperativeVectorLoadNV{{.*}}MakePointerVisible

// CHECK: 9
// CHECK-NEXT: A
// CHECK-NEXT: B
// CHECK-NEXT: C
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4

typealias DeviceCoherentPtr<T> = CoherentPtr<T>;
typealias WorkgroupCoherentPtr<T> = Ptr<T, AddressSpace::UserPointer, Access::ReadWrite, CoherentScope::Workgroup>;

[shader("compute")]
[numthreads(1, 1, 1)]
void computeMain()
{
    DeviceCoherentPtr<int32_t> deviceCoherentInputBuffer = DeviceCoherentPtr<int32_t>(inputBuffer);
    DeviceCoherentPtr<int32_t> deviceCoherentOutputBuffer = DeviceCoherentPtr<int32_t>(outputBuffer);

    WorkgroupCoherentPtr<int32_t> workgroupCoherentInputBuffer = WorkgroupCoherentPtr<int32_t>(inputBuffer);
    WorkgroupCoherentPtr<int32_t> workgroupCoherentOutputBuffer = WorkgroupCoherentPtr<int32_t>(outputBuffer);


    // First half of input.
    let a = coopVecLoad<4, int32_t>(deviceCoherentInputBuffer, 0);
    // Second half of input.
    let b = coopVecLoad<4, int32_t>(workgroupCoherentInputBuffer + 4, 4*4);

    // Store second half of input to first half of output buffer.
    b.store(deviceCoherentOutputBuffer, 0);
    // Store first half of input to second half of output buffer.
    a.store(workgroupCoherentOutputBuffer, 4*4);
}

