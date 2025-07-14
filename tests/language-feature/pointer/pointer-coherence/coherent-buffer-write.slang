//TEST:SIMPLE(filecheck=SPIRV):-stage compute -entry computeMain -target spirv
//TEST(compute):COMPARE_COMPUTE(filecheck-buffer=CHECK):-vk -output-using-type

// Write coherently to a buffer
// SPIRV-COUNT-4: OpStore{{.*}}MakePointerAvailable
// SPIRV-NOT: OpLoad{{.*}}MakePointerVisible

//TEST_INPUT:ubuffer(data=[0 0 0 0 0], stride=4):name=buffer1
RWStructuredBuffer<int> buffer1;
//TEST_INPUT:ubuffer(data=[0 0 0 0 0], stride=4):name=buffer2
uniform int* buffer2;

// TEST_INPUT:ubuffer(data=[0 0 0 0 0], stride=4):out,name=output
RWStructuredBuffer<int> output;

typealias DeviceCoherentPtr<T> = Ptr<T, AddressSpace::UserPointer, Access::ReadWrite, CoherentScope::Device>;
typealias WorkgroupCoherentPtr<T> = Ptr<T, AddressSpace::UserPointer, Access::ReadWrite, CoherentScope::Workgroup>;

[numthreads(1, 1, 1)]
void computeMain(int id: SV_DispatchThreadID)
{
    DeviceCoherentPtr<int> buffer1PtrDevice = DeviceCoherentPtr<int>(&buffer1[0]);
    WorkgroupCoherentPtr<int> buffer1PtrWorkgroup = WorkgroupCoherentPtr<int>(&buffer1[0]);

    DeviceCoherentPtr<int> buffer2PtrDevice = DeviceCoherentPtr<int>(buffer2);
    WorkgroupCoherentPtr<int> buffer2PtrWorkgroup = WorkgroupCoherentPtr<int>(buffer2);

    *buffer1PtrDevice = 1;
    buffer1PtrWorkgroup[1] = 2;

    buffer2PtrDevice[0] = 3;
    buffer2PtrWorkgroup[1] = 4;

    // CHECK: 1
    // CHECK-NEXT: 2
    // CHECK-NEXT: 3
    // CHECK-NEXT: 4
    output[0] = buffer1[0];
    output[0 + 1] = buffer1[1];
    output[0 + 2] = buffer2[0];
    output[0 + 3] = buffer2[1];
}