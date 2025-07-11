//TEST:SIMPLE(filecheck=CHECK_DEVICE):-stage compute -entry computeMain -target spirv -DDEVICE
//TEST:SIMPLE(filecheck=CHECK_WORKGROUP):-stage compute -entry computeMain -target spirv -DWORKGROUP

// CHECK_DEVICE Tests that if we do not declare a coherentScope pointer we do not enable `OpCapability VulkanMemoryModelDeviceScope`
// CHECK_WORKGROUP Tests that if we declare a coherentScope pointer we enable `OpCapability VulkanMemoryModelDeviceScope`

// CHECK_DEVICE: OpCapability VulkanMemoryModelDeviceScope
// CHECK_WORKGROUP-NOT: OpCapability VulkanMemoryModelDeviceScope

RWStructuredBuffer<int> processMemory;
RWStructuredBuffer<int> output;
// Type aliases for testing
#ifdef DEVICE
typedef Ptr<int, AddressSpace::UserPointer, Access::ReadWrite, CoherentScope::Device> TestPtr;
#endif
#ifdef WORKGROUP
typedef Ptr<int, AddressSpace::UserPointer, Access::ReadWrite, CoherentScope::Workgroup> TestPtr;
#endif

[numthreads(1, 1, 1)]
void computeMain(int id : SV_DispatchThreadID)
{
    TestPtr ptr = TestPtr(&processMemory[id.x]);
    ptr[0] = 1;
    output[id] = *ptr + ptr[5];
}
