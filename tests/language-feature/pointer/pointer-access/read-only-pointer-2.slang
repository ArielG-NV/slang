//TEST:SIMPLE(filecheck=CHECK):-stage compute -entry computeMain -target spirv

// Tests valid use of read-only pointer

// CHECK: OpEntryPoint
// CHECK-NOT: error

RWStructuredBuffer<int> processMemory;
RWStructuredBuffer<int> output;

__generic<AddressSpace addrSpace>
void writeToReadOnlyPointer(Ptr<int, Access::Read, addrSpace> ptr)
{
    ptr[0] = 1;
}

[numthreads(1, 1, 1)]
void computeMain(int id : SV_DispatchThreadID)
{
    Ptr<int, Access::Read, AddressSpace::UserPointer> ptr1 = &processMemory[id.x];

    output[id] = ptr1[id];
}
