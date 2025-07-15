//TEST:SIMPLE(filecheck=CHECK):-stage compute -entry computeMain -target spirv

// Tests pointer casting rules: Only explicit casting is allowed between pointer types.
// All implicit conversions between pointer types should fail.
RWStructuredBuffer<int> processMemory;
RWStructuredBuffer<int> output;

[numthreads(1, 1, 1)]
void computeMain(int id : SV_DispatchThreadID)
{
    // Any non-equal pointers being copied must be explicitly casted.
    // Cannot cast between different address spaces.

    // CHECK-NOT: error
    Ptr<int, Access::ReadWrite> rwPtr = &processMemory[id.x];
    Ptr<int, Access::Read> rPtr = Ptr<int, Access::Read>(&processMemory[id.x]);
    Ptr<int, Access::ReadWrite, AddressSpace::UserPointer> p1 = Ptr<int, Access::ReadWrite, AddressSpace::UserPointer>(rPtr);
    Ptr<int, Access::Read, AddressSpace::UserPointer> p2 = Ptr<int, Access::Read, AddressSpace::UserPointer>(rwPtr);
    Ptr<float, Access::ReadWrite, AddressSpace::UserPointer> p3 = Ptr<float, Access::ReadWrite, AddressSpace::UserPointer>(rPtr);

    // CHECK: ([[# @LINE+1]]): error 30019
    Ptr<float, Access::ReadWrite, AddressSpace::UserPointer> p3 = rPtr;
    // CHECK: ([[# @LINE+1]]): error 30019
    Ptr<float, Access::ReadWrite, AddressSpace::GroupShared> p4 = Ptr<float, Access::Read, AddressSpace::GroupShared> (rwPtr);
    // CHECK: ([[# @LINE+1]]): error 30019
    Ptr<int, Access::Read> p1 = Ptr<int, Access::ReadWrite>(&processMemory[id.x]);
    // CHECK: ([[# @LINE+1]]): error 30019
    Ptr<int, Access::ReadWrite> p2 = Ptr<int, Access::Read>(&processMemory[id.x]);

    // CHECK-NOT: error
    output[id] = *readPtr + readPtr[5];
}
