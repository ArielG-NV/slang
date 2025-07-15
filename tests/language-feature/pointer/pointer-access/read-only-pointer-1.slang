//TEST:SIMPLE(filecheck=CHECK):-stage compute -entry computeMain -target spirv

// Writing with a read-only pointer should be an error

RWStructuredBuffer<int> processMemory;
RWStructuredBuffer<int> output;

__generic<AddressSpace addrSpace>
void writeToReadOnlyPointer(Ptr<int, Access::Read, addrSpace> ptr)
{
    // CHECK: ([[# @LINE+1]]): error 41404
    ptr[0] = 1;
}

[numthreads(1, 1, 1)]
void computeMain(int id : SV_DispatchThreadID)
{
    Ptr<int, Access::Read, AddressSpace::UserPointer> ptr1 = &processMemory[id.x];

    // CHECK: ([[# @LINE+1]]): error 41404
    ptr1[id + 1] = 1;
    *ptr1 = 1;

    // CHECK: ([[# @LINE+1]]): error 41404

    writeToReadOnlyPointer(ptr1);
    output[id] = ptr1[id];
}
