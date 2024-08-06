//TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF): -vk -compute -shaderobj

import initializer_list_constructor_private_internal_helper;

//TEST_INPUT: ubuffer(data=[0], stride=4):out,name=outputBuffer
RWStructuredBuffer<uint> outputBuffer;

[numthreads(1, 1, 1)]
void computeMain(uint3 dispatchThreadID : SV_DispatchThreadID)
{
    Test val1 = getTest();
    Test val2 = Test::getTest();
    outputBuffer[0] = (true
        && val1.getVal2() == 1
        && val1.getNested1().getVal2() == 2

        && val2.getVal1() == 1
        && val2.getVal2() == 2
        && val2.getNested1().getVal2() == 3
        ) ? 1 : 0;

// BUF: 1
}