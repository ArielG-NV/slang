//TEST(compute):COMPARE_COMPUTE_EX:-cpu -compute -shaderobj -output-using-type
//TEST(compute):COMPARE_COMPUTE_EX:-slang -dx12 -compute -shaderobj -output-using-type
//TEST(compute, vulkan):COMPARE_COMPUTE_EX:-vk -compute -shaderobj -output-using-type

//TEST_INPUT:ubuffer(data=[0 0 0 0], stride=4):out,name=outputBuffer
RWStructuredBuffer<float> outputBuffer;

[ForwardDifferentiable]
vector<float, 3> g(float3x3 x, float3x3 y)
{
    return mul(2, mul(x, y));
}

[numthreads(1, 1, 1)]
void computeMain(uint3 dispatchThreadID : SV_DispatchThreadID)
{
    float3x3 a = float3x3(2.0);
    float3x3 b = float3x3(1.5);
    float3x3 da = float3x3(1.0);

    outputBuffer[0] = __fwd_diff(g)(
                          diffPair(a, da),
                          diffPair(b, da)).d.x; // Expect: 8
}
