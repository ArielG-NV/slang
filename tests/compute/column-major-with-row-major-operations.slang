// column-major-with-row-major-operations.slang

// Unfortunately CPU and CUDA only work with row layout, so they have to be disabled here.

//TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF):-cpu -compute -xslang -g0
//TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF):-cuda -compute -xslang -g0
//TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF):-mtl -compute -xslang -g0

// Following tests do not work because the backends do not handle row_major/column_major correctly.
//DISABLE_TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF):-slang -compute -dx12
//DISABLE_TEST(compute, vulkan):COMPARE_COMPUTE_EX(filecheck-buffer=BUF):-vk -compute

// This data is in column major layout order.... 
//TEST_INPUT:cbuffer(data=[1.0 0.0 10.0  0.0 1.0 20.0  0.0 0.0 1.0]):name colMatrixBuffer
ConstantBuffer<column_major float3x3> colMatrixBuffer;

//TEST_INPUT:cbuffer(data=[1.0 0.0 0.0  0.0 1.0 0.0  10.0 20.0 1.0]):name rowMatrixBuffer
ConstantBuffer<row_major float3x3> rowMatrixBuffer;

//TEST_INPUT:ubuffer(data=[0 0 0 0 0 0 0 0 0 999999 0 0 0  0 0 0  0 0 0 999999], stride=4):out,name output
RWStructuredBuffer<uint> output;

bool floatCheck(float data, float valueToCheckFor)
{
    return data < (valueToCheckFor + 0.001) && data > valueToCheckFor - 0.001;
}

[numthreads(1, 1, 1)]
void computeMain(uint3 tid : SV_DispatchThreadID)
{
    float4 v = float4(1, 2, 3, 1);

    // test multiplication

    row_major float3x3 MR1;
    MR1 = rowMatrixBuffer;
    
    row_major float3x3 MR2;
    MR2 = float3x3(
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            10.0, 20.0, 1.0,
        );

    column_major float3x3 MC1;
    MC1 = colMatrixBuffer;

    column_major float3x3 MC2;
    MC2 = float3x3(
            1.0, 0.0, 10.0, 
            0.0, 1.0, 20.0,
            0.0, 0.0, 1.0,
        );

    float3x3 MResult1 = mul(MC1, MR1);
    float3x3 MResult2 = mul(MR1, MC1);
    float3x3 MResult3 = mul(MC2, MR2);

    // test constructors and copy constructors
    column_major float3x3 MC3;
    MC3 = float3x3(
            1.0, 4.0, 7.0,
            2.0, 5.0, 8.0,
            3.0, 6.0, 9.0,
        );

    column_major float3x3 MC4;
    MC4 = float3x3(
            float4x4(
                1.0, 4.0, 7.0, 106.0,
                2.0, 5.0, 8.0, 105.0,
                3.0, 6.0, 9.0, 104.0,
                100.0, 101.0, 102.0, 103.0,
            )
        );

    column_major float3x3 MC5;
    MC5 = float3x3(
            float3(1.0, 4.0, 7.0),
            float3(2.0, 5.0, 8.0),
            float3(3.0, 6.0, 9.0),
        );

    row_major float3x3 MResult4;
    MResult4 = mul(MC3, MR2);

    row_major float3x3 MResult5;
    MResult5 = mul(MC4, MR2);

    row_major float3x3 MResult6;
    MResult6 = mul(MC5, MR2);

    bool res = (true
            && floatCheck(MResult1[2][0], 10)
            && floatCheck(MResult1[0][2], 10)
        
            && floatCheck(MResult2[2][0], 10)
            && floatCheck(MResult2[0][2], 10)

            && floatCheck(MResult3[2][0], 10)
            && floatCheck(MResult3[0][2], 10)


            && floatCheck(MC3[0].x, 1)
            && floatCheck(MC3[0].y, 2)
            && floatCheck(MC3[0].z, 3)
            && floatCheck(MC3[0][0], 1)
            && floatCheck(MC3[0][1], 2)
            && floatCheck(MC3[0][2], 3)
            
            && floatCheck(MC4[0].x, 1)
            && floatCheck(MC4[0].y, 2)
            && floatCheck(MC4[0].z, 3)
            && floatCheck(MC4[0][0], 1)
            && floatCheck(MC4[0][1], 2)
            && floatCheck(MC4[0][2], 3)

            && floatCheck(MC5[0].x, 1)
            && floatCheck(MC5[0].y, 2)
            && floatCheck(MC5[0].z, 3)
            && floatCheck(MC5[0][0], 1)
            && floatCheck(MC5[0][1], 2)
            && floatCheck(MC5[0][2], 3)


            && floatCheck(MResult4[2].x, 93)
            && floatCheck(MResult4[2].y, 186)
            && floatCheck(MResult4[2].z, 9)
            && floatCheck(MResult4[2][0], 93)
            && floatCheck(MResult4[2][1], 186)
            && floatCheck(MResult4[2][2], 9)
            
            && floatCheck(MResult5[2].x, 93)
            && floatCheck(MResult5[2].y, 186)
            && floatCheck(MResult5[2].z, 9)
            && floatCheck(MResult5[2][0], 93)
            && floatCheck(MResult5[2][1], 186)
            && floatCheck(MResult5[2][2], 9)

            && floatCheck(MResult6[2].x, 93)
            && floatCheck(MResult6[2].y, 186)
            && floatCheck(MResult6[2].z, 9)
            && floatCheck(MResult6[2][0], 93)
            && floatCheck(MResult6[2][1], 186)
            && floatCheck(MResult6[2][2], 9)
        );
    if (res)
        output[0] = 100;
    else 
        output[0] = 0;

    output[1] = (uint)MResult1[0][2]; // 2
    output[2] = (uint)MResult2[0][2]; // 3
    output[3] = (uint)MResult3[0][2]; // 4
    output[4] = (uint)MResult4[0][2]; // 5
    output[5] = (uint)MResult5[0][2]; // 6
    output[6] = (uint)MResult6[0][2]; // 7

    for (int x = 0; x < 3; x++)
    {
        output[10 + x * 3 + 0] = (uint)MC3[x][0];
        output[10 + x * 3 + 1] = (uint)MC3[x][1];
        output[10 + x * 3 + 2] = (uint)MC3[x][2];
    }
    //BUF: 64
}
