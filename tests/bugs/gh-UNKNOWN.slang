struct Data
{
    int inner1;
};

static Data needToRemoveThisStatic;

ByteAddressBuffer needToRemoveThisBuffer;

RWStructuredBuffer<uint> outputBuffer;

struct returnedNestedMagic
{
    uint inner1;
    uint inner2[3];
}
StructuredBuffer<returnedNestedMagic> magicLoad;
struct returnedMagic
{
    uint inner1;
    float inner2;
    uint inner3;
}

uint someNestedNestedMagic(returnedNestedMagic nestedMagic)
{
    int val1 = (nestedMagic.inner2[0] & 1) / 255;
    int2 val2 = (nestedMagic.inner2[0] & 1);
    return val2.x + val1;
}
returnedMagic someNestedMagic(returnedNestedMagic nestedMagic)
{
    returnedMagic magic;
    magic.inner1 = nestedMagic.inner1;
    magic.inner2 = f16tof32(nestedMagic.inner1 & 0xffff);
    magic.inner3 = someNestedNestedMagic(nestedMagic);
    return magic;
}
returnedMagic someMagic(uint inData)
{
    return  someNestedMagic(magicLoad[inData]);
}

struct Payload_t
{
    int payloadData1;
}

[shader("anyhit")]
void main1(inout Payload_t payload , in BuiltInTriangleIntersectionAttributes attrs)
{
    needToRemoveThisStatic = needToRemoveThisBuffer.Load<Data>(someMagic(InstanceIndex()).inner1);
    outputBuffer[0] = 1;

    if (payload.payloadData1 == 2)
    {
        AcceptHitAndEndSearch();
    }
    else
    {
        IgnoreHit();
    }
}

[shader("anyhit")]
void main2(inout Payload_t payload , in BuiltInTriangleIntersectionAttributes attrs)
{
    needToRemoveThisStatic = needToRemoveThisBuffer.Load<Data>(someMagic(InstanceIndex()).inner1);
    outputBuffer[1] = 1;

    if (payload.payloadData1 == 1)
    {
        AcceptHitAndEndSearch();
    }
    else
    {
        IgnoreHit();
    }
}