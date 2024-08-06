//TEST(compute):COMPARE_COMPUTE_EX(filecheck-buffer=BUF): -vk -compute -shaderobj


public struct TestDerived1
{
    private int val1;
    public int val2;
}
public struct Test1 : TestDerived1
{
    private int val3;
    public int val4;
}

public struct TestDerived2
{
    private int val1;
    public int val2;

    __init(int val)
    {
        this.val2 = val + 5;
    }
}
public struct Test2 : TestDerived2
{
    private int val3;
    public int val4;
}

public struct TestDerived3
{
    private int val1;
    public int val2;

    __init(int val)
    {
        this.val2 = val + 5;
    }
}
public struct Test3 : TestDerived3
{
    private int val3;
    public int val4;
    __init(int val)
    {
        val4 = val + 5; 
    }
}

public struct TestDerived4
{
    private int val1;
    public int val2;

    __init(int val)
    {
        this.val2 = val + 5;
    }
}
public struct Test4 : TestDerived4
{
    private int val3;
    public int val4;
    __init(int val)
    {
        val2 = val + 5;
        val4 = val + 10;
    }
}

public struct TestDerived5
{
    private int val1;
    public int val2;
}
public struct Test5 : TestDerived5
{
    private int val3;
    public int val4;
    __init(int val1, int val2)
    {
        this.val2 = val1 + 5;
        this.val4 = val2 + 10;
    }
}

public struct TestDerived6
{
    private int val1;
    public int val2;
}
public struct Test6 : TestDerived6
{
    private int val3;
    public int val4 = 0;
    __init(int val1, int val2)
    {
        val4 = val1 + 10;
    }
}
//TEST_INPUT: ubuffer(data=[0], stride=4):out,name=outputBuffer
RWStructuredBuffer<uint> outputBuffer;

[numthreads(1, 1, 1)]
void computeMain(uint3 dispatchThreadID : SV_DispatchThreadID)
{
    Test1 val1 = { 1, 2 };
    Test2 val2 = { 1, 2 };

//// ERROR --> NOT ALLOWED init parent and derived for init list if parent conforms
    Test3 val3 = { 1, 2 };
//// ERROR --> NOT ALLOWED init parent and derived for init list if parent conforms
    Test4 val4 = { 1, 2 };

    Test5 val5 = { 1, 2 };
    Test6 val6 = { 1, 2 };
	outputBuffer[0] = (true
        && val1.val2 == 1
        && val1.val4 == 2
        
        && val2.val2 == 6
        && val2.val4 == 2

        && val3.val2 == 6
        && val3.val4 == 2
        
        && val5.val2 == 6
        && val2.val4 == 12
        
        && val5.val2 == 0
        && val2.val4 == 12
        
        ) ? 1 : 0;

// BUF: 1
}