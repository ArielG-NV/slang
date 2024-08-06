public struct TestNested
{
    private int val1;
    internal int val2;

    public int getVal1()
    {
        return val1;
    }
    public int getVal2()
    {
        return val1;
    }    
}

public struct Test
{
    private int val1;
    internal int val2;
    private TestNested nested1;

    public static Test getTest()
    {
        return { 1, 2, { 3 }};
    }
    public int getVal1()
    {
        return val1;
    }
    public int getVal2()
    {
        return val2;
    }
    public TestNested getNested1()
    {
        return nested1;
    }
};

public Test getTest()
{
    return { 1, { 2 } };
}