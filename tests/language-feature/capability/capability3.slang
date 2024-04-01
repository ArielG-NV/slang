//TEST:SIMPLE(filecheck=CHECK): -target spirv -emit-spirv-directly -entry main -stage compute

// Test that capabilities can be declared on module.
[require(glsl)]
[require(spirv)]
module test;

RWStructuredBuffer<int> sideEffect;

// CHECK: error 36104
[require(spirv, hlsl)]
public void use1()
{
    __target_switch
    {
    case spirv:
        sideEffect[0] = 2;
        return;
    }
}

[require(glsl)]
void use2Sub()
{
    sideEffect[1] = 1;
}
// CHECK: error 36108
[require(spirv)]
public void use2()
{
    use2Sub();   
}

void main()
{
    use1();
    use2();
}