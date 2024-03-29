//TEST:SIMPLE(filecheck=CHECK): -target hlsl -stage raygeneration -entry main

#define TRACING_EPSILON 1e-6

RaytracingAccelerationStructure as;

RWStructuredBuffer<float> outputBuffer1;

void addComplexity1(inout float someInData1)
{
    someInData1 = outputBuffer1[1];
}

void addComplexity2(float dataIn)
{
    outputBuffer1[2] = dataIn;
}

void main()
{
    RayDesc ray;
    ray.Origin = float3(0);
    ray.TMin = TRACING_EPSILON;
    ray.Direction = float3(0,0,1);
    ray.TMax = 100.0f;

    // CHECK: ForceVarIntoStructTemporarily_t{{_[0-9]}} forceVarIntoStructTemporarily{{_[0-9]}};
    float someInData1 = 5.0f;
    addComplexity1(someInData1);

    // CHECK: forceVarIntoStructTemporarily{{_[0-9]}}.data{{_[0-9]}} = someInData
    // CHECK: TraceRay(
    // CHECK: someInData1{{.*}} = forceVarIntoStructTemporarily{{.*}}.data{{.*}};
    TraceRay(as,
             1,
             0xff,
             0,
             0,
             2,
             ray,
             someInData1);
    outputBuffer1[0] = outputBuffer1[0]+someInData1;

    // CHECK: forceVarIntoStructTemporarily{{_[0-9]}}.data{{_[0-9]}} = someInData
    // CHECK: TraceMotionRay(
    // CHECK: someInData1{{.*}} = forceVarIntoStructTemporarily{{.*}}.data{{.*}};
    TraceMotionRay(as,
            1,
            0xff,
            0,
            0,
            2,
            ray,
            0.0f,
            someInData1);
    outputBuffer1[0] = outputBuffer1[0]+someInData1;

    // CHECK: forceVarIntoStructTemporarily{{_[0-9]}}.data{{_[0-9]}} = someInData
    // CHECK: NvTraceRayHitObject(
    // CHECK: someInData1{{.*}} = forceVarIntoStructTemporarily{{.*}}.data{{.*}};
    HitObject::TraceRay(as,
             1,
             0xff,
             0,
             0,
             2,
             ray,
             someInData1);
    outputBuffer1[0] = outputBuffer1[0]+someInData1;

    // CHECK: forceVarIntoStructTemporarily{{_[0-9]}}.data{{_[0-9]}} = someInData
    // CHECK: TraceMotionRay(
    // CHECK: someInData1{{.*}} = forceVarIntoStructTemporarily{{.*}}.data{{.*}};
    HitObject::TraceMotionRay(as,
            1,
            0xff,
            0,
            0,
            2,
            ray,
            0.0f,
            someInData1);
    outputBuffer1[0] = outputBuffer1[0]+someInData1;

    // CHECK: forceVarIntoStructTemporarily{{_[0-9]}}.data{{_[0-9]}} = someInData
    // CHECK: NvInvokeHitObject(
    // CHECK: someInData1{{.*}} = forceVarIntoStructTemporarily{{.*}}.data{{.*}};
    HitObject hitObject_HitOrMiss; 
    HitObject::Invoke(
        as,
        hitObject_HitOrMiss,
        someInData1);
    outputBuffer1[0] = outputBuffer1[0]+someInData1;

    addComplexity2(someInData1);
}