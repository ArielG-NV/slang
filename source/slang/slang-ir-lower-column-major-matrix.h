#pragma once

#include "slang-ir.h"
#include "slang-compiler.h"

namespace Slang
{
    class DiagnosticSink;

    void lowerColumnMajorMatrix(IRModule* module, DiagnosticSink* sink);
}
