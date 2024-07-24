#include "slang-ir-redundancy-removal.h"
#include "slang-ir-dominators.h"
#include "slang-ir-call-graph.h"
#include "slang-ir-util.h"

namespace Slang
{

struct RedundancyRemovalContext
{
    RefPtr<IRDominatorTree> dom;

    bool tryHoistInstToOuterMostLoop(IRGlobalValueWithCode* func, IRInst* inst)
    {
        bool changed = false;
        for (auto parentBlock = dom->getImmediateDominator(as<IRBlock>(inst->getParent()));
             parentBlock;
             parentBlock = dom->getImmediateDominator(parentBlock))
        {
            auto terminatorInst = parentBlock->getTerminator();
            if (terminatorInst->getOp() == kIROp_loop)
            {
                // Consider hoisting the inst into this block.
                // This is only possible if all operands of the inst are dominating `parentBlock`.
                bool canHoist = true;
                for (UInt i = 0; i < inst->getOperandCount(); i++)
                {
                    auto operand = inst->getOperand(i);
                    if (!hasDescendent(func, operand))
                    {
                        // Only prevent hoisting from operands local to this function
                        continue;
                    }
                    auto operandParent = as<IRBlock>(operand->getParent());
                    if (!operandParent)
                    {
                        canHoist = false;
                        break;
                    }
                    canHoist = dom->dominates(operandParent, parentBlock);
                    if (!canHoist)
                        break;
                }
                if (!canHoist)
                    break;

                // Move inst to parentBlock.
                inst->insertBefore(terminatorInst);
                changed = true;

                // Continue to consider outer hoisting positions.
            }
        }
        return changed;
    }

    bool removeRedundancyInBlock(Dictionary<IRBlock*, DeduplicateContext>& mapBlockToDedupContext, IRGlobalValueWithCode* func, IRBlock* block)
    {
        bool result = false;
        auto& deduplicateContext = mapBlockToDedupContext.getValue(block);
        for (auto instP : block->getModifiableChildren())
        {
            auto resultInst = deduplicateContext.deduplicate(instP, [&](IRInst* inst)
                {
                    auto parentBlock = as<IRBlock>(inst->getParent());
                    if (!parentBlock)
                        return false;
                    if (dom->isUnreachable(parentBlock))
                        return false;
                    return isMovableInst(inst);
                });
            if (resultInst != instP)
            {
                instP->replaceUsesWith(resultInst);
                instP->removeAndDeallocate();
                result = true;
            }
            else if (isMovableInst(resultInst))
            {
                // This inst is unique, we should consider hoisting it
                // if it is inside a loop.
                result |= tryHoistInstToOuterMostLoop(func, resultInst);
            }
        }
        for (auto child : dom->getImmediatelyDominatedBlocks(block))
        {
            DeduplicateContext& subContext = mapBlockToDedupContext.getValue(child);
            subContext.deduplicateMap = deduplicateContext.deduplicateMap;
        }
        return result;
    }
};

bool removeRedundancy(IRModule* module)
{
    bool changed = false;
    for (auto inst : module->getGlobalInsts())
    {
        if (auto genericInst = as<IRGeneric>(inst))
        {
            removeRedundancyInFunc(genericInst);
            inst = findGenericReturnVal(genericInst);
        }
        if (auto func = as<IRFunc>(inst))
        {
            changed |= removeRedundancyInFunc(func);
        }
    }
    return changed;
}

bool removeRedundancyInFunc(IRGlobalValueWithCode* func)
{
    auto root = func->getFirstBlock();
    if (!root)
        return false;

    RedundancyRemovalContext context;
    context.dom = computeDominatorTree(func);
    Dictionary<IRBlock*, DeduplicateContext> mapBlockToDeduplicateContext;
    for (auto block : func->getBlocks())
    {
        mapBlockToDeduplicateContext[block] = DeduplicateContext();
    }
    List<IRBlock*> workList, pendingWorkList;
    workList.add(root);
    bool result = false;
    while (workList.getCount())
    {
        for (auto block : workList)
        {
            result |= context.removeRedundancyInBlock(mapBlockToDeduplicateContext, func, block);
            
            for (auto child : context.dom->getImmediatelyDominatedBlocks(block))
            {
                pendingWorkList.add(child);
            }
        }
        workList.swapWith(pendingWorkList);
        pendingWorkList.clear();
    }
    if (auto normalFunc = as<IRFunc>(func))
    {
        result |= eliminateRedundantLoadStore(normalFunc);
    }
    return result;
}

static bool  _isIndirectRootVar(IRInst* inst)
{
    switch (inst->getOp())
    {
    case kIROp_FieldAddress:
    case kIROp_GetElementPtr:
        return true;
    default:
        return false;
    }
}
static IRInst* _getRootVar(IRInst* inst)
{
    while (inst)
    {
        if (_isIndirectRootVar(inst))
            inst = inst->getOperand(0);
        else
            return inst;
    }
    return inst;
}

const HashSet<IRFunc*>& getReferencedEntryPointsOfInst(IRInst* inst, IRModule* module)
{
    // Op in global scope is in all entry-points
    auto parentFunc = getParentFunc(inst);
    if (!parentFunc)
        return module->getEntryPoints();
    else
    {
        // Compiler introduced (specialized?) function, may need to rebuild ref-graph
        auto referencedEntryPoints = module->getMaximalEntryPointReferenceGraph().tryGetValue(parentFunc);
        if (!referencedEntryPoints)
            return {};
        return *referencedEntryPoints;
    }
}

bool tryRemoveRedundantStoreIntoGlobal(IRGlobalValueWithCode* func, IRStore* store)
{
    // A 'storeInst' has 2 parameters, 'src' and 'dst'. If 'src' is sideeffect-free
    // then to determine if 'storeInst' is live we need to analyze the 'dst' we are storing into.
    // If the 'dst' is 'unused' we can then drop this 'storeInst'.
    // 'dst' is 'unused' if: 1. 'dst' is not global memory 2. we only use 'dst' for Store's within the same referenced entry-points as 'storeInst'.
    // This logic needs to watchout for a call-graph that may call a function which modifies a global across 2+ entry-points.
    //
    // As a result of these rules, we only optimize 'main1'.
    /*
    static int globalVar;

    void main1()
    {
        globalVar = 2;
    }

    void main2()
    {
        globalVar = 1;
        int var = globalVar;
    }
    */
    auto storeInstModule = store->getModule();
    if (!storeInstModule)
        return false;

    // 'src' cannot have a side effect if Store is to be removed.
    if (store->getVal()->mightHaveSideEffects())
        return false;

    // 'dst' does not impact device memory
    auto dstInst = _getRootVar(store->getPtr());
    if (instAffectsDeviceMemory(dstInst))
        return false;

    auto storeInstReferencedEntryPoints = getReferencedEntryPointsOfInst(store, storeInstModule);
    auto moduleEntryPointRefGraph = storeInstModule->getMaximalEntryPointReferenceGraph();
    auto moduleEntryPoints = storeInstModule->getEntryPoints();

    List<IRUse*> workList;
    for (auto use = dstInst->firstUse; use; use = use->nextUse)
    {
        auto user = use->getUser();
        //TODO: get indirect root var uses
        if (_isIndirectRootVar(user))
        {
            for (auto useOfRoot = user->firstUse; useOfRoot; useOfRoot = useOfRoot->nextUse)
                workList.add(useOfRoot);
        }
        else
            workList.add(use);
    }
    for(auto use : workList)
    {
        auto user = use->getUser();

        // Trivial case.
        if (user == store)
            continue;

        // DebugInst mean we are in a debug-mode of sorts, end here.
        if (isDebugInst(user))
            return false;

        // Inst is a store where 'dst' is the destination, end here.
        if (isInstTheDstOfStore(use->usedValue, user))
            continue;

        auto refGraphOfInst = getReferencedEntryPointsOfInst(user, storeInstModule);
        // 0 entry point references, 'dst' is unused.
        if (refGraphOfInst.getCount() == 0)
            continue;

        for (auto entryPoint : storeInstReferencedEntryPoints)
            if (refGraphOfInst.contains(entryPoint))
                // found shared entry point which was not a Store, we can end here.
                return false;
    }

    store->removeAndDeallocate();
    return true;
}

bool tryRemoveRedundantStore(IRGlobalValueWithCode* func, IRStore* store)
{
    // If 'src' has a side effect, do not optimize out
    if (store->getVal()->mightHaveSideEffects())
        return false;

    // Stores to global variables can be removed in rare cases, these are checked for.
    auto rootVar = _getRootVar(store->getPtr());
    if (!isChildInstOf(rootVar, func))
    {
        return tryRemoveRedundantStoreIntoGlobal(func, store);
    }

    // We perform a quick and conservative check:
    // A store is redundant if it is followed by another store to the same address in
    // the same basic block, and there are no instructions that may use any addresses
    // related to this address.
    bool hasAddrUse = false;
    bool hasOverridingStore = false;

    // A store can be removed if it stores into a local variable
    // that has no other uses than store.
    if (const auto varInst = as<IRVar>(rootVar))
    {
        bool hasNonStoreUse = false;
        // If the entire access chain doesn't non-store use, we can safely remove it.
        InstHashSet knownAccessChain(func->getModule());
        for (auto accessChain = store->getPtr(); accessChain;)
        {
            knownAccessChain.add(accessChain);
            for (auto use = accessChain->firstUse; use; use = use->nextUse)
            {
                if (as<IRDecoration>(use->getUser()))
                    continue;
                if (knownAccessChain.contains(use->getUser()))
                    continue;
                if (use->getUser()->getOp() == kIROp_Store && 
                    use == use->getUser()->getOperands())
                {
                    continue;
                }
                hasNonStoreUse = true;
                break;
            }
            if (hasNonStoreUse)
                break;
            switch (accessChain->getOp())
            {
            case kIROp_GetElementPtr:
            case kIROp_FieldAddress:
                accessChain = accessChain->getOperand(0);
                continue;
            default:
                break;
            }
            break;
        }
        if (!hasNonStoreUse)
        {
            store->removeAndDeallocate();
            return true;
        }
    }
    // A store can be removed if there are subsequent stores to the same variable,
    // and there are no insts in between the stores that can read the variable.

    HashSet<IRBlock*> visitedBlocks;
    for (auto next = store->getNextInst(); next;)
    {
        if (auto nextStore = as<IRStore>(next))
        {
            if (nextStore->getPtr() == store->getPtr())
            {
                hasOverridingStore = true;
                break;
            }
        }

        // If we see any insts that have reads or modifies the address before seeing
        // an overriding store, don't remove the store.
        // We can make the test more accurate by collecting all addresses related to
        // the target address first, and only bail out if any of the related addresses
        // are involved.
        switch (next->getOp())
        {
        case kIROp_Load:
            if (canAddressesPotentiallyAlias(func, next->getOperand(0), store->getPtr()))
            {
                hasAddrUse = true;
            }
            break;
        default:
            if (canInstHaveSideEffectAtAddress(func, next, store->getPtr()))
            {
                hasAddrUse = true;
            }
            break;
        }
        if (hasAddrUse)
            break;

        // If we are at the end of the current block and see a unconditional branch,
        // we can follow the path and check the subsequent block.
        if (auto branch = as<IRUnconditionalBranch>(next))
        {
            auto nextBlock = branch->getTargetBlock();
            if (visitedBlocks.add(nextBlock))
            {
                next = nextBlock->getFirstInst();
                continue;
            }
        }
        next = next->getNextInst();
    }

    if (!hasAddrUse && hasOverridingStore)
    {
        store->removeAndDeallocate();
        return true;
    }

    // A store can be removed if it is a store into the same var, and there are
    // no side effects between the load of the var and the store of the var.
    if (auto load = as<IRLoad>(store->getVal()))
    {
        if (load->getPtr() == store->getPtr())
        {
            if (load->getParent() == store->getParent())
            {
                bool valueMayChange = false;
                for (auto inst = load->next; inst; inst = inst->next)
                {
                    if (inst == store)
                        break;
                    if (canInstHaveSideEffectAtAddress(func, inst, store->getPtr()))
                    {
                        valueMayChange = true;
                        break;
                    }
                }
                if (!valueMayChange)
                {
                    store->removeAndDeallocate();
                    return true;
                }
            }
        }
    }
    return false;
}

bool eliminateRedundantLoadStore(IRGlobalValueWithCode* func)
{
    bool changed = false;
    for (auto block : func->getBlocks())
    {
        for (auto inst = block->getFirstInst(); inst;)
        {
            auto nextInst = inst->getNextInst();
            if (auto load = as<IRLoad>(inst))
            {
                for (auto prev = inst->getPrevInst(); prev; prev = prev->getPrevInst())
                {
                    if (auto store = as<IRStore>(prev))
                    {
                        if (store->getPtr() == load->getPtr())
                        {
                            // If the load is preceeded by a store without any side-effect insts in-between, remove the load.
                            auto value = store->getVal();
                            load->replaceUsesWith(value);
                            load->removeAndDeallocate();
                            changed = true;
                            break;
                        }
                    }

                    if (canInstHaveSideEffectAtAddress(func, prev, load->getPtr()))
                    {
                        break;
                    }
                }
            }
            else if (auto store = as<IRStore>(inst))
            {
                changed |= tryRemoveRedundantStore(func, store);
            }
            inst = nextInst;
        }
    }
    return changed;
}

}
