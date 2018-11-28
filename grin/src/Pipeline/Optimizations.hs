module Pipeline.Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseOptimisationM
  , updateElimination
  , copyPropagation
  , constantPropagation
  , deadDataEliminationM
  , deadFunctionEliminationM
  , deadParameterEliminationM
  , deadVariableEliminationM
  , simpleDeadFunctionElimination
  , simpleDeadVariableElimination
  , simpleDeadParameterElimination
  , commonSubExpressionElimination
  , caseCopyPropagation
  , generalizedUnboxing
  , arityRaising
  , caseHoisting
  , lateInlining
  , nonSharedEliminationM
  ) where

import Transformations.Optimising.ConstantFolding (constantFolding)
import Transformations.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.Optimising.SparseCaseOptimisation (sparseCaseOptimisationM)
import Transformations.Optimising.UpdateElimination (updateElimination)
import Transformations.Optimising.CopyPropagation (copyPropagation)
import Transformations.Optimising.ConstantPropagation (constantPropagation)
import Transformations.Optimising.DeadDataElimination (deadDataEliminationM)
import Transformations.Optimising.DeadFunctionElimination (deadFunctionEliminationM)
import Transformations.Optimising.DeadParameterElimination (deadParameterEliminationM)
import Transformations.Optimising.DeadVariableElimination (deadVariableEliminationM)
import Transformations.Optimising.SimpleDeadFunctionElimination (simpleDeadFunctionElimination)
import Transformations.Optimising.SimpleDeadVariableElimination (simpleDeadVariableElimination)
import Transformations.Optimising.SimpleDeadParameterElimination (simpleDeadParameterElimination)
import Transformations.Optimising.CSE (commonSubExpressionElimination)
import Transformations.Optimising.CaseCopyPropagation (caseCopyPropagation)
import Transformations.Optimising.GeneralizedUnboxing (generalizedUnboxing)
import Transformations.Optimising.ArityRaisingSimple (arityRaising)
import Transformations.Optimising.CaseHoisting (caseHoisting)
import Transformations.Optimising.Inlining (lateInlining)
import Transformations.Optimising.NonSharedElimination (nonSharedEliminationM)
