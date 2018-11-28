module Pipeline.Utils where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except

import Lens.Micro.Mtl

import Pipeline.Definitions

import Grin.Grin
import Grin.EffectMap
import Grin.TypeEnvDefs
import AbstractInterpretation.CByResultTypes
import AbstractInterpretation.LVAResultTypes
import AbstractInterpretation.SharingResult

pipelineLog :: String -> PipelineM ()
pipelineLog str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStrLn str

pipelineLogNoLn :: String -> PipelineM ()
pipelineLogNoLn str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStr str

pipelineLogIterations :: Int -> PipelineM ()
pipelineLogIterations n = pipelineLogNoLn $ "iterations: " ++ show n

defaultOptimizations :: [Transformation]
defaultOptimizations =
  [ EvaluatedCaseElimination
  , TrivialCaseElimination
  , SparseCaseOptimisation
  , UpdateElimination
  , NonSharedElimination
  , CopyPropagation
  , ConstantPropagation
  , SimpleDeadFunctionElimination
  , SimpleDeadParameterElimination
  , SimpleDeadVariableElimination
  , DeadCodeElimination
  , CommonSubExpressionElimination
  , CaseCopyPropagation
  , CaseHoisting
  , GeneralizedUnboxing
  , ArityRaising
  , InlineEval
  , InlineApply
  , LateInlining
  ]

defaultOnChange :: [PipelineStep]
defaultOnChange =
  [ T ProducerNameIntroduction
  , T BindNormalisation
  , CBy CompileToAbstractProgram
  , CBy RunAbstractProgramPure
  , LVA CompileToAbstractProgram
  , LVA RunAbstractProgramPure
  , Sharing CompileToAbstractProgram
  , Sharing RunAbstractProgramPure
  , T UnitPropagation
  , Eff CalcEffectMap
  ]

-- Copy propagation, SDVE and bind normalisitaion
-- together can clean up all unnecessary artifacts
-- of producer name introduction.
defaultCleanUp :: [PipelineStep]
defaultCleanUp =
  [ T CopyPropagation
  , T SimpleDeadVariableElimination
  ]

debugPipeline :: [PipelineStep] -> [PipelineStep]
debugPipeline ps = [PrintGrin id] ++ ps ++ [PrintGrin id]

debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- get
  liftIO $ print ps

printingSteps :: [PipelineStep]
printingSteps =
  [ HPT PrintAbstractProgram
  , HPT PrintAbstractResult
  , CBy PrintAbstractProgram
  , CBy PrintAbstractResult
  , LVA PrintAbstractProgram
  , LVA PrintAbstractResult
  , Sharing PrintAbstractProgram
  , Sharing PrintAbstractResult
  , PrintTypeEnv
  , Eff PrintEffectMap
  , PrintAST
  , PrintErrors
  , PrintTypeAnnots
  , DebugPipelineState
  , PrintGrin id
  ]

isPrintingStep :: PipelineStep -> Bool
isPrintingStep = flip elem printingSteps

getExp :: ExceptT String PipelineM Exp
getExp = lift . use $ psExp

getTypeEnv :: ExceptT String PipelineM TypeEnv
getTypeEnv = do
  let err = "Type environment is not available"
  typeEnvM <- lift $ use psTypeEnv
  maybe (throwE err) return typeEnvM

getEffectMap :: ExceptT String PipelineM EffectMap
getEffectMap = do
  let err = "Effect map is not available"
  effMapM <- lift $ use psEffectMap
  maybe (throwE err) return effMapM

getCByResult :: ExceptT String PipelineM CByResult
getCByResult = do
  let err = "Created-by analysis result is not available"
  cbyResultM <- lift $ use psCByResult
  maybe (throwE err) return cbyResultM

getLVAResult :: ExceptT String PipelineM LVAResult
getLVAResult = do
  let err = "Live variable analysis result is not available"
  lvaResultM <- lift $ use psLVAResult
  maybe (throwE err) return lvaResultM

getSharingResult :: ExceptT String PipelineM SharingResult
getSharingResult = do
  let err = "Sharing analysis result is not available"
  shResultM <- lift $ use psSharingResult
  maybe (throwE err) return shResultM

runTransformation :: ExceptT String PipelineM Exp -> PipelineM ()
runTransformation trfM = do
  expE <- runExceptT trfM
  case expE of
    Right e' -> psExp .= e' >> psTransStep %= (+1)
    Left err -> pipelineLog $ err ++ ", skipping next step"

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT = ExceptT . pure
