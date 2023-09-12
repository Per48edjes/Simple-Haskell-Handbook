{-# LANGUAGE OverloadedRecordDot #-}

module Core where

import qualified Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

newtype Pipeline = Pipeline
    { steps :: NonEmpty Step
    }
    deriving (Show, Eq)

data Step = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Docker.Image
    }
    deriving (Show, Eq)

data StepResult = StepSucceeded | StepFailed
    deriving (Eq, Show)

data Build = Build
    { pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    }
    deriving (Show, Eq)

data BuildState = BuildReady | BuildRunning BuildRunningState | BuildFinished BuildResult
    deriving (Show, Eq)

data BuildRunningState = BuildRunningState {step :: StepName}
    deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed
    deriving (Show, Eq)

newtype StepName = StepName Text
    deriving (Show, Eq, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit
    | Docker.exitCodeToInt exit == 0 = StepSucceeded
    | otherwise = StepFailed

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
    if allSucceeded
        then case nextStep of
            Just step -> Right step
            Nothing -> Left BuildSucceeded
        else Left BuildFailed
  where
    allSucceeded = List.all (StepSucceeded ==) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps

progress :: Build -> IO Build
progress build =
    case build.state of
        BuildReady ->
            case buildHasNextStep build of
                Left result -> pure $ build{state = BuildFinished result}
                Right step -> do
                    let s = BuildRunningState{step = step.name}
                    pure $ build{state = BuildRunning s}
        BuildRunning state -> do
            let exit = Docker.ContainerExitCode 0
                result = exitCodeToStepResult exit
            pure build{state = BuildReady, completedSteps = Map.insert state.step result build.completedSteps}
        BuildFinished _ -> pure build
