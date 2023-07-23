module Core where

import RIO

newtype Pipeline = Pipeline
    { steps :: NonEmpty Step
    }
    deriving (Show, Eq)

data Step = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Image
    }
    deriving (Show, Eq)

data Build = Build
    { pipeline :: Pipeline
    , state :: BuildState
    }
    deriving (Show, Eq)

data BuildState = BuildReady | BuildRunning | BuildFinished BuildResult
    deriving (Show, Eq)

data BuildResult = BuildSuccess | BuildFailure
    deriving (Show, Eq)

newtype StepName = StepName Text
    deriving (Show, Eq)

newtype Image = Image Text
    deriving (Show, Eq)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image
