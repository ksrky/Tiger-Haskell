module Temp where

import Control.Monad.State
import Data.IORef

import Symbol

type Temp = Int
type Label = Symbol

data TempState = TS {temps :: IORef Temp, labs :: IORef Int}

initState :: MonadIO m => m TempState
initState = do
        ref_temp <- liftIO $ newIORef 100
        ref_lab <- liftIO $ newIORef 0
        return TS{temps = ref_temp, labs = ref_lab}

newTemp :: MonadIO m => TempState -> m Temp
newTemp st = do
        let ref = temps st
        temp <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (temp + 1)
        return temp

makeString :: Int -> String
makeString t = "t" ++ show t

newLabel :: MonadIO m => TempState -> m Label
newLabel st = do
        let ref = temps st
        lab <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (lab + 1)
        return ("L" ++ show lab)

namedLabel :: String -> Label
namedLabel = symbol