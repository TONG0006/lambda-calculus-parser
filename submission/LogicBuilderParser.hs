{-# OPTIONS_GHC -Wno-typed-holes #-}

module LogicBuilderParser where
import           AdditionalParser (token)
import           Parser
import           Prelude          hiding (fail)

trueToken :: Parser String
trueToken = token (string "True")

falseToken :: Parser String
falseToken = token (string "False")
