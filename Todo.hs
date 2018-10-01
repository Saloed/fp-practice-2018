module Todo(todo) where

import Control.Exception

data NotImplementedException = NotImplementedException

instance Exception NotImplementedException where

instance Show NotImplementedException where
    show _ = "Not implemented yet"

todo :: a
todo = throw NotImplementedException

data TodoType = TodoType
