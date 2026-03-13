-- | Generated tool: echo
module GeneratedTools.Echo where

import Runix.Safe.Polysemy (Sem)
import Data.Text (Text)
import Runix.Safe.Autodocodec (HasCodec(..), dimapCodec)
import UniversalLLM.Tools (ToolFunction(..), ToolParameter(..))

-- Generated tool: echo
newtype EchoInput = EchoInput Text
  deriving (Show, Eq)

instance HasCodec EchoInput where
  codec = dimapCodec EchoInput (\(EchoInput t) -> t) codec

instance ToolParameter EchoInput where
  paramName = "text"
  paramDescription = "The text to echo back"

newtype EchoResult = EchoResult Text
  deriving (Show, Eq)

instance HasCodec EchoResult where
  codec = dimapCodec EchoResult (\(EchoResult t) -> t) codec

instance ToolParameter EchoResult where
  paramName = "result"
  paramDescription = "The echoed text"

instance ToolFunction EchoResult where
  toolFunctionName = "echo"
  toolFunctionDescription = "A simple tool that echoes back the provided text input"

echoTool :: EchoInput -> Sem r EchoResult
echoTool (EchoInput text) = return $ EchoResult text

