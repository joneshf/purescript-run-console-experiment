module Run.Console where

import Control.Applicative (pure)
import Control.Bind (bindFlipped)
import Control.Monad.Eff.Console as Eff
import Control.Semigroupoid ((<<<))
import Data.Either (either)
import Data.Functor (class Functor, (<$), (<$>))
import Data.Functor.Variant (FProxy, on)
import Data.List (reverse)
import Data.List.Types (List(..))
import Data.Show (class Show, show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Run (BaseEff, Run, interpret, liftEffect, peel, send)

data Console a
  = Error String a
  | Info String a
  | Log String a
  | Warn String a

derive instance functorConsole :: Functor Console

type CONSOLE
  = FProxy Console

_console :: SProxy "console"
_console = SProxy

error :: forall r. String -> Run (console :: CONSOLE | r) Unit
error str = liftEffect _console (Error str unit)

errorShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
errorShow x = liftEffect _console (Error (show x) unit)

info :: forall r. String -> Run (console :: CONSOLE | r) Unit
info str = liftEffect _console (Info str unit)

infoShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
infoShow x = liftEffect _console (Info (show x) unit)

log :: forall r. String -> Run (console :: CONSOLE | r) Unit
log str = liftEffect _console (Log str unit)

logShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
logShow x = liftEffect _console (Log (show x) unit)

warn :: forall r. String -> Run (console :: CONSOLE | r) Unit
warn str = liftEffect _console (Warn str unit)

warnShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
warnShow x = liftEffect _console (Warn (show x) unit)

runConsole
  :: forall a e r
  . Run (console :: CONSOLE | r) a
  -> Run (base :: BaseEff (console :: Eff.CONSOLE | e) | r) a
runConsole = interpret _console case _ of
  Error str x -> x <$ Eff.error str
  Info str x -> x <$ Eff.info str
  Log str x -> x <$ Eff.log str
  Warn str x -> x <$ Eff.warn str

runNoConsole
  :: forall a r
  . Run (console :: CONSOLE | r) a
  -> Run r a
runNoConsole = runPure case _ of
  Error _ w -> w
  Info _ w -> w
  Log _ w -> w
  Warn _ w -> w

runPure
  :: forall a r
  . (Console (Run (console :: CONSOLE | r) a) -> Run (console :: CONSOLE | r) a)
  -> Run (console :: CONSOLE | r) a
  -> Run r a
runPure f = go
  where
  go x = either (on _console (go <<< f) (bindFlipped go <<< send)) pure (peel x)

runAccumulate
  :: forall a r
  . Run (console :: CONSOLE | r) a
  -> Run r (List String)
runAccumulate x' = reverse <$> runPure cons (Nil <$ x')
  where
  cons = case _ of
    Error str w -> Cons str <$> w
    Info str w -> Cons str <$> w
    Log str w -> Cons str <$> w
    Warn str w -> Cons str <$> w
