module Run.Console
  ( error
  , errorShow
  , info
  , infoShow
  , log
  , logShow
  , warn
  , warnShow
  , runConsole
  , runNoConsole
  , runAccumulate
  , runEff
  , runPure
  , Console(..)
  , CONSOLE
  , _console
  )
  where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Rec.Class (Step(..))
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor (class Functor, (<$), (<$>))
import Data.Functor.Variant (FProxy, on, onMatch)
import Data.List (reverse)
import Data.List.Types (List(..))
import Data.NaturalTransformation (type (~>))
import Data.Show (class Show, show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Run (EFF, Run, send)
import Run as Run

-- | The possible messages we can have on the console
-- |
-- | You'll only need to interact with this if you are writing an interpreter.
data Console a
  = Error String a
  | Info String a
  | Log String a
  | Warn String a

derive instance functorConsole :: Functor Console

-- | A type synonym to clean up some signatures
type CONSOLE
  = FProxy Console

-- | A helper for the `console` label used in row types.
-- |
-- | You'll only need to interact with this if you are writing an interpreter.
_console :: SProxy "console"
_console = SProxy

error :: forall r. String -> Run (console :: CONSOLE | r) Unit
error str = Run.lift _console (Error str unit)


errorShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
errorShow x = Run.lift _console (Error (show x) unit)

info :: forall r. String -> Run (console :: CONSOLE | r) Unit
info str = Run.lift _console (Info str unit)

infoShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
infoShow x = Run.lift _console (Info (show x) unit)

log :: forall r. String -> Run (console :: CONSOLE | r) Unit
log str = Run.lift _console (Log str unit)

logShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
logShow x = Run.lift _console (Log (show x) unit)

warn :: forall r. String -> Run (console :: CONSOLE | r) Unit
warn str = Run.lift _console (Warn str unit)

warnShow :: forall a r. Show a => a -> Run (console :: CONSOLE | r) Unit
warnShow x = Run.lift _console (Warn (show x) unit)

-- | Accumulates all console messages into a list
-- | but does not print any to the console.
-- |
-- | Useful when you want to see what would be printed to the console.
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

-- | Prints all messages to the console.
-- |
-- | Normally this is what you'll use to print to the console.
runConsole
  :: forall a e r
  . Run (console :: CONSOLE, eff :: EFF (console :: Eff.CONSOLE | e) | r) a
  -> Run (eff :: EFF (console :: Eff.CONSOLE | e) | r) a
runConsole = runEff case _ of
  Error str x -> x <$ Eff.error str
  Info str x -> x <$ Eff.info str
  Log str x -> x <$ Eff.log str
  Warn str x -> x <$ Eff.warn str

-- | Runs the given function in an effectful context.
-- | This converts the `CONSOLE` type into a
-- | `Control.Monad.Eff.Console.CONSOLE` effect.
-- |
-- | Useful for building up new effectful interpreters.
runEff
  :: forall a e r
  . Console ~> Eff (console :: Eff.CONSOLE | e)
  -> Run (console :: CONSOLE, eff :: EFF (console :: Eff.CONSOLE | e) | r) a
  -> Run (eff :: EFF (console :: Eff.CONSOLE | e) | r) a
runEff f = Run.interpretRec (on _console (liftEff <<< f) send)

-- | Runs without printing any messages to the console.
-- |
-- | Useful when you want to eliminate the `CONSOLE` type
-- | without printing anything to the console.
runNoConsole :: forall a r. Run (console :: CONSOLE | r) a -> Run r a
runNoConsole = runPure case _ of
  Error _ w -> w
  Info _ w -> w
  Log _ w -> w
  Warn _ w -> w

-- | Runs the given function in a pure context eliminating the `CONSOLE` type.
-- |
-- | Useful for building up new pure interpreters.
runPure
  :: forall a r
  . (Console (Run (console :: CONSOLE | r) a) -> Run (console :: CONSOLE | r) a)
  -> Run (console :: CONSOLE | r) a
  -> Run r a
runPure f = Run.runPure $ onMatch { console: Loop <<< f } Done
