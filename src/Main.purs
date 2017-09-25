module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Eff
import Run (BaseEff, Run, runBase)
import Run.Console (CONSOLE, Console(Warn, Log, Info, Error), error, log, runConsole, runEff)

runProduction
  :: forall a e r
  . Run (console :: CONSOLE | r) a
  -> Run (base :: BaseEff (console :: Eff.CONSOLE | e) | r) a
runProduction = runEff case _ of
  Error s x -> x <$ Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s)
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure x

main :: forall e. Eff (console :: Eff.CONSOLE | e) Unit
main = runBase $ runConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  error "Oh no sailor!"
  log "Goodbye sailor!"
