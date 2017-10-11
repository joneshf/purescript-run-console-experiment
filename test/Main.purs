module Test.Main where

import Control.Bind (discard)
import Control.Monad.Eff (Eff)
import Data.Function (($))
import Data.List.Types (List(..), (:))
import Data.Unit (Unit, unit)
import Run (Run, extract, runBaseEff)
import Run.Console (CONSOLE, error, info, log, runAccumulate, runConsole, runNoConsole, warn)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (RunnerEffects)
import Test.Spec.Runner as Test.Spec.Runner

main :: Eff (RunnerEffects ()) Unit
main = do
  runBaseEff $ runConsole script

  Test.Spec.Runner.run [specReporter] do
    describe "Run.Console" do
      describe "runAccumulate" do
        it "accumulates console messages" do
          extract (runAccumulate script) `shouldEqual`
            ( "This is an error message"
            : "This is an info message"
            : "This is a log message"
            : "This is a warn message"
            : Nil
            )
      describe "runNoConsole" do
        it "does nothing" do
          extract (runNoConsole script) `shouldEqual` unit

script :: forall e. Run (console :: CONSOLE | e) Unit
script = do
  error "This is an error message"
  info "This is an info message"
  log "This is a log message"
  warn "This is a warn message"
