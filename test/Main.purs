module Test.Main where

import Control.Bind (discard)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Eff
import Control.Monad.Eff.Timer (TIMER)
import Data.Function (($))
import Data.List.Types (List(..), (:))
import Data.Unit (Unit, unit)
import Node.Process (PROCESS)
import Run (Run, run, runBase)
import Run.Console (CONSOLE, error, info, log, runAccumulate, runConsole, runNoConsole, warn)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner as Test.Spec.Runner

main
  :: forall e
  . Eff
    ( avar :: AVAR
    , console :: Eff.CONSOLE
    , process :: PROCESS
    , timer :: TIMER
    | e
    )
    Unit
main = do
  runBase $ runConsole script

  Test.Spec.Runner.run [specReporter] do
    describe "Run.Console" do
      describe "runAccumulate" do
        it "accumulates console messages" do
          run (runAccumulate script) `shouldEqual`
            ( "This is an error message"
            : "This is an info message"
            : "This is a log message"
            : "This is a warn message"
            : Nil
            )
      describe "runNoConsole" do
        it "does nothing" do
          run (runNoConsole script) `shouldEqual` unit

script :: forall e. Run (console :: CONSOLE | e) Unit
script = do
  error "This is an error message"
  info "This is an info message"
  log "This is a log message"
  warn "This is a warn message"
