# purescript-run-console-experiment


[![Latest release](http://img.shields.io/bower/v/purescript-run-console-experiment.svg)](https://github.com/joneshf/purescript-run-console-experiment/releases)
[![Build Status](https://travis-ci.org/joneshf/purescript-run-console-experiment.svg?branch=master)](https://travis-ci.org/joneshf/purescript-run-console-experiment)

## Installation

```
bower install purescript-run-console-experiment
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-run-console-experiment).

## Why `-experiment`?

This is only an experiment because I don't know if I can maintain it well enough and I don't feel like squatting on the name `purescript-run-console`.
It has feature parity with `purescript-console`.
If that's your concern, be at ease.

If you want to maintain this, please do, but maintain it under `purescript-run-console`!
Maybe this is could go in `purescript-contrib` or directly in `purescript`?

## How do I use this?

Much like `purescript-console`.
You write your code using functions, and at the end you have to "run" it.

If you had something like this:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
```

You would change it to this:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (runBaseEff)
import Run.Console (log, runConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBaseEff $ runConsole do
  log "Hello sailor!"
```

## Why would I use this?

You may be asking yourself, "Self, why would I use this?"
There's probably a ton of reasons someone could give you for why you would.
Here are a couple of reasons.

### Turn off all logs in production

Sometimes you write logging code for development, that you don't want to show up in production.
If you use this library, the change is minimal.

Say you have some code like this:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (runBaseEff)
import Run.Console (log, runConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBaseEff $ runConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  log "Goodbye sailor!"
```

To turn off logging, you change the function `runConsole` to `runNoConsole`.
That's it!

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (runBaseEff)
import Run.Console (log, runNoConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBaseEff $ runNoConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  log "Goodbye sailor!"
```

If you wanted to be more specific,
you could change the line to `pure $ extract $ runNoConsole` and remove the `Control.Monad.Eff.Console.CONSOLE` effect:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (extract)
import Run.Console (log, runNoConsole)

main :: forall e. Eff e Unit
main = pure $ extract $ runNoConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  log "Goodbye sailor!"
```

### Test logging code quickly

Sometimes you want to ensure that what is being logged is correct.
You can take the console messages you write, and run them with a different interpreter that accumulates all logs.

Say you had some code like this:

```PureScript
module Main where

import Prelude

import Run.Console (CONSOLE, log)

sailorTime :: forall r. Run (console :: CONSOLE | r) Unit
sailorTime = do
  log "Hello sailor!"
  log "Goodbye sailor!"
```

In order to test that the two messages are sent properly, we can run an accumulating interpreter:

```PureScript
module Main where

import Prelude

import Data.List.Types (List)

import Run (extract)
import Run.Console (CONSOLE, log, runAccumulate)

sailorTime :: forall r. Run (console :: CONSOLE | r) Unit
sailorTime = do
  log "Hello sailor!"
  log "Goodbye sailor!"

logs :: List String
logs = extract $ runAccumulate sailorTime
```

This code does not reach out to the actual console and write anything; it's completely pure!
All it does is evaluate `sailorTime` as though it were any other value in PureScript.
`logs` contains both of those messages and you could write a test that verifies

```PureScript
module Main where

import Prelude

import Data.List.Types (List(..), (:))

import Run (extract)
import Run.Console (CONSOLE, log, runAccumulate)

import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner as Test.Spec.Runner

sailorTime :: forall r. Run (console :: CONSOLE | r) Unit
sailorTime = do
  log "Hello sailor!"
  log "Goodbye sailor!"

logs :: List String
logs = extract $ runAccumulate sailorTime

main = do
  Test.Spec.Runner.run [specReporter] do
    it "logs are correct" do
      logs `shouldEqual` ("Hello sailor!" : "Goodbye sailor!" : Nil)
```

## How can I do something advanced?

Let's say that you don't like the idea of turning off all messages in production.
Instead, you'd like to still show the `error` messages, but ignore `info`, `log`, and `warning`.
Additionally, you want to prefix the `error` messages with a bright red label.
You can do that by supplying a different interpreter to run.
Let's write that interpreter!

The first thing to know is that `Console a` is just a Plain Old PureScript Type.
It has a case for each level of message it can handle: `Error`, `Info`, `Log`, and `Warn`.
So we can do things like case on it and decide what to do in each case.
In every case except the `Error` case, we want to ignore the supplied message.
The general idea of what we want is a function like:

```PureScript
module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)

import Control.Monad.Eff.Console as Eff

import Run.Console (Console(..))

go :: forall a e. Console a -> Eff (console :: Eff.CONSOLE | e) a
go = case _ of
  Error s x -> Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s) $> x
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure x
```

We pull in `purescript-ansi` for the coloring and do the following for the `Error` case:

* construct the `BrightRed` label
* append the string we get to this label
* log the string to the actual console
* replace the `Unit` from `Eff (console :: Eff.CONSOLE | e) Unit` with `a`

As it turns out, this function is a `NaturalTransformation` from `Console` to `Eff (console :: Eff.CONSOLE | e)`.
You might see `NaturalTransformation` as an alias `(~>)` often.
Notice that we don't touch whatever the `a` is in `Console`;
we just pass it right along to `Eff (console :: Eff.CONSOLE | e)`.
So we have something like `Canvas ~> Eff (console :: Eff.CONSOLE | e)`.
We can rewrite the signature to reflect that fact.

```PureScript
module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)

import Control.Monad.Eff.Console as Eff

import Run.Console (Console(..))

go :: forall e. Console ~> Eff (console :: Eff.CONSOLE | e)
go = case _ of
  Error s x -> Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s) $> x
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure x
```

Now that we have our function that does what we want,
we can use it with `runEff` to build an interpreter!

```PureScript
module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)

import Control.Monad.Eff.Console as Eff

import Run (EFF, Run)
import Run.Console (CONSOLE, Console(..), runEff)

go :: forall e. Console ~> Eff (console :: Eff.CONSOLE | e)
go = case _ of
  Error s x -> Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s) $> x
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure xit

runProduction
  :: forall a e r
  . Run (console :: CONSOLE, eff :: EFF (console :: Eff.CONSOLE | e) | r) a
  -> Run (eff :: EFF (console :: Eff.CONSOLE | e) | r) a
runProduction = runEff go
```

Aside: Notice that to write an interpreter that interprets into `EFF e`,
we have to assume the given `Run r a` already has `EFF e` as part of its row `r`.

This idea may seem confusing.
But, consider what would happen if we did not specify the given `Run r a`
had `EFF e` as part of its row `r`.
We would be saying that by interpreting with `runProduction`,
we would be "introducing" the `EFF e` into the row `r`.
If we had a similar interpreter, it too would "introduce" `EFF e` into the row `r`.
We would end up with duplicate labels!

Rather than opening that can of worms,
`purescript-run` forces us to say that
the given `Run r a` already has `EFF e` as part of `r`.

In fact, we can even inline the function if we'd like:

```PureScript
module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)

import Control.Monad.Eff.Console as Eff

import Run (EFF, Run)
import Run.Console (CONSOLE, Console(..), runEff)

runProduction
  :: forall a e r
  . Run (console :: CONSOLE, eff :: EFF (console :: Eff.CONSOLE | e) | r) a
  -> Run (eff :: EFF (console :: Eff.CONSOLE | e) | r) a
runProduction = runEff case _ of
  Error s x -> Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s) $> x
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure x
```

That's it!
Now, we can run this interpreter just like any other.
But it will only print the error messages.

```PureScript
module Main where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Eff

import Run (EFF, Run, runBaseEff)
import Run.Console (CONSOLE, Console(..), error, log, runEff)

runProduction
  :: forall a e r
  . Run (console :: CONSOLE, eff :: EFF (console :: Eff.CONSOLE | e) | r) a
  -> Run (eff :: EFF (console :: Eff.CONSOLE | e) | r) a
runProduction = runEff case _ of
  Error s x -> x <$ Eff.log (withGraphics (foreground BrightRed) "[ERROR] " <> s)
  Info _ x -> pure x
  Log _ x -> pure x
  Warn _ x -> pure x

main :: forall e. Eff (console :: Eff.CONSOLE | e) Unit
main = runBaseEff $ runProduction do
  log "Hello sailor!"
  -- do a bunch of stuff
  error "Oh no sailor!"
  log "Goodbye sailor!"
```

The only message we'll see is `"[ERROR] Oh no sailor!"` where the `[ERROR]` is bright red.
All of the rest, we ignore.

This idea can be extended to most anything you need.
You could format log messages in a different format, apply filtering on certain messages, add timestamps.
