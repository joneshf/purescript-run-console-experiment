# purescript-run-console-experiment


[![Latest release](http://img.shields.io/bower/v/purescript-run-console-experiment.svg)](https://github.com/joneshf/purescript-run-console-experiment/releases)

## Installation

```
bower install purescript-run-console-experiment
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-run-console-experiment).

## Why `-experiment`?

This is only an experiment because I don't know if I can maintain it well enough and I don't feel like squatting on the name `purescript-run-console`.
It has feature parity with `purescript-console`.
So, if that's your concern be at ease.

## How do I use this?

Much like `purescript-console`.
You write your code using functions, and at the end you have to `run` it.

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

import Run (runBase)
import Run.Console (log, runConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBase $ runConsole do
  log "Hello sailor!"
```

## Why would I use this?

You may be asking yourself, "Self, why would I use this?"
There's probably a ton of reasons someone could give you for why you would.
Here's one reason: sometimes you write logging code for development, that you don't want to show up in production.
If you use this library, the change is minimal.

Say you have some code like this:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (runBase)
import Run.Console (log, runConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBase $ runConsole do
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

import Run (runBase)
import Run.Console (log, runConsole)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runBase $ runNoConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  log "Goodbye sailor!"
```

If you wanted to be more specific,
you could change the line to `pure $ run $ runNoConsole` and remove the `Control.Monad.Eff.Console.CONSOLE` effect:

```PureScript
module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Run (runBase)
import Run.Console (log, runConsole)

main :: forall e. Eff e Unit
main = pure $ run $ runNoConsole do
  log "Hello sailor!"
  -- do a bunch of stuff
  log "Goodbye sailor!"
```
