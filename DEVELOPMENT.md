This document contains random, not always especially pertinent or maintained, documentation about the build process and development of LocoMotion, intended as a place for the research team to collect useful notes for its own use.

## Installing and testing development tools (purs, spago)

On OS X 10.15.7 I first re-installed the (then) latest version of node.js (16.3.2) which included npm 8.1.2. Then I did:
```
npm update
npm install -g purescript
npm install -g spago
purs --version
spago --version
```
The last 2 lines there were to verify the installed purescript version (it was 0.14.5) and spago version (0.20.3).

If you have purs and spago installed then you should be able to start a bare purescript repl with: ```spago repl```

Inside the purs/spago REPL this is how to quit: ```:quit```

And you can build and serve the project by changing into the project folder and doing (assuming you have python installed, which is used here just as a no-frills web server):
```
make build
make serve
````

Then you can connect to the just built project by opening a browser and going to: http://127.0.0.1:8000

## Miscellaneous development workflow notes

Useful reference re: working with Bower and Spago
https://jordanmartinez.github.io/purescript-jordans-reference-site/content/03-Build-Tools/01-Dependency-Managers/02-Spago-Explained.html

This is important to note:
https://github.com/purescript/spago#why-cant-spago-also-install-my-npm-dependencies

A short note re: passing Effect a computations into Javascript FFI:
https://github.com/purescript/documentation/blob/master/guides/FFI-Tips.md#why-doesnt-my-effect-work-when-passed-to-a-normal-js-function

An introduction to Aff for asynchronous programming in PureScript:
https://blog.drewolson.org/asynchronous-purescript

## Notes on things we are using

A big document that summarizes differences between Haskell and PureScript
https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md

The documention (on Pursuit) for the Parsing package, which is very similar to Haskell's parsec library:
https://pursuit.purescript.org/packages/purescript-parsing/8.1.0

A great blog post about the use of the Aff monad for asynchronous & parallel programming:
https://blog.drewolson.org/asynchronous-purescript



Halogen.HTML module
provides key type (HTML w i) as well as definitions for DOM elements (Node-s)
Documentation: https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.HTML

Halogen.HTML.Properties module
note that the Halogen property for HTML class is called class_ because of conflict with reserved word
Documentation: https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.HTML.Properties
about "row types" from the PureScript documentation: https://github.com/purescript/documentation/blob/master/language/Types.md#rows
and in the PureScript by example book: https://book.purescript.org/chapter5.html#record-patterns-and-row-polymorphism

basic explanation of above two modules:
https://purescript-halogen.github.io/purescript-halogen/guide/01-Rendering-Halogen-HTML.html

the last part of the above basic explanation might come in handy in the future, about adding unsupported DOM properties:
https://purescript-halogen.github.io/purescript-halogen/guide/01-Rendering-Halogen-HTML.html#adding-missing-properties
