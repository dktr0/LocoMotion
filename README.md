
## Miscellaneous development workflow notes

Useful reference re: working with Bower and Spago
https://jordanmartinez.github.io/purescript-jordans-reference-site/content/03-Build-Tools/01-Dependency-Managers/02-Spago-Explained.html

This is important to note:
https://github.com/purescript/spago#why-cant-spago-also-install-my-npm-dependencies


## Notes on things we are using

A big document that summarizes differences between Haskell and PureScript
https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md

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
