# Lunrelm full text indexer

Copyright (c) 2016 Robin Luiten

This is a full text indexing engine inspired by lunr.js and written in Elm language.
See http://lunrjs.com/ for lunr.js

This version works but can not save or load indexes yet. So it may not be of much value yet.

Several packages were created for this project to use as follows

* trie
 * http://package.elm-lang.org/packages/rluiten/trie/latest
* stemmer
 * http://package.elm-lang.org/packages/rluiten/stemmer/latest
* sparsevector
 * http://package.elm-lang.org/packages/rluiten/sparsevector/latest

## Issues

* Performance is has not been tested there may be some rough spots.
* Save and Load of an index is not yet implemented.

### Parts of lunr.js were left out

* This does not have an event system.
* Its internal data structure is not compatible.

### Behaviours

* add doc returns Err if empty document reference field
* remove doc returns Err if empty document reference field
* search returns Err if query is empty

#### Behaviours under consideration

* should add return Err if all doc fields
 * YES at moment
* should add return Err if doc already in index
 * YES at moment
* should remove return Err if doc not in index
 * YES at moment
* should search return Err if query is non empty
but after token processing there is nothing left to
search with
 * YES at moment

#### Bugs discovered doing port of lunr.js to Lunrelm

* lunr.js
 * tokenStore.remove does not decrement length, but it doesnt use length really only save/load
 * stemmer "lay" -> "lay" "try" -> "tri" is opposite to porter stemmer
* porter stemmer erlang implementation
 * step5b does not use endsWithDoubleCons which is required afaik to pass the voc.txt output.txt cases
