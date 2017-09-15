# Probably Wrong Chain

This is currently a very hacky attempt at building a Blockchain implementation
using Haskell.

Loosely based on the following blog posts:

- [A blockchain in 200 lines of Javascript](https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54)
- [Build your own blockchain in Python](http://ecomunsing.com/build-your-own-blockchain)

There is no running system at the moment, but the moving parts are there will
continue to be chipped at as time goes by. The actual mechanisms of receiving
new data and extending the chain are handled by an FRP network using [reactive-banana](https://hackage.haskell.org/package/reactive-banana-1.1.0.1).

There is no "Proof of Work" implemented at this stage, but that is on the TODO
list and will most likely resemble a [Hashcash system](https://en.wikipedia.org/wiki/Hashcash).
