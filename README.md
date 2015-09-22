SFML
====

Low level Haskell bindings for SFML 2.x

### What's been wrapped:

* Window module
* System module
* Graphics module
* Audio module

### What's been left out:

* Threading and networking, since Haskell has better alternatives

### What's been tested:

* Window module, partially
* System module, all but vectors
* Graphics module, partially
* Audio module, partially

### Where it's been tested:

* Arch Linux 32-bit
* Ubuntu Linux 32-bit
* Windows 7 32-bit

### Installation (brief)

- Download latest CSFML 2.x
- Make sure your compiler can find CSFML's headers and libraries, then run:

```
$ git clone https://github.com/SFML-haskell/SFML.git
$ cd SFML
$ cabal install
```

For detailed installation instructions head to the [wiki][1].

[0]: https://github.com/jeannekamikaze/SFML/blob/master/demos/demos.cabal
[1]: https://github.com/jeannekamikaze/SFML/wiki
