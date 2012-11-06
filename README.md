SFML
====

Low level Haskell bindings for SFML 2.x

### Haddocks
http://shellblade.net/docs/SFML/index.html


### What's been wrapped:

* Window module
* System module
* Graphics module

### What's been tested:

* Window module, partially
* System module, all but vectors

### Where it's been tested:

* Arch Linux 32-bit
* Ubuntu Linux 32-bit
* Windows 7 64-bit

### Installation

Install SFML and CSFML on your system. You have to install them off the git repositories; the 2.0 RC snapshot won't work.

Make sure your compiler can find SFML's and CSFML's headers.

For detailed installation instructions head to the [wiki][1].

Run the following:

```
$ git clone https://github.com/jeannekamikaze/SFML.git
$ cd SFML
$ cabal install
```

[0]: https://github.com/jeannekamikaze/SFML/blob/master/demos/demos.cabal
[1]: https://github.com/jeannekamikaze/SFML/wiki