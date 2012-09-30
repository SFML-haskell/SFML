SFML
====

Low level Haskell bindings for SFML 2.x

### Haddocks
http://shellblade.net/docs/SFML/index.html


### What's been wrapped:

* Window module
* System module

### What's been tested:

* Window module, partially
* System module, all but vectors

### Where it's been tested:

* Arch Linux 32-bit

### Installation

Install SFML and CSFML on your system.

Make sure your compiler can find SFML's and CSFML's headers.

Run the following:

```
$ git clone https://github.com/jeannekamikaze/SFML.git
$ cd SFML
$ cabal install
```

### Linking

Binaries must be explicitly linked with whatever CSFML libraries they require (-lcsfml-system etc).

Check the cabal file inside the demos/ directory for an example.