# Haiku

This is the the compiler for the Haiku Language.

Installing it is easy with nix, the best package manager (and linux distro) in the world.
Just run the following command in your terminal to get the latest version:
```
nix profile add github:StefanValentinT/haiku-lang
```

In the future I want to bootstrap Haiku therefore I want to make installing through a non rust-dependent way possible,
but right now the seriously easiest way to install Haiku is jsut to download or clone this repo and then run the following two simple cargo commands in it.
```
cargo build
cargo install --path . --force
```

Happy coding! You may want to look into the `samples` folder for examples of the Haiku language or into `tests` for the automated compiler tests. Keep in mind this language is still very WIP.

# Roadmap to 0.2

Soon I want to release 0.2. That will be the first (real, not just for testing) release. At this point the language will not be stable,
but it will have most of its features and be usable for other people. Also I want to write raylib-bindings and make a
little game to prove the capabilities of Haiku. Until the release the following features have to be implemented.

- [ ] Strings and Vectors
- [ ] First class functions
- [ ] Module and imports
- [ ] Project based compiler
- [ ] a little stdlib
- [ ] totality checker
- [ ] Algebraic datatypes
- [ ] memory allocation

# About Haiku

Haiku strives to be a language for century, just one compiler can not be enough for this.
It lays in the very spirit of a language to be nothing more than a way to communicate,
the compiler is the mere receifer of these words of truth and beauty, unworthy of any further
consideration. Therefore Haiku as a language is defined by its standard which is currently still
in work. Every compiler that fulfills this specification may be regarded as the same, meaning
that there should be no apparent difference for the user. This includes necessary changes in the language.
All things have to decay, all water has to flow indefinitely. To assume a standard can be superior to this,
stay unwavering atop the ocean of rot is nothing but neglience for a language's health.

Haiku will change, its standard will progress, but for it to remain Haiku it needs to restrain itself
to a set of ideals, shiny icons of hope for what it shall become. These axioms do not require perfection,
but they will instead enable it. Haiku does not have to created to be better than anything that will ever come
thereafter; instead I want it to be able to flow with the river and always transform itself.

These axioms are:
- Simplicity: Haiku is a language of the water, it is like the river bed. Thus it is centered around functions,
the very act of transforming data into anaother representation of itself. The concept of simplicity extends into
the code as a whole too, it should be both easy to write and to read. This is achieved by having a minimal
amount of obligatory code and a coherent style in which the language is designed. One of its requirements is the absence
of requirements unless strictly needed, thus being basically everything allowed in function definition or in types.
- Elegance: Not all languages are created equally, some where made to already rot in some corporate office
from the moment of their inception. But Haiku wants to be a passionate, even manicial language.
- Safety: Haiku is a language to build safe, reliable and scalable systems in.
- Performance: Haiku follows the flow. Why should it impose barriers in its way?

Now go ahead and look at a piece of Haiku code in the samples folder or read the *Haiku Book* (Not made yet!).

# How to install

Install this compiler simply by cloning the repository and then executing `cargo run`.
This should just about work on any computer thanks to the cross-platform support of Rust’s toolchain and this project's minimal external dependencies.
