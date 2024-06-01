# Zapp (Zapp Assembles Packrat Parsers)

Zapp is a packrat parser generator, 
that is written in Zig and generates code written in zig.

## Documentation

The documentation is [here](https://github.com/PiIsRational/zapp/wiki):
* the grammar format is described [here](https://github.com/PiIsRational/zapp/wiki/Grammar-syntax)
* using actions in the grammar is explained [here](https://github.com/zapp/PiIsRational/wiki/Actions)
* informations about using zapp [here](https://github.com/PiIsRational/zapp/wiki/Zapp-Specifics)
* the interface of the generated parsers is explained [here](https://github.com/PiIsRational/zapp/wiki/The-Parser-interface)

## Installation

There is currently no prepackaged binaries,
so the project has to be built from source.

### Build Requirements

* zig >= 0.12.0

### Building The project

First the repository has to be cloned: 

```shell
git clone https://github.com/PiIsRational/zapp
```

got to the directory of the project and execute to build it:

```shell
zig build -Doptimize=ReleaseSafe
```

the binary will be: `./zig-out/bin/zapp`
