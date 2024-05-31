# Zapp (Zapp Assembles Packrat Parsers)

Zapp is a packrat parser generator, 
that is written in Zig and generates code written in zig.

## Documentation

There is no documentation yet, it is currently beeing written.

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
