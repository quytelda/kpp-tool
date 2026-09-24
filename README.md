# kpp-tool

`kpp-tool` is a utility for interacting with Krita brush presets (i.e.
KPP files) from the command line. The program can inspect/change
preset metadata or extract embedded resources such as brush tips and
patterns.

# Usage

The most common pattern for invoking `kpp-tool` is:

```
kpp-tool [FLAGS] KPP_FILE
```

where `KPP_FILE` is the path to a preset file. A single dash can be
used to indicate the preset file should be read from `stdin`.

There are generally two kinds of command line flags:

1. Action flags correspond to specific operations the program can take
   with a preset, such as looking up properties or changing the
   preset's name. These flags are executed in the order they are
   provided.
2. Global flags configure global program settings that affect all
   actions, like enabling overwrite mode. The position and order of
   these flags does not matter - they can appear anywhere on the
   command line.

By default, the program doesn't save any modifications made to the
preset. Use the global `-O/--overwrite` flag to modify a preset file
in-place, or the `-o/--output` action to save a preset to another
file. When input is taken from `stdin`, the `-O/--overwrite` option
will write the final output to `stdout`.

Note that `-o/--output` is an action flag, so it should appear *after*
any changes you want to save on the command line. For example, you
probably don't want to run `kpp-tool --output=output.kpp
--set-name=foobar input.kpp`, which would write the output *before*
changing the name to "foobar". Instead, `kpp-tool --set-name=foobar
--output=output.kpp input.kpp` would change the name, then save the
result.

## Examples

```
# Print detailed information about a preset.
$ kpp-tool --info preset.kpp

# Change the name of a preset to "foobar".
$ kpp-tool -O --set-name=foobar preset.kpp

# Set the 'EraserMode' parameter to "true".
$ kpp-tool -O --set-param "EraserMode=string:true" preset.kpp

# Query the values of the 'FlowSensor' and 'FlowValue' parameters.
$ kpp-tool --get-param FlowSensor --get-param FlowValue preset.kpp

# Extract a preset's current icon image, then set a new one.
# The old icon will be saved in 'old-icon.png', while the new icon
# will be read from 'new-icon.png'.
$ kpp-tool -O --get-icon=old-icon.png \
              --set-icon=new-icon.png \
              preset.kpp

# Create three variations of a preset with different blending modes.
# This will result in three new KPP files which are identical to the
# original, except for their blending modes, which have been set to
# 'multiply', 'dodge', and 'soft_light_svg' respectively.
$ kpp-tool --set-param CompositeOp=string:multiply \
           --output preset_multiply.kpp \
           --set-param CompositeOp=string:dodge \
           --output preset_dodge.kpp \
           --set-param CompositeOp=string:soft_light_svg \
           --output preset_softlight.kpp \
           preset.kpp

# Extract the raw XML settings document from a preset file.
# This will print the full XML document to stdout, which might be very
# long if the preset has embedded resources.
$ kpp-tool dump-xml preset.kpp
```

## Regular Options

The list of supported command line flags for regular operation is as
follows:

### Global Options

```
-O  --overwrite  Overwrite the input file after processing
-q  --quiet      Supress unnecessary output
```

### Actions

```
-C  --set-icon        PATH                                                 Replace the preset icon image
-N  --set-name        STRING                                               Change preset name
-P  --set-param       KEY=TYPE:VALUE                                       Set the value of a parameter
-S  --sync-name                                                            Synchronize preset name with the filename
-X  --extract-all     [DIR]                                                Extract all embedded resources
-c  --get-icon        PATH                                                 Extract the preset icon image
-e  --embed           path=STRING,type=STRING,[name=STRING],[file=STRING]  Insert or replace an embedded resource
-i  --info                                                                 Print a summary of preset settings
-l  --list-params                                                          Print a table of parameters and their values
-n  --get-name                                                             Print preset name
-o  --output          PATH                                                 Write preset data to file ("-" for stdout)
-p  --get-param       KEY                                                  Look up the value of a parameter
-r  --list-resources                                                       List embedded resources
-x  --extract         {name=STRING|file=STRING|md5=STRING},[dest=PATH]     Extract an embedded resources
```

### Other Options

```
-h  --help     Display help and usage information
-v  --version  Display version information
```

## dump-xml

`kpp-tool` also supports a `dump-xml` subcommand, which is useful for
extracting the raw XML settings document from a preset file without
doing any parsing or processing. The syntax for this command is:

```
kpp-tool dump-xml [PRESET_FILE] [OUTPUT_FILE]
```

This command reads the preset at `PRESET_FILE`, extracts it's XML
settings document, and writes it to `OUTPUT_FILE`. If an `OUTPUT_FILE`
is not specified, the XML is written to `stdout`. If `PRESET_FILE` is
also unspecified (or `-`), the preset data will be read from `stdin`.

# Building

`kpp-tool` can be built using stack or cabal. Build dependencies
include `zlib`, as well as common build tools like `pkg-config` and
`g++`. Depending on your platform, you may need to install development
packages for some dependencies like `libc` and `zlib` (e.g. `zlib-dev`
on Alpine).

## Stack

You should be able to build the program by running `stack build` in
the root directory of the repository. `stack install` will copy the
executable to the local bin directory (usually `$HOME/.local/bin`),
which may need to be added to PATH.

## Cabal

Building with Cabal is fairly similar. After obtaining the source code
and required dependencies, running `cabal build` in the root directory
of the repository should build the software. Alternatively, to produce
a fully static binary, build with the `--enable-executable-static`
option and make sure statically linked `zlib` is available.

For example, here is the full build process on Alpine Linux.
```
# (as root)
# For static linking, also install the zlib-static package.
apk update
apk add git cabal g++ zlib-dev

# (as regular user)
git clone https://github.com/quytelda/kpp-tool.git
cd kpp-tool

cabal update
cabal build

# Alternatively, to produce a fully static binary:
cabal build --enable-executable-static
```
