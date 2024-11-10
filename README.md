# kpp-tool

`kpp-tool` is a utility for interacting with Krita brush presets (i.e.
KPP files) from the command line. The program can inspect/change
preset metadata or extract embedded resources such as brush tips and
patterns.


# Usage

The most common pattern for invoking `kpp-tool` is:

```
kpp-tool [FLAGS] [KPP_FILE]
```

where `KPP_FILE` is the path to a preset file. If no path is provided,
the program will attempt to read a file from `stdin`.

There are generally two kinds of command line flags:

1. Operation flags correspond to specific actions the program can take
   with a preset, such as looking up properties or changing the
   preset's name. These flags are executed in the order they are
   provided.
2. Global flags configure global program settings that affect all
   operations, like enabling overwrite mode. The position and order of
   these flags does not matter - they can appear anywhere on the
   command line.

By default, the program doesn't save any modifications made to the
preset. Use the global `-O/--overwrite` flag to modify a preset file
in-place, or the `-o/--output` operation to save a preset to another
file. When input is taken from `stdin`, the `-O/--overwrite` option
will write the final output to `stdout`.

Note that `-o/--output` is an operation flag, so it should appear
*after* any changes you want to save on the command line. For example,
you probably don't want to run `kpp-tool --output=output.kpp
--set-name=foobar input.kpp`, which would write the output *before*
changing the name to "foobar". Instead, `kpp-tool --set-name=foobar
--output=output.kpp input.kpp` would change the name, then save the result.

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
$ kpp-tool -O --get-icon=old-icon.png \
              --set-icon=new-icon.png \
              preset.kpp

# Create three variations of a preset with different blending modes.
$ kpp-tool --set-param CompositeOp=string:multiply \
           --output preset_multiply.kpp \
           --set-param CompositeOp=string:dodge \
           --output preset_dodge.kpp \
           --set-param CompositeOp=string:soft_light_svg \
           --output preset_softlight.kpp \
           preset.kpp
```

## Options

The full list of supported command line flags is as follows:

```
-h                  --help                      Display help and usage information.
-v                  --version                   Display version information.
```

### Global Options

```
-O                  --overwrite                 Modify a preset file in-place.
-q                  --quiet                     Supress unnecessary output.
```

### Operations

```
-o PATH             --output=PATH               Write preset data to PATH.
                                                If PATH is "-", data will be written to stdout.
-i                  --info                      Print a description of a preset.
-n                  --get-name                  Print a preset's metadata name.
-N STRING           --set-name=STRING           Change a preset's metadata name.
-S                  --sync-name                 Change a preset's metadata name to match it's filename.
                                                For example, 'kpp-tool --sync-name foobar.kpp' will change
                                                the preset's name to "foobar".
-p KEY              --get-param=KEY             Print the value of a single parameter.
-P KEY=TYPE:VALUE   --set-param=KEY=TYPE:VALUE  Set the value of a parameter.
                                                TYPE can be 'string', 'internal', or 'binary'.
                                                For binary parameters, VALUE should be encoded in base-64.
-x KEY=VALUE[,...]  --extract=KEY=VALUE[,...]   Extract an embedded resource.
-X                  --extract-all               Extract all embedded resources.
-e KEY=VALUE[,...]  --embed=KEY=VALUE[,...]     Insert or update a resource file.
-c PATH             --get-icon=PATH             Extract a preset's PNG icon image.
-C PATH             --set-icon=PATH             Change a preset's icon image.
                                                FILE must be a PNG file.
```
