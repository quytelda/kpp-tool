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

where `KPP_FILE` is the path a preset file. If no path is provided,
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
```
