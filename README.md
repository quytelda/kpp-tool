# kpp-tool

`kpp-tool` is a utility for interacting with Krita brush presets (i.e.
KPP files) from the command line. The program can inspect/change
preset metadata or extract embedded resources such as brush tips and
patterns.

# Usage

The executable expects a sequence of "command" flags representing
access or edit operations, followed by an input file. Commands are
executed in order using the provided file as input. By default, no
output is written to disk unless the `-o/--output` command is used or
the global `-i/--in-place` flag is present.

For example, `kpp-tool --set-name=foobar --show --get-icon=icon.png
-o updated-preset.kpp my-preset.kpp` will read the preset from
`my-preset.kpp` and apply the following operations in order:

1. Change the preset name to "foobar".
2. Print a descriptive summary of all preset settings and data.
3. Extract the preset's PNG icon and write it to `icon.png`.
4. Save the altered preset in `updated-preset.kpp`.

The original file (`my-preset.kpp`) will not be changed.

## Command Line Options

```
$ kpp-tool --help
kpp-tool
  -h            --help                          Display help and usage information.
  -v            --version                       Display version information.
  -i            --in-place                      Modify a preset file in-place.
  -o FILE       --output=FILE                   Write preset data to FILE.
  -s            --show                          Print a description of a preset.
  -n            --get-name                      Print a preset's metadata name.
  -N STRING     --set-name=STRING               Change a preset's metadata name.
  -y            --sync-name                     Change a preset's metadata name to match it's filename.
                                                For example, 'kpp-tool --sync-name foobar.kpp' will change
                                                the preset's name to "foobar".
  -p KEY        --get-param=KEY                 Print the value of a single parameter.
                                                If the value is binary, it will be displayed in base-64.
  -S KEY=VALUE  --set-param-string=KEY=VALUE    Set the value of a string parameter.
  -P KEY=VALUE  --set-param-internal=KEY=VALUE  Set the value of an internal parameter.
  -B KEY=VALUE  --set-param-binary=KEY=VALUE    Set the value of binary (bytearray) parameter.
                                                VALUE should be encoded in base-64.
  -r RESOURCE   --extract=RESOURCE              Extract an embedded resource.
  -x            --extract-all                   Extract all embedded resources.
  -R RESOURCE   --insert=RESOURCE               Insert or update a resource file.
  -c FILE       --get-icon=FILE                 Extract a preset's icon image.
  -C FILE       --set-icon=FILE                 Change a preset's icon image.
                                                FILE must be a PNG file.
```
