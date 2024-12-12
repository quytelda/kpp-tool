# Changelog for `kpp-tool`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 1.0.0 - 2024-12-25

### Added

- A `--list-resources` option for listing resources
- A `--list-params` option for list parameters
- A `--dump-xml` option for extracting raw settings XML
- Support for filter presets
- A function for generating iTXt chunks
- Expanded unit tests

### Changed

- Allow extracting resources to a directory with `--extract-all`
- Rename App module to Kpp.App
- Rename Preset module to Kpp.Preset
- Move PNG parsing code into a new module (Kpp.Png)

### Fixed

- Parameters with no type no longer cause parsing failure
- Parameters with empty content no longer cause parsing failure

## 0.1.0 - 2024-11-13

### Added

- Parsing and rendering of KPP files
- The ability to change a preset's name
- The ability to synchronize a preset's name and filename
- The ability to get and set preset parameters
- The ability to extract and embed resources
- The ability to get and set preset icons
- A suite of unit tests
