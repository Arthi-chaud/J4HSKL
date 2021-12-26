[![J4HSKL](assets/logo.png)](assets/logo.png)

### Manage JSON with Haskell

[![Build](https://github.com/Arthi-chaud/J4HSKL/actions/workflows/build.yaml/badge.svg)](https://github.com/Arthi-chaud/J4HSKL/actions/workflows/build.yaml)
[![Documentation](https://img.shields.io/badge/Documentation-Haddock-blue)](https://Arthi-chaud.github.io/J4HSKL/)
[![Unit tests](https://github.com/Arthi-chaud/J4HSKL/actions/workflows/unit_tests.yaml/badge.svg)](https://github.com/Arthi-chaud/J4HSKL/actions/workflows/unit_tests.yaml)

## Features

- Parse JSON-formatted String into Haskell data
- Display data using JSON format
- Native Data types

## Usage

To parse JSON data from ```String```, use ```parseJSON :: Parser JSONValue``` from ```J4HSKL.Parser``` (with ```Parser``` being a simple Parser)

If the ```String``` contains **only** JSON Data, use ```parseStrictJSON :: String -> Maybe JSONValue``` from ```J4HSKL.Parser```

Read documentation [here](https://arthi-chaud.github.io/J4HSKL/J4HSKL-Parser.html) to understand how to user these parsers

In J4HSKL, a JSON Value is a data type using native Haskell data types: ```Bool```, ```Integer```, ```String```, ```List```...

Using ```JSONValue``` data, you can format them using JSON format, thanks to ```show```

Read documentation [here](https://arthi-chaud.github.io/J4HSKL/J4HSKL-Data.html) to understand how to manage JSON-parsed data

To import from/export to file, use respectively ```importJSON``` and ```exportJSON``` functions

These functions are documented [here](https://arthi-chaud.github.io/J4HSKL/J4HSKL-File.html)

## Upcoming features

- Beautified dump
- Import/export from file
- Better delivery method

## Build

To build J4HSKL library, run ```stack build```

## Toolstack

- Stack
- HUnit
- Haddock
