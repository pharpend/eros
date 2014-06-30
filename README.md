# Eros

A Haskell library for text censorship, using
[DansGuardian Phraselists](http://contentfilter.futuragts.com/phraselists/).

I converted those Phraselists to JSON. You can see the converted Phraselists
[here](https://github.com/pharpend/eros/tree/master/res/phraselists-pretty). There
are
[compressed versions](https://github.com/pharpend/eros/tree/master/res/phraselists-ugly)
for use in your code.

Eros is still in development, and is not ready to be actually used. If you would
like to contribute, please do.

At the time of this writing, I'm at version 0.3 (see
[here](https://github.com/pharpend/eros/releases)) for the current release.

See the [API documentation on Hackage](http://hackage.haskell.org/package/eros)
if you want to learn how to use the library.

# Usage - v.0.3.0.0

This is a usage guide for version 0.3.0.0. There will be more up-to-date usage
guides as more versions come, hopefully.

To install, run `cabal install eros`.

## `erosc`

The only way to interact with the library, for the time being is through the
client I built, `erosc`. It accepts input in Javascript Object Notation (JSON)
through `stdin`.

Here is the schema for the input:

```json
{
  "name": "erosc-input",
  "type": "object",
  "description": "The schema for the input to erosc.",
  "properties": {
    "text": {
      "type": "string",
      "description": "The text you want to be checked against the phraselists.",
      "required": true
    },
    "eros-lists": {
      "type": "array",
      "description": "The phraselists provided by eros you want \"text\" to be checked against.",
      "required": true,
      "items": {
        "type": "string"
      }
    },
  }
}
```

That is the up-to-date schema, as of version 0.3.0.0 . It is liable to
change. If the version you downloaded is greater than 0.3.0.0, make sure to
check the schema (it is distributed with the package) to make sure it is up to
date. The schema is located in `res/schemata/erosc-input.json`.

`erosc` will take that input, and send back output in JSON, in accordance with
the schema found in `res/schemata/erosc-output.json`.

```json
{
  "name": "erosc-output",
  "type": "array",
  "description": "The output of erosc.",
  "items": {
    "type": "object",
    "description": "The phraselist name, along with the score for that phraselist.",
    "properties": {
      "eros-list": {
        "type": "string",
        "description": "The name of the phraselist corresponding to this object."
      },
      "score": {
        "type": "number",
        "description": "The score corresponding to this phraselist."
      }
    }
  }
}
```

## Example

This is the example input from `res/erosc-dummy-inputs/input0.json`.

```json
{
  "text": "Fuck you, you fucking fuck! Fucking bitch tits milf sex sluts!",
  "eros-lists": [
    "gambling",
    "pornography"
  ]
}
```

Running `erosc < res/erosc-dummy-inputs/input0.json` yields

```json
[
  {
    "score": 0,
    "eros-list": "gambling"
  },
  {
    "score": 11394,
    "eros-list": "pornography"
  }
]
```

## Library

You are welcome to build your own client, and use that. To do so, simply import
`Text.Eros`. Hackage seems to be unable to build the API documentation for Eros,
but it won't hurt to check
[eros on Hackage](http://hackage.haskell.org/package/eros).

If that doesn't work, I publish the documentation
[here](https://pharpend.github.io/eros-haddock).

# Contributing

If you want to contribute, you'll need `ghc` and `cabal-install` 

0.  Clone the git repo

        git clone https://github.com/pharpend/eros.git

1.  Install in a sandbox

        cd eros
        cabal sandbox init
        cabal install --enable-tests
