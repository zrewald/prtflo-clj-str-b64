# prtflo-clj-str-b64

## Warning!

This library is not meant to be used in actual projects.  If you're looking for a Base64 encoder/decoder library, you'll need to look elsewhere.

## About

This is a simple portfolio project implementing (what I considered to be) an interesting algorithm.  Thanks for checking it out!

This library can currently encode only ASCII text, and makes no attempt at correctly handling unicode.  

## Installation

There is no provided jar for this project.  In order to run it, it will need to be cloned and worked with via REPL.

1.  Check out the project using whatever Git tool.
2.  `lein repl` within the project's folder
3.  Assuming you're in the `prtflo-clj-str-b64` namespace, you should be able to use the `encode` and `decode` functions.

## Usage

### Encoding

Limitations
- Input must be an ASCII-only string

```
(encode "Your input here")
```

### Decoding

Limitations
- Input must be a Base64-encoded string

```
(decode "WW91ciBpbnB1dCBoZXJl")
```