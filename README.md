# variant-encodings

Variant types are great to FFI JavaScript tagged union types. But the encoding
on the JavaScript side does not always match exactly the one from Variant (`{
type: ..., value: ...}`)

This library provides types that describe variants with custom encodings: Tag
and value keys can be configured. Also a flat encoding is available.

The types are merely meant as an intermediate step. There will be no utility
functions provided. However, there are functions provided that convert to and from `Variant`.
