# Changelog

## [Unreleased]

## [2.0.0]

### Fixed

- Disallow tag label in flat encoded values.
  `VariantEncodedFlat "kind" ( case1 :: { kind :: Int }, case2 :: {} )` is not possible anymore.

- Value types in flat encodings are Records now instead of Rows.
  Simplifies the API and is equally safe.

### Changed

- Improve API function names

## [1.0.0]

### Added

- Initial implementation

[unreleased]: https://github.com/thought2/purescript-ts-bridge/compare/v1.0.0...HEAD
[2.0.0]: https://github.com/thought2/purescript-ts-bridge/compare/v1.0.0...v2.0.0
[1.0.0]: https://github.com/thought2/purescript-ts-bridge/releases/tag/v1.0.0
