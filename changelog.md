# Changes #

- 0.1.1.2
  - parse external markup from CSV with several referencing methods:
  - referencing by start character offset and end character offset,
	by pairs of line and column and by a pair of line and column for
	the start position and a length for the end position.

- 0.1.1.1
  - introduce `GenericMarkup` type and `ExternalMarkup` GADT
  - it was first planned to use the GADT in the cli, but using
    `GenericMarkup` introduces less overhead

- 0.1.1.0
  - introduced special attribute `__standoff_special__ns` for adding a
    namespace on an inserted element.

- 0.1.0.1
  - deprecated module `StandOff.TagTypeDefs` because the type
    definitions have gone to `StandOff.Tag`

- 0.1.0.0
  - parse and internalize JSON annotations produced with standoff-mode
  - introduced attribute maps represented in YAML config files
  - removed the `dumped` command from cli
  - deprecated module `StandOff.AnnotationTypeDefs` - we better write
    a type on its own for each format of external markup
  - deprecated modules `StandOff.TagSerializer` and
    `StandOff.AttributeSerializer` because we have the greatly
    improved attributes map now
  - deactivated real world tests
  - removed tests for the parser combinators for elisp s-expression
    dumps because only the overall parser is still exported by the
    module

