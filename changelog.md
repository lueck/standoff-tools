# Changes #

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

