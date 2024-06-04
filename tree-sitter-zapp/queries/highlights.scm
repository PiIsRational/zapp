[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  (prefix_op)
  (suffix_op)
  "^"
  "="
] @operator

[
  (literal)
] @string

(type[
  (IDENTIFIER)
  (literal)
] @type)

[
  (comment)
  (top_level_comment)
] @comment

[
  ";"
  ":"
  "|"
] @punctuation.delimiter

[
  (fields)
  (top)
  (header_end)
  "%% NAME"
] @keyword

[
  "@"
  "."
] @constant.builtin

(range_char) @character

(range [
  "-" @operator
])

(definition 
  (IDENTIFIER) @field
  (#match? @field "^[A-Z_a-z][A-Z_0-9a-z]*$")
)

(primary
  (IDENTIFIER) @constant
  (#match? @constant "^[A-Z_][A-Z_0-9]*$")
)

(definition 
  (IDENTIFIER) @constant
  (#match? @constant "^[A-Z_][A-Z_0-9]*$")
)
