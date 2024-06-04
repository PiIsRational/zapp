module.exports = grammar({
  name: 'zapp',
  rules: {
    source_file: $ => seq(
      repeat(seq($.top_level_comment)),
      $.name,
      optional($.meta_data),
      repeat1($.definition),
    ),

    meta_data: $ => choice(
      seq($.top_header, optional($.field_header)),
      seq(optional($.field_header), $.top_header),
    ),

    name: $ => seq('%% NAME', $.IDENTIFIER),

    fields: $ => token(seq('%% FIELDS', /\r\n|\r|\n/)),
    top: $ => token(seq('%% TOP', /\r\n|\r|\n/)),

    top_header: $ => seq(
      $.top,
      optional($.header_content),
      $.header_end,
    ),

    field_header: $ => seq(
      $.fields,
      optional($.header_content),
      $.header_end,
    ),

    header_content: $ => repeat1(choice(
      $.header_inner,
      $.string_char,
    )),

    header_end: $ => token(seq('%%', /\r\n|\r|\n/)),

    string_char: $ => /["']([^"']|\\"|\\')*['"]/,

    header_inner: $ => token(choice(
      /%?[^%$"']/,
      /%%[^\r\n$"']/,
    )),

    definition: $ => seq(
      $.IDENTIFIER,
      optional($.type_annotation),
      '=',
      $.action_sequence,
      repeat(seq('|', $.action_sequence)),
      ';',
    ),

    type_annotation: $ => seq(':', $.type),

    type: $ => choice(
      $.IDENTIFIER,
      $.literal,
    ),

    action_sequence: $ => seq($.sequence, optional($.action)),

    sequence: $ => repeat1($.operated),

    // action start
    action: $ => seq('{', optional($.action_parts), '}'),

    action_parts: $ => choice(
      seq(
        repeat($.action_block),
        '{',
        optional($.action_parts),
        '}',
        optional($.action_parts),
      ),
      repeat1($.action_block),
    ),

    action_block: $ => choice(
      $.action_var,
      /%?[^"'{}$%]/,
      $.literal,
    ),

    action_var: $ => token(choice(
      /\$[0-9]+/,
      /\$\*[1-9][0-9]*/,
    )),

    // End of actions

    operated: $ => seq(optional($.prefix_op), $.primary, optional($.suffix_op)),

    prefix_op: $ => choice(
      '&',
      '!',
    ),

    suffix_op: $ => choice(
      '*',
      '+',
      '?',
    ),

    primary: $ => choice(
      seq('(', $.sequence, ')'),
      $.IDENTIFIER,
      $.literal,
      $.class,
      '.',
      '@',
      '^',
    ),

    // lexical syntax
    IDENTIFIER: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    literal: $ => token(choice(
      seq('\'', repeat(choice(
        /\\[nrt'"\[\]\\]/,
        /\\x[1-9A-Fa-f][0-9A-Fa-f]/,
        /\\x0[1-9A-Fa-f]/,
        /[^\\']/,
      )), '\''),

      seq('"', repeat(choice(
        /\\[nrt'"\[\]\\]/,
        /\\x[1-9A-Fa-f][0-9A-Fa-f]/,
        /\\x0[1-9A-Fa-f]/,
        /[^\\"]/,
      )),'"'),
    )),

    class: $ => seq('[', repeat($.range),']'),

    range: $ => choice(
      seq($.range_char, '-', $.range_char),
      $.range_char,
    ),

    range_char: $ => token(choice(
      /\\[nrt'\"\[\]\\\-]/,
      /\\x[1-9A-Fa-f][0-9A-Fa-f]/,
      /\\x0[1-9A-Fa-f]/,
      /[^\\\]]/,
    )),

    top_level_comment: $ => token(seq('//*', /[^\r\n]*/, /\r\n|\r|\n/)),
    comment: $ => token(seq('//', /[^*]?/, /[^\r\n]*/, /\r\n|\r|\n/)),

    line_end: $ => token(choice(
      '\r\n',
      '\r',
      '\n',
    )),

    space: $ => token(choice(
      ' ',
      '\t',
    )),
  },

  extras: $ => [
    $.line_end,
    $.space,
    $.comment,
  ],
});
