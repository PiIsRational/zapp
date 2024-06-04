#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 85
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 62
#define ALIAS_COUNT 0
#define TOKEN_COUNT 35
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 6
#define PRODUCTION_ID_COUNT 1

enum ts_symbol_identifiers {
  anon_sym_PERCENT_PERCENTNAME = 1,
  sym_fields = 2,
  sym_top = 3,
  sym_header_end = 4,
  sym_string_char = 5,
  sym_header_inner = 6,
  anon_sym_EQ = 7,
  anon_sym_PIPE = 8,
  anon_sym_SEMI = 9,
  anon_sym_COLON = 10,
  anon_sym_LBRACE = 11,
  anon_sym_RBRACE = 12,
  aux_sym_action_block_token1 = 13,
  sym_action_var = 14,
  anon_sym_AMP = 15,
  anon_sym_BANG = 16,
  anon_sym_STAR = 17,
  anon_sym_PLUS = 18,
  anon_sym_QMARK = 19,
  anon_sym_LPAREN = 20,
  anon_sym_RPAREN = 21,
  anon_sym_DOT = 22,
  anon_sym_AT = 23,
  anon_sym_CARET = 24,
  sym_IDENTIFIER = 25,
  sym_literal = 26,
  anon_sym_LBRACK = 27,
  anon_sym_RBRACK = 28,
  anon_sym_DASH = 29,
  sym_range_char = 30,
  sym_top_level_comment = 31,
  sym_comment = 32,
  sym_line_end = 33,
  sym_space = 34,
  sym_source_file = 35,
  sym_meta_data = 36,
  sym_name = 37,
  sym_top_header = 38,
  sym_field_header = 39,
  sym_header_content = 40,
  sym_definition = 41,
  sym_type_annotation = 42,
  sym_type = 43,
  sym_action_sequence = 44,
  sym_sequence = 45,
  sym_action = 46,
  sym_action_parts = 47,
  sym_action_block = 48,
  sym_operated = 49,
  sym_prefix_op = 50,
  sym_suffix_op = 51,
  sym_primary = 52,
  sym_class = 53,
  sym_range = 54,
  aux_sym_source_file_repeat1 = 55,
  aux_sym_source_file_repeat2 = 56,
  aux_sym_header_content_repeat1 = 57,
  aux_sym_definition_repeat1 = 58,
  aux_sym_sequence_repeat1 = 59,
  aux_sym_action_parts_repeat1 = 60,
  aux_sym_class_repeat1 = 61,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_PERCENT_PERCENTNAME] = "%% NAME",
  [sym_fields] = "fields",
  [sym_top] = "top",
  [sym_header_end] = "header_end",
  [sym_string_char] = "string_char",
  [sym_header_inner] = "header_inner",
  [anon_sym_EQ] = "=",
  [anon_sym_PIPE] = "|",
  [anon_sym_SEMI] = ";",
  [anon_sym_COLON] = ":",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [aux_sym_action_block_token1] = "action_block_token1",
  [sym_action_var] = "action_var",
  [anon_sym_AMP] = "&",
  [anon_sym_BANG] = "!",
  [anon_sym_STAR] = "*",
  [anon_sym_PLUS] = "+",
  [anon_sym_QMARK] = "\?",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DOT] = ".",
  [anon_sym_AT] = "@",
  [anon_sym_CARET] = "^",
  [sym_IDENTIFIER] = "IDENTIFIER",
  [sym_literal] = "literal",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_DASH] = "-",
  [sym_range_char] = "range_char",
  [sym_top_level_comment] = "top_level_comment",
  [sym_comment] = "comment",
  [sym_line_end] = "line_end",
  [sym_space] = "space",
  [sym_source_file] = "source_file",
  [sym_meta_data] = "meta_data",
  [sym_name] = "name",
  [sym_top_header] = "top_header",
  [sym_field_header] = "field_header",
  [sym_header_content] = "header_content",
  [sym_definition] = "definition",
  [sym_type_annotation] = "type_annotation",
  [sym_type] = "type",
  [sym_action_sequence] = "action_sequence",
  [sym_sequence] = "sequence",
  [sym_action] = "action",
  [sym_action_parts] = "action_parts",
  [sym_action_block] = "action_block",
  [sym_operated] = "operated",
  [sym_prefix_op] = "prefix_op",
  [sym_suffix_op] = "suffix_op",
  [sym_primary] = "primary",
  [sym_class] = "class",
  [sym_range] = "range",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_source_file_repeat2] = "source_file_repeat2",
  [aux_sym_header_content_repeat1] = "header_content_repeat1",
  [aux_sym_definition_repeat1] = "definition_repeat1",
  [aux_sym_sequence_repeat1] = "sequence_repeat1",
  [aux_sym_action_parts_repeat1] = "action_parts_repeat1",
  [aux_sym_class_repeat1] = "class_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_PERCENT_PERCENTNAME] = anon_sym_PERCENT_PERCENTNAME,
  [sym_fields] = sym_fields,
  [sym_top] = sym_top,
  [sym_header_end] = sym_header_end,
  [sym_string_char] = sym_string_char,
  [sym_header_inner] = sym_header_inner,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [aux_sym_action_block_token1] = aux_sym_action_block_token1,
  [sym_action_var] = sym_action_var,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_QMARK] = anon_sym_QMARK,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_AT] = anon_sym_AT,
  [anon_sym_CARET] = anon_sym_CARET,
  [sym_IDENTIFIER] = sym_IDENTIFIER,
  [sym_literal] = sym_literal,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_DASH] = anon_sym_DASH,
  [sym_range_char] = sym_range_char,
  [sym_top_level_comment] = sym_top_level_comment,
  [sym_comment] = sym_comment,
  [sym_line_end] = sym_line_end,
  [sym_space] = sym_space,
  [sym_source_file] = sym_source_file,
  [sym_meta_data] = sym_meta_data,
  [sym_name] = sym_name,
  [sym_top_header] = sym_top_header,
  [sym_field_header] = sym_field_header,
  [sym_header_content] = sym_header_content,
  [sym_definition] = sym_definition,
  [sym_type_annotation] = sym_type_annotation,
  [sym_type] = sym_type,
  [sym_action_sequence] = sym_action_sequence,
  [sym_sequence] = sym_sequence,
  [sym_action] = sym_action,
  [sym_action_parts] = sym_action_parts,
  [sym_action_block] = sym_action_block,
  [sym_operated] = sym_operated,
  [sym_prefix_op] = sym_prefix_op,
  [sym_suffix_op] = sym_suffix_op,
  [sym_primary] = sym_primary,
  [sym_class] = sym_class,
  [sym_range] = sym_range,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_source_file_repeat2] = aux_sym_source_file_repeat2,
  [aux_sym_header_content_repeat1] = aux_sym_header_content_repeat1,
  [aux_sym_definition_repeat1] = aux_sym_definition_repeat1,
  [aux_sym_sequence_repeat1] = aux_sym_sequence_repeat1,
  [aux_sym_action_parts_repeat1] = aux_sym_action_parts_repeat1,
  [aux_sym_class_repeat1] = aux_sym_class_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_PERCENT_PERCENTNAME] = {
    .visible = true,
    .named = false,
  },
  [sym_fields] = {
    .visible = true,
    .named = true,
  },
  [sym_top] = {
    .visible = true,
    .named = true,
  },
  [sym_header_end] = {
    .visible = true,
    .named = true,
  },
  [sym_string_char] = {
    .visible = true,
    .named = true,
  },
  [sym_header_inner] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_action_block_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_action_var] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_QMARK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_CARET] = {
    .visible = true,
    .named = false,
  },
  [sym_IDENTIFIER] = {
    .visible = true,
    .named = true,
  },
  [sym_literal] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [sym_range_char] = {
    .visible = true,
    .named = true,
  },
  [sym_top_level_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_line_end] = {
    .visible = true,
    .named = true,
  },
  [sym_space] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_meta_data] = {
    .visible = true,
    .named = true,
  },
  [sym_name] = {
    .visible = true,
    .named = true,
  },
  [sym_top_header] = {
    .visible = true,
    .named = true,
  },
  [sym_field_header] = {
    .visible = true,
    .named = true,
  },
  [sym_header_content] = {
    .visible = true,
    .named = true,
  },
  [sym_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_type_annotation] = {
    .visible = true,
    .named = true,
  },
  [sym_type] = {
    .visible = true,
    .named = true,
  },
  [sym_action_sequence] = {
    .visible = true,
    .named = true,
  },
  [sym_sequence] = {
    .visible = true,
    .named = true,
  },
  [sym_action] = {
    .visible = true,
    .named = true,
  },
  [sym_action_parts] = {
    .visible = true,
    .named = true,
  },
  [sym_action_block] = {
    .visible = true,
    .named = true,
  },
  [sym_operated] = {
    .visible = true,
    .named = true,
  },
  [sym_prefix_op] = {
    .visible = true,
    .named = true,
  },
  [sym_suffix_op] = {
    .visible = true,
    .named = true,
  },
  [sym_primary] = {
    .visible = true,
    .named = true,
  },
  [sym_class] = {
    .visible = true,
    .named = true,
  },
  [sym_range] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_source_file_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_header_content_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_definition_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_sequence_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_action_parts_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_class_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 48,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 69,
  [70] = 70,
  [71] = 71,
  [72] = 72,
  [73] = 73,
  [74] = 74,
  [75] = 75,
  [76] = 76,
  [77] = 77,
  [78] = 78,
  [79] = 79,
  [80] = 80,
  [81] = 81,
  [82] = 82,
  [83] = 83,
  [84] = 84,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(65);
      ADVANCE_MAP(
        '\n', 79,
        '\r', 80,
        '!', 97,
        '"', 113,
        '$', 116,
        '%', 115,
        '&', 96,
        '\'', 114,
        '(', 101,
        ')', 102,
        '*', 98,
        '+', 99,
        '-', 110,
        '.', 103,
        '/', 81,
        ':', 89,
        ';', 88,
        '=', 86,
        '?', 100,
        '@', 104,
        '[', 108,
        '\\', 84,
        ']', 109,
        '^', 105,
        '{', 90,
        '|', 87,
        '}', 91,
        '\t', 79,
        ' ', 79,
      );
      if (('A' <= lookahead && lookahead <= '_') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(85);
      if (lookahead != 0) ADVANCE(79);
      END_STATE();
    case 1:
      ADVANCE_MAP(
        '\n', 79,
        '\r', 80,
        '%', 23,
        '/', 82,
        '\t', 79,
        ' ', 79,
        '"', 49,
        '\'', 49,
      );
      if (lookahead != 0 &&
          lookahead != '$' &&
          lookahead != '%') ADVANCE(79);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(123);
      if (lookahead == '\r') ADVANCE(124);
      if (lookahead == '%') ADVANCE(21);
      if (lookahead == '/') ADVANCE(26);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(125);
      END_STATE();
    case 3:
      ADVANCE_MAP(
        '\n', 111,
        '\r', 112,
        '-', 110,
        '/', 117,
        '\\', 53,
        ']', 109,
        '\t', 111,
        ' ', 111,
      );
      if (lookahead != 0) ADVANCE(111);
      END_STATE();
    case 4:
      if (lookahead == '\n') ADVANCE(111);
      if (lookahead == '\r') ADVANCE(112);
      if (lookahead == '/') ADVANCE(117);
      if (lookahead == '\\') ADVANCE(53);
      if (lookahead == ']') ADVANCE(109);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(111);
      if (lookahead != 0) ADVANCE(111);
      END_STATE();
    case 5:
      if (lookahead == '\n') ADVANCE(71);
      if (lookahead == '\r') ADVANCE(72);
      if (lookahead == ' ') ADVANCE(83);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'') ADVANCE(79);
      END_STATE();
    case 6:
      if (lookahead == '\n') ADVANCE(71);
      if (lookahead == '\r') ADVANCE(72);
      if (lookahead == ' ') ADVANCE(37);
      END_STATE();
    case 7:
      if (lookahead == '\n') ADVANCE(71);
      if (lookahead == '\r') ADVANCE(72);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'') ADVANCE(79);
      END_STATE();
    case 8:
      if (lookahead == '\n') ADVANCE(122);
      if (lookahead == '\r') ADVANCE(122);
      if (lookahead == '*') ADVANCE(11);
      if (lookahead != 0) ADVANCE(10);
      END_STATE();
    case 9:
      if (lookahead == '\n') ADVANCE(122);
      if (lookahead == '\r') ADVANCE(122);
      if (lookahead == '*') ADVANCE(10);
      if (lookahead != 0) ADVANCE(10);
      END_STATE();
    case 10:
      if (lookahead == '\n') ADVANCE(120);
      if (lookahead == '\r') ADVANCE(121);
      if (lookahead != 0) ADVANCE(10);
      END_STATE();
    case 11:
      if (lookahead == '\n') ADVANCE(118);
      if (lookahead == '\r') ADVANCE(119);
      if (lookahead != 0) ADVANCE(11);
      END_STATE();
    case 12:
      if (lookahead == '\n') ADVANCE(69);
      if (lookahead == '\r') ADVANCE(70);
      END_STATE();
    case 13:
      if (lookahead == '\n') ADVANCE(67);
      if (lookahead == '\r') ADVANCE(68);
      END_STATE();
    case 14:
      ADVANCE_MAP(
        '\n', 92,
        '\r', 93,
        '"', 18,
        '$', 25,
        '%', 63,
        '\'', 24,
        '/', 94,
        '{', 90,
        '}', 91,
        '\t', 92,
        ' ', 92,
      );
      if (lookahead != 0) ADVANCE(92);
      END_STATE();
    case 15:
      if (lookahead == ' ') ADVANCE(41);
      END_STATE();
    case 16:
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '\'') ADVANCE(76);
      if (lookahead == '\\') ADVANCE(51);
      if (lookahead != 0) ADVANCE(16);
      END_STATE();
    case 17:
      if (lookahead == '"') ADVANCE(77);
      if (lookahead == '\'') ADVANCE(73);
      if (lookahead == '\\') ADVANCE(52);
      if (lookahead != 0) ADVANCE(17);
      END_STATE();
    case 18:
      if (lookahead == '"') ADVANCE(107);
      if (lookahead == '\\') ADVANCE(54);
      if (lookahead != 0) ADVANCE(18);
      END_STATE();
    case 19:
      if (lookahead == '"') ADVANCE(78);
      if (lookahead == '\'') ADVANCE(74);
      if (lookahead == '\\') ADVANCE(51);
      if (lookahead != 0) ADVANCE(16);
      END_STATE();
    case 20:
      if (lookahead == '"') ADVANCE(75);
      if (lookahead == '\'') ADVANCE(78);
      if (lookahead == '\\') ADVANCE(52);
      if (lookahead != 0) ADVANCE(17);
      END_STATE();
    case 21:
      if (lookahead == '%') ADVANCE(15);
      END_STATE();
    case 22:
      if (lookahead == '%') ADVANCE(6);
      END_STATE();
    case 23:
      if (lookahead == '%') ADVANCE(7);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '%' &&
          lookahead != '\'') ADVANCE(79);
      END_STATE();
    case 24:
      if (lookahead == '\'') ADVANCE(107);
      if (lookahead == '\\') ADVANCE(55);
      if (lookahead != 0) ADVANCE(24);
      END_STATE();
    case 25:
      if (lookahead == '*') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 26:
      if (lookahead == '/') ADVANCE(8);
      END_STATE();
    case 27:
      if (lookahead == '/') ADVANCE(9);
      END_STATE();
    case 28:
      if (lookahead == '0') ADVANCE(57);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(60);
      END_STATE();
    case 29:
      if (lookahead == '0') ADVANCE(45);
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(47);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 30:
      if (lookahead == '0') ADVANCE(58);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(61);
      END_STATE();
    case 31:
      if (lookahead == '0') ADVANCE(46);
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(48);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 32:
      if (lookahead == '0') ADVANCE(59);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(62);
      END_STATE();
    case 33:
      if (lookahead == 'A') ADVANCE(40);
      END_STATE();
    case 34:
      if (lookahead == 'D') ADVANCE(44);
      END_STATE();
    case 35:
      if (lookahead == 'E') ADVANCE(39);
      END_STATE();
    case 36:
      if (lookahead == 'E') ADVANCE(66);
      END_STATE();
    case 37:
      if (lookahead == 'F') ADVANCE(38);
      if (lookahead == 'T') ADVANCE(42);
      END_STATE();
    case 38:
      if (lookahead == 'I') ADVANCE(35);
      END_STATE();
    case 39:
      if (lookahead == 'L') ADVANCE(34);
      END_STATE();
    case 40:
      if (lookahead == 'M') ADVANCE(36);
      END_STATE();
    case 41:
      if (lookahead == 'N') ADVANCE(33);
      END_STATE();
    case 42:
      if (lookahead == 'O') ADVANCE(43);
      END_STATE();
    case 43:
      if (lookahead == 'P') ADVANCE(12);
      END_STATE();
    case 44:
      if (lookahead == 'S') ADVANCE(13);
      END_STATE();
    case 45:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(16);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 46:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(17);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 47:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(16);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 48:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(17);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 49:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 50:
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(78);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 51:
      ADVANCE_MAP(
        '\\', 19,
        'x', 29,
        '"', 74,
        '\'', 74,
        '[', 16,
        ']', 16,
        'n', 16,
        'r', 16,
        't', 16,
      );
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 52:
      ADVANCE_MAP(
        '\\', 20,
        'x', 31,
        '"', 75,
        '\'', 75,
        '[', 17,
        ']', 17,
        'n', 17,
        'r', 17,
        't', 17,
      );
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 53:
      if (lookahead == 'x') ADVANCE(28);
      if (lookahead == '"' ||
          lookahead == '\'' ||
          lookahead == '-' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't') ADVANCE(111);
      END_STATE();
    case 54:
      if (lookahead == 'x') ADVANCE(30);
      if (lookahead == '"' ||
          lookahead == '\'' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't') ADVANCE(18);
      END_STATE();
    case 55:
      if (lookahead == 'x') ADVANCE(32);
      if (lookahead == '"' ||
          lookahead == '\'' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't') ADVANCE(24);
      END_STATE();
    case 56:
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 57:
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(111);
      END_STATE();
    case 58:
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(18);
      END_STATE();
    case 59:
      if (('1' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(24);
      END_STATE();
    case 60:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(111);
      END_STATE();
    case 61:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(18);
      END_STATE();
    case 62:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(24);
      END_STATE();
    case 63:
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '%' &&
          lookahead != '\'' &&
          lookahead != '{' &&
          lookahead != '}') ADVANCE(92);
      END_STATE();
    case 64:
      if (eof) ADVANCE(65);
      ADVANCE_MAP(
        '\n', 123,
        '\r', 124,
        '!', 97,
        '"', 18,
        '%', 22,
        '&', 96,
        '\'', 24,
        '(', 101,
        ')', 102,
        '*', 98,
        '+', 99,
        '.', 103,
        '/', 27,
        ':', 89,
        ';', 88,
        '=', 86,
        '?', 100,
        '@', 104,
        '[', 108,
        '^', 105,
        '{', 90,
        '|', 87,
        '}', 91,
        '\t', 125,
        ' ', 125,
      );
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(106);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_PERCENT_PERCENTNAME);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(sym_fields);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(sym_fields);
      if (lookahead == '\n') ADVANCE(67);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(sym_top);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_top);
      if (lookahead == '\n') ADVANCE(69);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_header_end);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_header_end);
      if (lookahead == '\n') ADVANCE(71);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_string_char);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(sym_string_char);
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '\'') ADVANCE(76);
      if (lookahead == '\\') ADVANCE(51);
      if (lookahead != 0) ADVANCE(16);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_string_char);
      if (lookahead == '"') ADVANCE(77);
      if (lookahead == '\'') ADVANCE(73);
      if (lookahead == '\\') ADVANCE(52);
      if (lookahead != 0) ADVANCE(17);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_string_char);
      if (lookahead == '"') ADVANCE(107);
      if (lookahead == '\\') ADVANCE(54);
      if (lookahead != 0) ADVANCE(18);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_string_char);
      if (lookahead == '\'') ADVANCE(107);
      if (lookahead == '\\') ADVANCE(55);
      if (lookahead != 0) ADVANCE(24);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_string_char);
      if (lookahead == '\\') ADVANCE(50);
      if (lookahead == '"' ||
          lookahead == '\'') ADVANCE(73);
      if (lookahead != 0) ADVANCE(49);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_header_inner);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_header_inner);
      if (lookahead == '\n') ADVANCE(123);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_header_inner);
      if (lookahead == '/') ADVANCE(8);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(sym_header_inner);
      if (lookahead == '/') ADVANCE(9);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(sym_header_inner);
      if (lookahead == 'F') ADVANCE(38);
      if (lookahead == 'N') ADVANCE(33);
      if (lookahead == 'T') ADVANCE(42);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(sym_header_inner);
      if (lookahead == 'x') ADVANCE(28);
      if (lookahead == '"' ||
          lookahead == '\'' ||
          lookahead == '-' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == 'n' ||
          lookahead == 'r' ||
          lookahead == 't') ADVANCE(111);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_header_inner);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(106);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(aux_sym_action_block_token1);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(aux_sym_action_block_token1);
      if (lookahead == '\n') ADVANCE(123);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(aux_sym_action_block_token1);
      if (lookahead == '/') ADVANCE(9);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_action_var);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(anon_sym_QMARK);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_IDENTIFIER);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(106);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_literal);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_range_char);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '\n') ADVANCE(123);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '\'') ADVANCE(76);
      if (lookahead == '\\') ADVANCE(51);
      if (lookahead != 0) ADVANCE(16);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '"') ADVANCE(77);
      if (lookahead == '\'') ADVANCE(73);
      if (lookahead == '\\') ADVANCE(52);
      if (lookahead != 0) ADVANCE(17);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '%') ADVANCE(5);
      if (lookahead == '{' ||
          lookahead == '}') ADVANCE(79);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '%' &&
          lookahead != '\'') ADVANCE(79);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '*') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(95);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_range_char);
      if (lookahead == '/') ADVANCE(9);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_top_level_comment);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_top_level_comment);
      if (lookahead == '\n') ADVANCE(118);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_comment);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\n') ADVANCE(120);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\n') ADVANCE(120);
      if (lookahead == '\r') ADVANCE(121);
      if (lookahead != 0) ADVANCE(10);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(sym_line_end);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(sym_line_end);
      if (lookahead == '\n') ADVANCE(123);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(sym_space);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 2},
  [2] = {.lex_state = 64},
  [3] = {.lex_state = 64},
  [4] = {.lex_state = 64},
  [5] = {.lex_state = 64},
  [6] = {.lex_state = 64},
  [7] = {.lex_state = 64},
  [8] = {.lex_state = 64},
  [9] = {.lex_state = 64},
  [10] = {.lex_state = 64},
  [11] = {.lex_state = 64},
  [12] = {.lex_state = 64},
  [13] = {.lex_state = 64},
  [14] = {.lex_state = 64},
  [15] = {.lex_state = 64},
  [16] = {.lex_state = 64},
  [17] = {.lex_state = 64},
  [18] = {.lex_state = 14},
  [19] = {.lex_state = 14},
  [20] = {.lex_state = 14},
  [21] = {.lex_state = 64},
  [22] = {.lex_state = 14},
  [23] = {.lex_state = 14},
  [24] = {.lex_state = 14},
  [25] = {.lex_state = 64},
  [26] = {.lex_state = 64},
  [27] = {.lex_state = 14},
  [28] = {.lex_state = 14},
  [29] = {.lex_state = 1},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 14},
  [32] = {.lex_state = 64},
  [33] = {.lex_state = 64},
  [34] = {.lex_state = 1},
  [35] = {.lex_state = 4},
  [36] = {.lex_state = 64},
  [37] = {.lex_state = 64},
  [38] = {.lex_state = 4},
  [39] = {.lex_state = 64},
  [40] = {.lex_state = 2},
  [41] = {.lex_state = 4},
  [42] = {.lex_state = 1},
  [43] = {.lex_state = 64},
  [44] = {.lex_state = 64},
  [45] = {.lex_state = 64},
  [46] = {.lex_state = 3},
  [47] = {.lex_state = 64},
  [48] = {.lex_state = 64},
  [49] = {.lex_state = 64},
  [50] = {.lex_state = 64},
  [51] = {.lex_state = 64},
  [52] = {.lex_state = 64},
  [53] = {.lex_state = 2},
  [54] = {.lex_state = 64},
  [55] = {.lex_state = 64},
  [56] = {.lex_state = 64},
  [57] = {.lex_state = 64},
  [58] = {.lex_state = 64},
  [59] = {.lex_state = 64},
  [60] = {.lex_state = 64},
  [61] = {.lex_state = 64},
  [62] = {.lex_state = 64},
  [63] = {.lex_state = 64},
  [64] = {.lex_state = 4},
  [65] = {.lex_state = 64},
  [66] = {.lex_state = 64},
  [67] = {.lex_state = 64},
  [68] = {.lex_state = 64},
  [69] = {.lex_state = 64},
  [70] = {.lex_state = 64},
  [71] = {.lex_state = 64},
  [72] = {.lex_state = 64},
  [73] = {.lex_state = 64},
  [74] = {.lex_state = 64},
  [75] = {.lex_state = 4},
  [76] = {.lex_state = 64},
  [77] = {.lex_state = 64},
  [78] = {.lex_state = 64},
  [79] = {.lex_state = 64},
  [80] = {.lex_state = 64},
  [81] = {.lex_state = 64},
  [82] = {.lex_state = 64},
  [83] = {.lex_state = 64},
  [84] = {.lex_state = 64},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_PERCENT_PERCENTNAME] = ACTIONS(1),
    [sym_fields] = ACTIONS(1),
    [sym_top] = ACTIONS(1),
    [sym_header_end] = ACTIONS(1),
    [sym_string_char] = ACTIONS(1),
    [sym_header_inner] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [aux_sym_action_block_token1] = ACTIONS(1),
    [sym_action_var] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_QMARK] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
    [anon_sym_CARET] = ACTIONS(1),
    [sym_IDENTIFIER] = ACTIONS(1),
    [sym_literal] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [sym_range_char] = ACTIONS(1),
    [sym_top_level_comment] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [sym_line_end] = ACTIONS(3),
    [sym_space] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(83),
    [sym_name] = STATE(21),
    [aux_sym_source_file_repeat1] = STATE(40),
    [anon_sym_PERCENT_PERCENTNAME] = ACTIONS(5),
    [sym_top_level_comment] = ACTIONS(7),
    [sym_comment] = ACTIONS(3),
    [sym_line_end] = ACTIONS(9),
    [sym_space] = ACTIONS(9),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 10,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    ACTIONS(13), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(3), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(11), 4,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RPAREN,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [42] = 10,
    ACTIONS(26), 1,
      anon_sym_LPAREN,
    ACTIONS(32), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    ACTIONS(23), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(3), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(21), 4,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_RPAREN,
    ACTIONS(29), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [84] = 4,
    STATE(16), 1,
      sym_suffix_op,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(37), 3,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
    ACTIONS(35), 13,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [113] = 4,
    STATE(14), 1,
      sym_suffix_op,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(37), 3,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
    ACTIONS(39), 13,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [142] = 11,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    STATE(39), 1,
      sym_sequence,
    STATE(51), 1,
      sym_action_sequence,
    ACTIONS(13), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(2), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [184] = 11,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    STATE(39), 1,
      sym_sequence,
    STATE(44), 1,
      sym_action_sequence,
    ACTIONS(13), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(2), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [226] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(41), 16,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [250] = 11,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    STATE(39), 1,
      sym_sequence,
    STATE(62), 1,
      sym_action_sequence,
    ACTIONS(13), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(2), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [292] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(43), 16,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [316] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(45), 16,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [340] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(47), 16,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_STAR,
      anon_sym_PLUS,
      anon_sym_QMARK,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [364] = 10,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(4), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    STATE(17), 1,
      sym_prefix_op,
    STATE(69), 1,
      sym_sequence,
    ACTIONS(13), 2,
      anon_sym_AMP,
      anon_sym_BANG,
    STATE(2), 2,
      sym_operated,
      aux_sym_sequence_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [403] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(49), 13,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [424] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(51), 13,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [445] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(39), 13,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_LBRACE,
      anon_sym_AMP,
      anon_sym_BANG,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [466] = 6,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_LBRACK,
    STATE(5), 1,
      sym_primary,
    STATE(10), 1,
      sym_class,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(17), 5,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
  [491] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(55), 1,
      anon_sym_RBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    STATE(79), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [519] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(61), 1,
      anon_sym_RBRACE,
    STATE(70), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [547] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(63), 1,
      anon_sym_RBRACE,
    STATE(82), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [575] = 8,
    ACTIONS(65), 1,
      sym_fields,
    ACTIONS(67), 1,
      sym_top,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    STATE(48), 1,
      sym_meta_data,
    STATE(49), 1,
      sym_top_header,
    STATE(68), 1,
      sym_field_header,
    STATE(33), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [603] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(71), 1,
      anon_sym_RBRACE,
    STATE(84), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [631] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(73), 1,
      anon_sym_RBRACE,
    STATE(81), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [659] = 8,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(75), 1,
      anon_sym_RBRACE,
    STATE(74), 1,
      sym_action_parts,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(27), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [687] = 8,
    ACTIONS(65), 1,
      sym_fields,
    ACTIONS(67), 1,
      sym_top,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    STATE(49), 1,
      sym_top_header,
    STATE(55), 1,
      sym_meta_data,
    STATE(68), 1,
      sym_field_header,
    STATE(37), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [715] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(77), 7,
      anon_sym_LPAREN,
      anon_sym_DOT,
      anon_sym_AT,
      anon_sym_CARET,
      sym_IDENTIFIER,
      sym_literal,
      anon_sym_LBRACK,
  [730] = 7,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(57), 1,
      aux_sym_action_block_token1,
    ACTIONS(79), 1,
      anon_sym_LBRACE,
    ACTIONS(81), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(59), 2,
      sym_action_var,
      sym_literal,
    STATE(28), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [755] = 6,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(85), 1,
      aux_sym_action_block_token1,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(83), 2,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
    ACTIONS(88), 2,
      sym_action_var,
      sym_literal,
    STATE(28), 2,
      sym_action_block,
      aux_sym_action_parts_repeat1,
  [778] = 7,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(91), 1,
      sym_header_end,
    ACTIONS(93), 1,
      sym_string_char,
    ACTIONS(95), 1,
      sym_header_inner,
    STATE(42), 1,
      aux_sym_header_content_repeat1,
    STATE(73), 1,
      sym_header_content,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [801] = 7,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(93), 1,
      sym_string_char,
    ACTIONS(95), 1,
      sym_header_inner,
    ACTIONS(97), 1,
      sym_header_end,
    STATE(42), 1,
      aux_sym_header_content_repeat1,
    STATE(71), 1,
      sym_header_content,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [824] = 4,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(101), 1,
      aux_sym_action_block_token1,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    ACTIONS(99), 4,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      sym_action_var,
      sym_literal,
  [841] = 4,
    ACTIONS(103), 1,
      ts_builtin_sym_end,
    ACTIONS(105), 1,
      sym_IDENTIFIER,
    STATE(32), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [857] = 4,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    ACTIONS(108), 1,
      ts_builtin_sym_end,
    STATE(32), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [873] = 6,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(110), 1,
      sym_header_end,
    ACTIONS(112), 1,
      sym_string_char,
    ACTIONS(115), 1,
      sym_header_inner,
    STATE(34), 1,
      aux_sym_header_content_repeat1,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [893] = 5,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(118), 1,
      anon_sym_RBRACK,
    ACTIONS(120), 1,
      sym_range_char,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    STATE(35), 2,
      sym_range,
      aux_sym_class_repeat1,
  [911] = 4,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    ACTIONS(123), 1,
      ts_builtin_sym_end,
    STATE(32), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [927] = 4,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    ACTIONS(125), 1,
      ts_builtin_sym_end,
    STATE(32), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [943] = 5,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(127), 1,
      anon_sym_RBRACK,
    ACTIONS(129), 1,
      sym_range_char,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    STATE(41), 2,
      sym_range,
      aux_sym_class_repeat1,
  [961] = 4,
    ACTIONS(133), 1,
      anon_sym_LBRACE,
    STATE(60), 1,
      sym_action,
    ACTIONS(131), 2,
      anon_sym_PIPE,
      anon_sym_SEMI,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [977] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      anon_sym_PERCENT_PERCENTNAME,
    ACTIONS(135), 1,
      sym_top_level_comment,
    STATE(25), 1,
      sym_name,
    STATE(53), 1,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 2,
      sym_line_end,
      sym_space,
  [997] = 5,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(129), 1,
      sym_range_char,
    ACTIONS(137), 1,
      anon_sym_RBRACK,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
    STATE(35), 2,
      sym_range,
      aux_sym_class_repeat1,
  [1015] = 6,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(139), 1,
      sym_header_end,
    ACTIONS(141), 1,
      sym_string_char,
    ACTIONS(143), 1,
      sym_header_inner,
    STATE(34), 1,
      aux_sym_header_content_repeat1,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [1035] = 3,
    STATE(80), 1,
      sym_type,
    ACTIONS(145), 2,
      sym_IDENTIFIER,
      sym_literal,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1048] = 4,
    ACTIONS(147), 1,
      anon_sym_PIPE,
    ACTIONS(149), 1,
      anon_sym_SEMI,
    STATE(52), 1,
      aux_sym_definition_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1063] = 2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
    ACTIONS(151), 3,
      sym_fields,
      sym_top,
      sym_IDENTIFIER,
  [1074] = 5,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(153), 1,
      anon_sym_RBRACK,
    ACTIONS(155), 1,
      anon_sym_DASH,
    ACTIONS(157), 1,
      sym_range_char,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [1091] = 4,
    ACTIONS(159), 1,
      anon_sym_EQ,
    ACTIONS(161), 1,
      anon_sym_COLON,
    STATE(76), 1,
      sym_type_annotation,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1106] = 3,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    STATE(37), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1119] = 4,
    ACTIONS(65), 1,
      sym_fields,
    ACTIONS(163), 1,
      sym_IDENTIFIER,
    STATE(78), 1,
      sym_field_header,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1134] = 4,
    ACTIONS(147), 1,
      anon_sym_PIPE,
    ACTIONS(149), 1,
      anon_sym_SEMI,
    STATE(54), 1,
      aux_sym_definition_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1149] = 4,
    ACTIONS(147), 1,
      anon_sym_PIPE,
    ACTIONS(165), 1,
      anon_sym_SEMI,
    STATE(50), 1,
      aux_sym_definition_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1164] = 4,
    ACTIONS(147), 1,
      anon_sym_PIPE,
    ACTIONS(167), 1,
      anon_sym_SEMI,
    STATE(54), 1,
      aux_sym_definition_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1179] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(169), 1,
      anon_sym_PERCENT_PERCENTNAME,
    ACTIONS(171), 1,
      sym_top_level_comment,
    STATE(53), 1,
      aux_sym_source_file_repeat1,
    ACTIONS(9), 2,
      sym_line_end,
      sym_space,
  [1196] = 4,
    ACTIONS(174), 1,
      anon_sym_PIPE,
    ACTIONS(177), 1,
      anon_sym_SEMI,
    STATE(54), 1,
      aux_sym_definition_repeat1,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1211] = 3,
    ACTIONS(69), 1,
      sym_IDENTIFIER,
    STATE(36), 2,
      sym_definition,
      aux_sym_source_file_repeat2,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1224] = 2,
    ACTIONS(179), 2,
      sym_top,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1234] = 2,
    ACTIONS(181), 2,
      ts_builtin_sym_end,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1244] = 2,
    ACTIONS(183), 2,
      ts_builtin_sym_end,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1254] = 2,
    ACTIONS(185), 2,
      anon_sym_PIPE,
      anon_sym_SEMI,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1264] = 2,
    ACTIONS(187), 2,
      anon_sym_PIPE,
      anon_sym_SEMI,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1274] = 2,
    ACTIONS(189), 2,
      sym_fields,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1284] = 2,
    ACTIONS(177), 2,
      anon_sym_PIPE,
      anon_sym_SEMI,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1294] = 2,
    ACTIONS(191), 2,
      ts_builtin_sym_end,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1304] = 4,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(193), 1,
      anon_sym_RBRACK,
    ACTIONS(195), 1,
      sym_range_char,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [1318] = 2,
    ACTIONS(197), 2,
      sym_fields,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1328] = 2,
    ACTIONS(199), 2,
      anon_sym_PIPE,
      anon_sym_SEMI,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1338] = 2,
    ACTIONS(201), 2,
      sym_top,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1348] = 3,
    ACTIONS(67), 1,
      sym_top,
    STATE(78), 1,
      sym_top_header,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1360] = 2,
    ACTIONS(203), 1,
      anon_sym_RPAREN,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1369] = 2,
    ACTIONS(205), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1378] = 2,
    ACTIONS(207), 1,
      sym_header_end,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1387] = 2,
    ACTIONS(209), 1,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1396] = 2,
    ACTIONS(211), 1,
      sym_header_end,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1405] = 2,
    ACTIONS(73), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1414] = 3,
    ACTIONS(9), 1,
      sym_comment,
    ACTIONS(213), 1,
      sym_range_char,
    ACTIONS(3), 2,
      sym_line_end,
      sym_space,
  [1425] = 2,
    ACTIONS(215), 1,
      anon_sym_EQ,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1434] = 2,
    ACTIONS(217), 1,
      anon_sym_EQ,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1443] = 2,
    ACTIONS(219), 1,
      sym_IDENTIFIER,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1452] = 2,
    ACTIONS(63), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1461] = 2,
    ACTIONS(221), 1,
      anon_sym_EQ,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1470] = 2,
    ACTIONS(223), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1479] = 2,
    ACTIONS(71), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1488] = 2,
    ACTIONS(225), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
  [1497] = 2,
    ACTIONS(227), 1,
      anon_sym_RBRACE,
    ACTIONS(9), 3,
      sym_comment,
      sym_line_end,
      sym_space,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 42,
  [SMALL_STATE(4)] = 84,
  [SMALL_STATE(5)] = 113,
  [SMALL_STATE(6)] = 142,
  [SMALL_STATE(7)] = 184,
  [SMALL_STATE(8)] = 226,
  [SMALL_STATE(9)] = 250,
  [SMALL_STATE(10)] = 292,
  [SMALL_STATE(11)] = 316,
  [SMALL_STATE(12)] = 340,
  [SMALL_STATE(13)] = 364,
  [SMALL_STATE(14)] = 403,
  [SMALL_STATE(15)] = 424,
  [SMALL_STATE(16)] = 445,
  [SMALL_STATE(17)] = 466,
  [SMALL_STATE(18)] = 491,
  [SMALL_STATE(19)] = 519,
  [SMALL_STATE(20)] = 547,
  [SMALL_STATE(21)] = 575,
  [SMALL_STATE(22)] = 603,
  [SMALL_STATE(23)] = 631,
  [SMALL_STATE(24)] = 659,
  [SMALL_STATE(25)] = 687,
  [SMALL_STATE(26)] = 715,
  [SMALL_STATE(27)] = 730,
  [SMALL_STATE(28)] = 755,
  [SMALL_STATE(29)] = 778,
  [SMALL_STATE(30)] = 801,
  [SMALL_STATE(31)] = 824,
  [SMALL_STATE(32)] = 841,
  [SMALL_STATE(33)] = 857,
  [SMALL_STATE(34)] = 873,
  [SMALL_STATE(35)] = 893,
  [SMALL_STATE(36)] = 911,
  [SMALL_STATE(37)] = 927,
  [SMALL_STATE(38)] = 943,
  [SMALL_STATE(39)] = 961,
  [SMALL_STATE(40)] = 977,
  [SMALL_STATE(41)] = 997,
  [SMALL_STATE(42)] = 1015,
  [SMALL_STATE(43)] = 1035,
  [SMALL_STATE(44)] = 1048,
  [SMALL_STATE(45)] = 1063,
  [SMALL_STATE(46)] = 1074,
  [SMALL_STATE(47)] = 1091,
  [SMALL_STATE(48)] = 1106,
  [SMALL_STATE(49)] = 1119,
  [SMALL_STATE(50)] = 1134,
  [SMALL_STATE(51)] = 1149,
  [SMALL_STATE(52)] = 1164,
  [SMALL_STATE(53)] = 1179,
  [SMALL_STATE(54)] = 1196,
  [SMALL_STATE(55)] = 1211,
  [SMALL_STATE(56)] = 1224,
  [SMALL_STATE(57)] = 1234,
  [SMALL_STATE(58)] = 1244,
  [SMALL_STATE(59)] = 1254,
  [SMALL_STATE(60)] = 1264,
  [SMALL_STATE(61)] = 1274,
  [SMALL_STATE(62)] = 1284,
  [SMALL_STATE(63)] = 1294,
  [SMALL_STATE(64)] = 1304,
  [SMALL_STATE(65)] = 1318,
  [SMALL_STATE(66)] = 1328,
  [SMALL_STATE(67)] = 1338,
  [SMALL_STATE(68)] = 1348,
  [SMALL_STATE(69)] = 1360,
  [SMALL_STATE(70)] = 1369,
  [SMALL_STATE(71)] = 1378,
  [SMALL_STATE(72)] = 1387,
  [SMALL_STATE(73)] = 1396,
  [SMALL_STATE(74)] = 1405,
  [SMALL_STATE(75)] = 1414,
  [SMALL_STATE(76)] = 1425,
  [SMALL_STATE(77)] = 1434,
  [SMALL_STATE(78)] = 1443,
  [SMALL_STATE(79)] = 1452,
  [SMALL_STATE(80)] = 1461,
  [SMALL_STATE(81)] = 1470,
  [SMALL_STATE(82)] = 1479,
  [SMALL_STATE(83)] = 1488,
  [SMALL_STATE(84)] = 1497,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sequence, 1, 0, 0),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_sequence_repeat1, 2, 0, 0),
  [23] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_sequence_repeat1, 2, 0, 0), SHIFT_REPEAT(26),
  [26] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_sequence_repeat1, 2, 0, 0), SHIFT_REPEAT(13),
  [29] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_sequence_repeat1, 2, 0, 0), SHIFT_REPEAT(10),
  [32] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_sequence_repeat1, 2, 0, 0), SHIFT_REPEAT(38),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operated, 1, 0, 0),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operated, 2, 0, 0),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_class, 2, 0, 0),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary, 1, 0, 0),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_class, 3, 0, 0),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primary, 3, 0, 0),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operated, 3, 0, 0),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_suffix_op, 1, 0, 0),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_parts, 2, 0, 0),
  [57] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [63] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_parts, 3, 0, 0),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_parts, 4, 0, 0),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_prefix_op, 1, 0, 0),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_parts, 1, 0, 0),
  [83] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_action_parts_repeat1, 2, 0, 0),
  [85] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_action_parts_repeat1, 2, 0, 0), SHIFT_REPEAT(31),
  [88] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_action_parts_repeat1, 2, 0, 0), SHIFT_REPEAT(31),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [99] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_block, 1, 0, 0),
  [101] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_action_block, 1, 0, 0),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2, 0, 0),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat2, 2, 0, 0), SHIFT_REPEAT(47),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2, 0, 0),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_header_content_repeat1, 2, 0, 0),
  [112] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_header_content_repeat1, 2, 0, 0), SHIFT_REPEAT(34),
  [115] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_header_content_repeat1, 2, 0, 0), SHIFT_REPEAT(34),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_class_repeat1, 2, 0, 0),
  [120] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_class_repeat1, 2, 0, 0), SHIFT_REPEAT(46),
  [123] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 4, 0, 0),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 3, 0, 0),
  [127] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [129] = {.entry = {.count = 1, .reusable = false}}, SHIFT(46),
  [131] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_sequence, 1, 0, 0),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [137] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [139] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_header_content, 1, 0, 0),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [143] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [145] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [147] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [149] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [151] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_name, 2, 0, 0),
  [153] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_range, 1, 0, 0),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [157] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_range, 1, 0, 0),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [163] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_meta_data, 1, 0, 0),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [167] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [169] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [171] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(53),
  [174] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_definition_repeat1, 2, 0, 0), SHIFT_REPEAT(9),
  [177] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_definition_repeat1, 2, 0, 0),
  [179] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field_header, 3, 0, 0),
  [181] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 6, 0, 0),
  [183] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 4, 0, 0),
  [185] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action, 3, 0, 0),
  [187] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_sequence, 2, 0, 0),
  [189] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_top_header, 2, 0, 0),
  [191] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 5, 0, 0),
  [193] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_range, 3, 0, 0),
  [195] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_range, 3, 0, 0),
  [197] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_top_header, 3, 0, 0),
  [199] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action, 2, 0, 0),
  [201] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_field_header, 2, 0, 0),
  [203] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [211] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [213] = {.entry = {.count = 1, .reusable = false}}, SHIFT(64),
  [215] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [217] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1, 0, 0),
  [219] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_meta_data, 2, 0, 0),
  [221] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_annotation, 2, 0, 0),
  [223] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [225] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [227] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_action_parts, 5, 0, 0),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_zapp(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
