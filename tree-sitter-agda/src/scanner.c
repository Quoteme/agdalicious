// External scanner for tree-sitter-agda: Agda's layout rule and comments.
//
// Agda opens an indentation block after a layout keyword (where, postulate,
// field, let, do, private, ...). The column of the first token after the
// keyword is the block's column. A later line starting at that column starts
// a new item in the block (virtual `;`), a line starting left of it closes
// the block (virtual `}`).
//
// This scanner keeps a stack of block columns and emits zero-width
// _layout_start / _layout_semicolon / _layout_end tokens. It follows
// Agda.Syntax.Parser.Layout, simplified:
//
//  * A block opened after a keyword whose first token is on the same line is
//    "tentative". If the first token is on a new line it is "confirmed".
//    A new block must be strictly right of the innermost confirmed block,
//    otherwise it is empty (`module M where` followed by a less-indented
//    line).
//  * A block also closes, without a newline, before `)`, `}`, `⦄`, `⦈`, and
//    a `let` block closes before `in`.
//
// Comments (`--` to end of line, nested `{- -}`) are scanned here too, so
// that they never influence layout.

#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  LAYOUT_START,
  LET_START,
  LAYOUT_SEMICOLON,
  LAYOUT_END,
  COMMENT,
  ERROR_SENTINEL,
};

typedef struct {
  uint16_t column;
  bool confirmed;
  bool let;
} Block;

#define MAX_DEPTH 255

typedef struct {
  Block blocks[MAX_DEPTH];
  uint8_t depth;
  // The whitespace before the last external token contained a newline.
  // The next scan call starts after that token (layout tokens are
  // zero-width but the whitespace before them is consumed), so it cannot
  // see that newline itself. The state outlives the next scan call, though:
  // it is restored after every internal token too, until the next external
  // one. So the flag only counts if the scan starts at newline_column, the
  // column right after the external token.
  bool newline_pending;
  uint16_t newline_column;
  // The `in` at newline_column has closed its `let` block already (with the
  // blocks inside it); it must not close enclosing blocks too.
  bool in_done;
} Scanner;

void *tree_sitter_agda_external_scanner_create(void) {
  return calloc(1, sizeof(Scanner));
}

void tree_sitter_agda_external_scanner_destroy(void *payload) { free(payload); }

unsigned tree_sitter_agda_external_scanner_serialize(void *payload,
                                                     char *buffer) {
  Scanner *s = payload;
  unsigned n = 0;
  buffer[n++] = (char)((s->newline_pending ? 1 : 0) | (s->in_done ? 2 : 0));
  buffer[n++] = (char)(s->newline_column & 0xff);
  buffer[n++] = (char)(s->newline_column >> 8);
  buffer[n++] = (char)s->depth;
  for (unsigned i = 0; i < s->depth; i++) {
    Block b = s->blocks[i];
    if (n + 3 > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) break;
    buffer[n++] = (char)(b.column & 0xff);
    buffer[n++] = (char)(b.column >> 8);
    buffer[n++] = (char)((b.confirmed ? 1 : 0) | (b.let ? 2 : 0));
  }
  return n;
}

void tree_sitter_agda_external_scanner_deserialize(void *payload,
                                                   const char *buffer,
                                                   unsigned length) {
  Scanner *s = payload;
  memset(s, 0, sizeof(Scanner));
  if (length < 4) return;
  s->newline_pending = buffer[0] & 1;
  s->in_done = (buffer[0] & 2) != 0;
  s->newline_column =
      (uint8_t)buffer[1] | ((uint16_t)(uint8_t)buffer[2] << 8);
  unsigned depth = (uint8_t)buffer[3];
  unsigned n = 4;
  for (unsigned i = 0; i < depth && n + 3 <= length; i++) {
    Block *b = &s->blocks[i];
    b->column = (uint8_t)buffer[n] | ((uint16_t)(uint8_t)buffer[n + 1] << 8);
    b->confirmed = buffer[n + 2] & 1;
    b->let = (buffer[n + 2] & 2) != 0;
    n += 3;
    s->depth = i + 1;
  }
}

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

// Agda also treats a no-break space as white space (iswspace does not, in
// the C locale).
static bool is_space(int32_t c) { return iswspace(c) || c == 0xa0; }

// Characters that can be part of an Agda name (see NOT_NAME in grammar.js).
static bool is_name_char(int32_t c) {
  if (c == 0 || is_space(c)) return false;
  switch (c) {
    case '.': case ';': case '{': case '}': case '(': case ')':
    case '@': case '"':
    case 0x2983: case 0x2984:  // ⦃ ⦄
    case 0x2987: case 0x2988:  // ⦇ ⦈
      return false;
    default:
      return true;
  }
}

// Whether some open block is a `let` block.
static bool in_let(Scanner *s) {
  for (unsigned i = 0; i < s->depth; i++)
    if (s->blocks[i].let) return true;
  return false;
}

// Whether the input continues with the keyword `word` (not a longer name).
static bool scan_word(TSLexer *lexer, const char *word) {
  for (const char *p = word; *p; p++) {
    if (lexer->lookahead != *p) return false;
    advance(lexer);
  }
  return !is_name_char(lexer->lookahead);
}

static bool push(Scanner *s, uint16_t column, bool confirmed, bool let) {
  if (s->depth >= MAX_DEPTH) return false;
  s->blocks[s->depth++] = (Block){column, confirmed, let};
  return true;
}

static void confirm_all(Scanner *s) {
  for (unsigned i = 0; i < s->depth; i++) s->blocks[i].confirmed = true;
}

// Column of the innermost confirmed block, or -1.
static int confirmed_column(Scanner *s) {
  for (int i = (int)s->depth - 1; i >= 0; i--)
    if (s->blocks[i].confirmed) return s->blocks[i].column;
  return -1;
}

// Lexes a nested block comment; the lexer is past "{-".
static void set_newline(Scanner *s, bool newline, uint16_t column) {
  s->newline_pending = newline;
  s->newline_column = column;
  s->in_done = false;
}

static bool scan_block_comment(TSLexer *lexer) {
  unsigned depth = 1;
  while (!lexer->eof(lexer)) {
    int32_t c = lexer->lookahead;
    advance(lexer);
    if (c == '{' && lexer->lookahead == '-') {
      advance(lexer);
      depth++;
    } else if (c == '-' && lexer->lookahead == '}') {
      advance(lexer);
      if (--depth == 0) break;
    }
  }
  lexer->mark_end(lexer);
  lexer->result_symbol = COMMENT;
  return true;
}

static bool scan_line_comment(TSLexer *lexer) {
  while (!lexer->eof(lexer) && lexer->lookahead != '\n') advance(lexer);
  lexer->mark_end(lexer);
  lexer->result_symbol = COMMENT;
  return true;
}

// Emits a zero-width layout token in front of the token at `column`.
static bool emit(TSLexer *lexer, Scanner *s, enum TokenType t, bool newline,
                 uint16_t column) {
  lexer->result_symbol = t;
  set_newline(s, newline, column);
  return true;
}

bool tree_sitter_agda_external_scanner_scan(void *payload, TSLexer *lexer,
                                            const bool *valid) {
  Scanner *s = payload;
  bool error_recovery = valid[ERROR_SENTINEL];

  bool at_recorded = lexer->get_column(lexer) == s->newline_column;
  bool newline = s->newline_pending && at_recorded;
  bool in_done = s->in_done && at_recorded;
  while (is_space(lexer->lookahead)) {
    if (lexer->lookahead == '\n') newline = true;
    skip(lexer);
  }
  lexer->mark_end(lexer);

  bool eof = lexer->eof(lexer);
  uint16_t column = eof ? 0 : (uint16_t)lexer->get_column(lexer);

  // Comments first: they are extras and never take part in layout.
  if (!eof && lexer->lookahead == '-') {
    advance(lexer);
    if (lexer->lookahead == '-') {
      // the newline after a line comment is seen by the next scan itself
      set_newline(s, false, 0);
      return scan_line_comment(lexer);
    }
    goto layout;  // a name starting with '-'; nothing consumed is kept
  }
  if (!eof && lexer->lookahead == '{') {
    advance(lexer);
    if (lexer->lookahead == '-') {
      advance(lexer);
      if (lexer->lookahead != '#') {
        scan_block_comment(lexer);
        set_newline(s, newline, (uint16_t)lexer->get_column(lexer));
        return true;
      }
    }
    // `{-#` is a pragma, `{`/`{!` are grammar tokens. Fall through: the
    // token is a closing-relevant character only for '}' (handled below).
    goto layout;
  }

layout:
  if (error_recovery) return false;

  // After a '-' or '{' was looked at above, lookahead is past the token's
  // first character; the checks on `c` below only apply when it is not.
  bool advanced = !eof && lexer->get_column(lexer) != column;
  int32_t c = lexer->lookahead;

  if (valid[LAYOUT_START] || valid[LET_START]) {
    bool let = !valid[LAYOUT_START];
    if (eof) {
      push(s, 0, true, let);
      return emit(lexer, s, let ? LET_START : LAYOUT_START, true, column);
    }
    int confirmed = confirmed_column(s);
    if (newline && (int)column <= confirmed) {
      // Empty block: open it at a column that the current token is left
      // of, so that it is closed again right away (the newline is kept
      // for that).
      push(s, (uint16_t)(confirmed + 1), true, let);
      return emit(lexer, s, let ? LET_START : LAYOUT_START, true, column);
    } else {
      // The file's own block stays tentative until its first newline, so
      // that `module M where` on the first line can own the column-0
      // declarations that follow it, as in Agda.
      push(s, column, newline && s->depth > 0, let);
    }
    // The token starts the block's first item: it is not a new item, so
    // the newline must not produce a `;` for it.
    return emit(lexer, s, let ? LET_START : LAYOUT_START, false, column);
  }

  if (s->depth == 0) return false;
  Block top = s->blocks[s->depth - 1];

  if (valid[LAYOUT_END]) {
    if (eof || (newline && column < top.column)) {
      s->depth--;
      if (newline) confirm_all(s);
      return emit(lexer, s, LAYOUT_END, newline, column);
    }
    if (!advanced && (c == ')' || c == '}' || c == 0x2984 || c == 0x2988)) {
      s->depth--;
      return emit(lexer, s, LAYOUT_END, newline, column);
    }
    // A `let` block ends before `in`. Any block ends before a `where` at or
    // left of its column: `where` never starts a block item, it belongs to
    // the enclosing clause (`x ← e` in a `do` block, a clause in `where`).
    if (!advanced && (c == 'i' || c == 'w')) {
      const char *word = c == 'i' ? "in" : "where";
      // `in` also closes blocks opened inside its `let`, as in
      // `let instance x = a in e`, but nothing outside it.
      bool applies = c == 'i' ? !in_done && in_let(s) : column <= top.column;
      if (applies && scan_word(lexer, word)) {
        s->depth--;
        emit(lexer, s, LAYOUT_END, newline, column);
        s->in_done = c == 'i' && top.let;
        return true;
      }
      // Not the keyword: go on (the `;` below is zero-width, so it does not
      // matter that the lexer has moved).
    }
  }

  if (valid[LAYOUT_SEMICOLON] && newline && !eof && column == top.column) {
    confirm_all(s);
    // One `;` per new line; blocks accept `;;`, so keeping the newline
    // would emit them forever.
    return emit(lexer, s, LAYOUT_SEMICOLON, false, column);
  }

  return false;
}
