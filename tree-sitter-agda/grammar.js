/**
 * @file Agda grammar for tree-sitter, aimed at syntax highlighting.
 *
 * Agda cannot be parsed fully without scope information: operators are
 * user-defined mixfix names (`if_then_else_`, `_≡⟨_⟩_`, ...) whose parts are
 * only known after imports are resolved. This grammar therefore parses the
 * declaration structure (modules, data types, records, signatures, clauses,
 * layout blocks) exactly, and expressions as flat sequences of atoms. That is
 * enough to highlight definitions, constructors, fields, keywords, literals,
 * holes and binders.
 *
 * Layout (indentation-based blocks after `where`, `postulate`, `field`, `let`,
 * `do`, ...) is handled by the external scanner in src/scanner.c.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

// Characters that can never be part of a name (Agda's lexer reserves them).
const NOT_NAME = '\\s.;{}()@"⦃⦄⦇⦈';
// A name may start with a backslash only if a non-letter follows it, as in
// the operator `\\-cong`; a backslash before a letter is a lambda.
const NAME_PART = `(\\\\[^${NOT_NAME}a-zA-Z]|[^${NOT_NAME}\\\\'])[^${NOT_NAME}]*`;
const QNAME = new RegExp(`(${NAME_PART}\\.)*${NAME_PART}`);

const sep1 = (rule, separator) => seq(rule, repeat(seq(separator, rule)));

// The items of a layout block. Empty items are allowed: a pragma at the
// block's column gets a virtual `;` before it, and may be the last thing in
// the block, and explicit `;;` is fine in Agda too.
const items = ($, rule) => seq(
  repeat($._semicolon),
  optional(seq(
    rule,
    repeat(seq(repeat1($._semicolon), rule)),
    repeat($._semicolon),
  )),
);

module.exports = grammar({
  name: 'agda',

  externals: $ => [
    $._layout_start,
    $._let_start,
    $._layout_semicolon,
    $._layout_end,
    $.comment,
    $._error_sentinel,
  ],

  extras: $ => [/\s/, $.comment, $.pragma],

  word: $ => $._qname,

  // Agda's keywords and reserved symbols are never names. (`as` is only a
  // keyword after `import`, and `to` only inside `renaming (...)`, so
  // they are not listed.)
  reserved: {
    global: _ => [
      'abstract', 'codata', 'coinductive', 'constructor', 'data', 'do',
      'eta-equality', 'field', 'forall', 'hiding', 'import', 'in', 'inductive',
      'infix', 'infixl', 'infixr', 'instance', 'interleaved', 'let', 'macro',
      'module', 'mutual', 'no-eta-equality', 'opaque', 'open', 'pattern',
      'postulate', 'primitive', 'private', 'public', 'quote', 'quoteTerm',
      'record', 'renaming', 'rewrite', 'syntax', 'tactic', 'unfolding',
      'unquote', 'unquoteDecl', 'unquoteDef', 'using', 'variable', 'where',
      'with',
      ':', '=', '|', '->', '→', '\\', 'λ', '∀', '_', '?',
    ],
  },

  conflicts: $ => [
    [$.signature, $._atom],
    [$.typed_binding, $._atom],
  ],

  rules: {
    source_file: $ => $._top_block,

    // The file itself is one layout block.
    _top_block: $ => seq(
      $._layout_start,
      items($, $._declaration),
      $._layout_end,
    ),

    _semicolon: $ => choice($._layout_semicolon, ';'),

    _decl_block: $ => seq(
      $._layout_start,
      items($, $._declaration),
      $._layout_end,
    ),

    // ------------------------------------------------------------------
    // Declarations

    _declaration: $ => choice(
      $.module_declaration,
      $.open,
      $.import,
      $.data_declaration,
      $.record_declaration,
      $.signature,
      $.function_clause,
      $.fixity,
      $.syntax_declaration,
      $.pattern_synonym,
      $.postulate,
      $.private,
      $.abstract,
      $.instance,
      $.mutual,
      $.macro,
      $.variable,
      $.primitive,
      $.opaque,
      $.unfolding,
      $.unquote_declaration,
    ),

    module_declaration: $ => seq(
      'module',
      field('name', choice($.qname, $.underscore)),
      repeat($._atom),
      choice(
        seq('where', $._decl_block),
        seq('=', $._expression, repeat($._import_directive)),
      ),
    ),

    import: $ => seq(
      optional('open'),
      'import',
      field('module', $.qname),
      repeat($._atom),
      optional(seq('as', field('alias', $.qname))),
      repeat($._import_directive),
    ),

    open: $ => choice(
      seq(
        'open',
        field('module', $.qname),
        repeat($._atom),
        repeat($._import_directive),
      ),
      // open module M = N args
      seq(
        'open',
        'module',
        field('name', choice($.qname, $.underscore)),
        repeat($._atom),
        '=',
        $._expression,
        repeat($._import_directive),
      ),
    ),

    _import_directive: $ => choice(
      'public',
      seq('using', $.import_list),
      seq('hiding', $.import_list),
      seq('renaming', $.renaming_list),
    ),

    import_list: $ => seq('(', optional(sep1($._imported_name, ';')), ')'),

    renaming_list: $ => seq(
      '(',
      optional(sep1($.renaming, ';')),
      ')',
    ),

    renaming: $ => seq(
      $._imported_name,
      'to',
      optional(seq(choice('infix', 'infixl', 'infixr'), $.number)),
      $._imported_name,
    ),

    _imported_name: $ => seq(optional('module'), $.qname),

    data_declaration: $ => seq(
      choice('data', 'codata'),
      field('name', choice($.qname, $.underscore)),
      repeat($._binding),
      optional(seq(':', field('type', $._expression))),
      optional(seq('where', alias($._constructor_block, $.constructors))),
    ),

    _constructor_block: $ => seq(
      $._layout_start,
      items($, choice(
        alias($.signature, $.constructor),
        seq('instance', $._constructor_block),
      )),
      $._layout_end,
    ),

    record_declaration: $ => seq(
      'record',
      field('name', $.qname),
      repeat($._binding),
      optional(seq(':', field('type', $._expression))),
      optional(seq('where', $._record_block)),
    ),

    _record_block: $ => seq(
      $._layout_start,
      items($, $._record_item),
      $._layout_end,
    ),

    _record_item: $ => choice(
      $.record_constructor,
      $.fields,
      $.record_directive,
      $._declaration,
    ),

    record_constructor: $ => seq('constructor', field('name', $.qname)),

    record_directive: _ => choice(
      'eta-equality', 'no-eta-equality', 'inductive', 'coinductive', 'pattern',
    ),

    fields: $ => seq('field', $._field_block),

    _field_block: $ => seq(
      $._layout_start,
      items($, choice(
        alias($.signature, $.field_signature),
        seq('instance', $._field_block),
      )),
      $._layout_end,
    ),

    signature: $ => seq(
      repeat1(choice(
        field('name', choice($.qname, $.underscore)),
        $.braces,
        $.instance_braces,
        '.', '..', '@', $.number,
      )),
      ':',
      field('type', $._expression),
    ),

    function_clause: $ => seq(
      field('lhs', $.lhs),
      optional(seq('=', field('rhs', $._expression))),
      optional(choice($.where, $.where_module)),
    ),

    // `using p ← e` is the pattern-matching `using` of Agda 2.7. Clauses of
    // `λ where` are `p → e`: they parse as a left-hand side only, so it
    // takes any expression.
    lhs: $ => repeat1(choice(
      $._expression_item, 'with', 'rewrite', 'in', '|', 'using', '←', '<-', $.ellipsis,
    )),

    ellipsis: _ => '...',

    where: $ => seq('where', $._decl_block),

    // A named where block: `f = g  module M where ...`.
    where_module: $ => seq(
      'module',
      field('name', $.qname),
      'where',
      $._decl_block,
    ),

    fixity: $ => seq(
      choice('infix', 'infixl', 'infixr'),
      $.number,
      repeat1($.qname),
    ),

    syntax_declaration: $ => seq('syntax', repeat1($._atom), '=', $._expression),

    pattern_synonym: $ => seq(
      'pattern',
      field('name', $.qname),
      repeat($._atom),
      '=',
      $._expression,
    ),

    postulate: $ => seq('postulate', $._decl_block),
    private: $ => seq('private', $._decl_block),
    abstract: $ => seq('abstract', $._decl_block),
    instance: $ => seq('instance', $._decl_block),
    mutual: $ => seq(optional('interleaved'), 'mutual', $._decl_block),
    macro: $ => seq('macro', $._decl_block),
    variable: $ => seq('variable', $._decl_block),
    primitive: $ => seq('primitive', $._decl_block),
    opaque: $ => seq('opaque', $._decl_block),
    unfolding: $ => seq('unfolding', repeat1($._atom)),

    unquote_declaration: $ => seq(
      choice('unquoteDecl', 'unquoteDef'),
      repeat($._atom),
      '=',
      $._expression,
    ),

    // ------------------------------------------------------------------
    // Expressions (flat; see the file header)

    _expression: $ => repeat1(choice($._expression_item, 'record')),

    _expression_item: $ => choice(
      $._atom,
      $.let,
      $.do,
      $.lambda_where,
      'quote',
      'quoteTerm',
      'unquote',
      'tactic',
    ),

    _atom: $ => choice(
      $.qname,
      $.number,
      $.string,
      $.char,
      $.hole,
      $.underscore,
      $.parens,
      $.braces,
      $.instance_braces,
      $.idiom_brackets,
      $.absurd,
      $.record_expression,
      '→', '->',
      'λ', '\\',
      '∀', 'forall',
      '.', '..',
      '@',
    ),

    // A binder position in data/record headers: (A : Set) {n : ℕ} or a name.
    _binding: $ => choice($.qname, $.underscore, $.parens, $.braces, $.instance_braces, '@', '.', '..'),

    underscore: _ => '_',

    absurd: _ => '()',

    hole: _ => choice('?', token(seq('{!', /([^!]|!+[^!}])*/, /!+\}/))),

    parens: $ => seq(
      '(',
      choice($.typed_binding, $._expression, $.open),
      ')',
    ),

    braces: $ => seq(
      '{',
      optional(sep1(choice($.typed_binding, $.assignment, $._expression), ';')),
      '}',
    ),

    instance_braces: $ => seq(
      choice('⦃', '{{'),
      choice($.typed_binding, $.assignment, $._expression),
      choice('⦄', seq('}', '}')),
    ),

    // `(|` only opens an idiom bracket when white space follows.
    idiom_brackets: $ => choice(
      seq('⦇', optional($._expression), '⦈'),
      seq(alias(token(seq('(|', /\s/)), '(|'), $._expression, '|)'),
    ),

    typed_binding: $ => seq($._typed_names, ':', field('type', $._expression)),
    _typed_names: $ => repeat1(choice(
      field('name', choice($.qname, $.underscore)),
      '@', '.', '..', $.number, $.parens, $.braces,
    )),

    assignment: $ => seq(
      field('name', $.qname),
      '=',
      field('value', $._expression),
    ),

    record_expression: $ => prec(1, seq('record', $.braces)),

    let: $ => prec.right(seq(
      'let',
      $._let_block,
      optional(seq('in', $._expression)),
    )),

    _let_block: $ => seq(
      $._let_start,
      items($, $._declaration),
      $._layout_end,
    ),

    do: $ => seq('do', $._do_block),

    _do_block: $ => seq(
      $._layout_start,
      items($, $.do_statement),
      $._layout_end,
    ),

    // `let` without `in` is a statement; it parses as a `let` expression.
    // `pat ← e where alts` handles the other patterns.
    do_statement: $ => seq(
      $._expression,
      optional(seq(
        choice('←', '<-'),
        $._expression,
        optional($.where),
      )),
    ),

    lambda_where: $ => prec(1, seq(choice('λ', '\\'), 'where', $._decl_block)),

    // ------------------------------------------------------------------
    // Tokens. The literal rules come before _qname: for a tie in length the
    // earlier rule wins, so `42` is a number but `1+n` is still a name.

    number: _ => token(choice(
      /-?0x[0-9a-fA-F]+(_[0-9a-fA-F]+)*/,
      /-?0b[01]+(_[01]+)*/,
      /-?[0-9]+\.[0-9]+([eE][-+]?[0-9]+)?/,
      /-?[0-9]+(_[0-9]+)*/,
    )),

    // Agda lets a string span lines; `\` white space `\` is a gap.
    string: _ => token(seq('"', repeat(choice(/[^"\\]/, /\\\s+\\/, /\\./)), '"')),

    char: _ => token(seq('\'', choice(/[^'\\\n]/, /\\[^\n]+?/), '\'')),

    pragma: _ => token(seq('{-#', /([^#]|#+[^#-]|#+-[^#}])*/, /#+-\}/)),

    qname: $ => $._qname,

    _qname: _ => token(QNAME),
  },
});
