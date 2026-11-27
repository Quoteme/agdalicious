; Highlighting for Agda. Without scope information we cannot tell a
; constructor use from a variable, so uses of names stay plain and only
; definition sites, keywords and literals are coloured. Later patterns take
; precedence over earlier ones.

; Literals and comments ---------------------------------------------------

(comment) @comment @spell
(pragma !code) @keyword.directive
(pragma ["{-#" "#-}"] @keyword.directive)
(backend) @keyword.directive
(string) @string
(char) @character
(number) @number
(hole) @comment.warning
(underscore) @character.special
(absurd) @punctuation.special
(ellipsis) @punctuation.special

; Keywords ----------------------------------------------------------------

[
  "module" "where" "let" "in" "do" "with" "rewrite"
  "postulate" "private" "abstract" "instance" "interleaved" "mutual" "macro"
  "variable" "primitive" "opaque" "unfolding"
  "constructor" "field" "pattern" "syntax"
  "eta-equality" "no-eta-equality" "inductive" "coinductive"
  "quote" "quoteTerm" "unquote" "unquoteDecl" "unquoteDef" "tactic"
  "∀" "forall"
] @keyword

["data" "codata" "record"] @keyword.type

["open" "import" "as" "using" "hiding" "renaming" "to" "public"] @keyword.import

; `using p ← e` in a clause is not an import
(lhs "using" @keyword)

["infix" "infixl" "infixr"] @keyword.modifier

["λ" "\\"] @keyword.function

; Operators and punctuation -----------------------------------------------

[":" "=" "→" "->" "|" "←" "<-" "@"] @operator

["." ".." ";"] @punctuation.delimiter

["(" ")" "{" "}" "{{" "⦃" "⦄" "⦇" "⦈" "(|" "|)"] @punctuation.bracket

; Built-in sorts ----------------------------------------------------------

((qname) @type.builtin
  (#match? @type.builtin "^(Set|Prop|SSet)([0-9₀-₉]*|ω[0-9₀-₉]*)$"))

; Bound variables ---------------------------------------------------------

(typed_binding name: (qname) @variable.parameter)
(braces (qname) @variable.parameter)

; Definition sites --------------------------------------------------------

(module_declaration name: (qname) @module)
(import module: (qname) @module)
(import alias: (qname) @module)
(open module: (qname) @module)
(open name: (qname) @module)
(where_module name: (qname) @module)

(data_declaration name: (qname) @type.definition)
(record_declaration name: (qname) @type.definition)

(signature name: (qname) @function)
(variable (signature name: (qname) @variable))

; The head of a clause is the defined function, in the usual prefix case.
(function_clause lhs: (lhs . (qname) @function))

(constructor name: (qname) @constructor)
(record_constructor name: (qname) @constructor)
(pattern_synonym name: (qname) @constructor)

(field_signature name: (qname) @variable.member)
(field_signature (braces (qname) @variable.member))
(field_signature (instance_braces (qname) @variable.member))
(assignment name: (qname) @variable.member)

(fixity (qname) @operator)
