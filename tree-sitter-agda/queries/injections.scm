; The code in FOREIGN and COMPILE pragmas, in the backend's language. The
; backend's name is the language (neovim lower-cases it: C, CPP, JS, ...),
; except for GHC, whose code is Haskell.
(pragma
  backend: (backend) @injection.language
  code: (code) @injection.content
  (#gsub! @injection.language "^GHC$" "haskell"))
