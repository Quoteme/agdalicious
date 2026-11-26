# What Agda can do with a file: `--interaction` and the Emacs mode

Reference for building agdalicious-lsp. It comes from the Agda 2.8.0 sources (`src/full/Agda/Interaction/Base.hs` for commands, `Response/Base.hs` for responses, `src/data/emacs-mode/agda2-mode.el` for the Emacs side) and was checked against a live `agda --interaction-json` session (at the end).

## The protocol

`agda --interaction` (S-expression output, used by Emacs) and `agda --interaction-json` (one JSON object per line; the one to use) read commands from stdin, one per line. Each output batch starts with the prompt `JSON> ` (or `Agda2> `).

A command line is a Haskell `Read`-syntax value:

```
IOTCM "<file>" <HighlightingLevel> <HighlightingMethod> (<Interaction>)
```

- `<file>`: absolute path of the current file. Goal commands fail if it is not the file that was loaded last.
- `HighlightingLevel`: `None`, `NonInteractive` (highlighting for the whole file after checking) or `Interactive` (also while it checks).
- `HighlightingMethod`: `Direct` (highlighting comes inline on stdout) or `Indirect` (in temporary files that stdout names; this is what Emacs uses, to keep its pipe small).
- Ranges: `noRange`, or `(intervalsToRange (Just (mkAbsolute "<file>")) [Interval (Pn () <offset> <line> <col>) (Pn () <offset> <line> <col>)])`. Offsets, lines and columns count from 1. For a goal command the range is the hole's, and the string is the text inside it.
- Goals are named by `InteractionId`, a number. `Cmd_load` returns the numbers (`InteractionPoints`).
- Strings are Haskell string literals (`"..."` with `\"`, `\n`, `\\`).

Commands are queued and run one at a time. `Cmd_abort` (sent outside the queue) tries to stop the current one.

### Arguments that recur

| Type | Values | Meaning |
|---|---|---|
| `Rewrite` | `AsIs`, `Instantiated`, `HeadNormal`, `Simplified`, `Normalised` | how much to normalise the types shown |
| `ComputeMode` | `DefaultCompute`, `HeadCompute`, `IgnoreAbstract`, `UseShowInstance` | how `Cmd_compute*` evaluates |
| `UseForce` | `WithForce`, `WithoutForce` | `give` even when termination/positivity checks fail |
| `Remove` | `Remove`, `Keep` | delete the temporary file after `Cmd_tokenHighlighting` |

## Commands (`Interaction`)

### Whole file

| Command | Does |
|---|---|
| `Cmd_load "<file>" [<args>]` | Type-check the file with extra command-line options. Sends highlighting, warnings/errors, all goals, the interaction points. |
| `Cmd_load_no_metas "<file>"` | Load, and fail if any metavariable is left unsolved. |
| `Cmd_compile <backend> "<file>" [<args>]` | Compile: `GHC`, `GHCNoMain`, `JS`, `LaTeX`, `QuickLaTeX`, `HTML`, or another backend by name. |
| `Cmd_constraints` | Show unsolved constraints. |
| `Cmd_metas <Rewrite>` | Show all goals (and unsolved metas); constraints if there are none. |
| `Cmd_solveAll <Rewrite>` | Fill every goal whose value the constraints determine. |
| `Cmd_autoAll <Rewrite>` | Run proof search (Mimer) on all goals. |
| `Cmd_load_highlighting_info "<file>"` | Send cached highlighting for an already-checked file, without checking it again. |
| `Cmd_tokenHighlighting "<file>" <Remove>` | Token-based highlighting only. Works on files that don't scope-check or have the wrong module name, so it can be run on a temporary copy of an unsaved buffer (with `Remove`). |
| `ShowImplicitArgs <Bool>` / `ToggleImplicitArgs` | Show `{implicit}` arguments in output. |
| `ShowIrrelevantArgs <Bool>` / `ToggleIrrelevantArgs` | Show irrelevant arguments. |
| `Cmd_show_version` | Agda's version. |
| `Cmd_abort` / `Cmd_exit` | Abort the running command / quit. |

### Top level (no goal needed; parsed in the scope of the loaded module)

| Command | Does |
|---|---|
| `Cmd_infer_toplevel <Rewrite> "<expr>"` | Infer the type of an expression. |
| `Cmd_compute_toplevel <ComputeMode> "<expr>"` | Normalise an expression. |
| `Cmd_why_in_scope_toplevel "<name>"` | Explain where a name comes from (definition site, imports, `open`s). |
| `Cmd_show_module_contents_toplevel <Rewrite> "<module>"` | List a module's names with their types. |
| `Cmd_search_about_toplevel <Rewrite> "<names>"` | Find definitions whose types mention all the given names (`_+_ _≡_`). |
| `Cmd_backend_top <backend> "<payload>"` | Backend-specific command. |

### Goals (`<id> <range> "<text in the hole>"`)

| Command | Does | Edits the buffer? |
|---|---|---|
| `Cmd_give <UseForce> …` | Check the hole's text against the goal type and fill it in. | yes (`GiveAction`) |
| `Cmd_elaborate_give <Rewrite> …` | Like give, but put the elaborated term in (implicits made explicit). | yes |
| `Cmd_refine …` | Apply the given function to new holes for its arguments; with an empty hole, a constructor of the goal type. | yes |
| `Cmd_intro <Bool> …` | Introduce: a constructor, or a λ for a function type. | yes |
| `Cmd_refine_or_intro <Bool> …` | `refine` if the hole has text, else `intro` (Emacs `C-c C-r`). | yes |
| `Cmd_make_case …` | Split on the variables in the hole (empty: split on the result, i.e. copatterns or introduce args). Returns new clauses that replace the clause's line. | yes (`MakeCase`) |
| `Cmd_autoOne <Rewrite> …` | Proof search for this goal; the hole text can give hints/options (`-t 10`, `-m`, names). | yes |
| `Cmd_solveOne <Rewrite> …` | Fill this goal if the constraints determine it. | yes (`SolveAll`) |
| `Cmd_goal_type <Rewrite> …` | The goal's type. | no |
| `Cmd_context <Rewrite> …` | The local context (variables in scope with their types). | no |
| `Cmd_goal_type_context <Rewrite> …` | Goal type and context. | no |
| `Cmd_goal_type_context_infer <Rewrite> …` | …and the inferred type of the hole's text ("Have:"). | no |
| `Cmd_goal_type_context_check <Rewrite> …` | …and the hole's text checked against the goal, elaborated. | no |
| `Cmd_infer <Rewrite> …` | Infer the type of the hole's text, in the goal's scope. | no |
| `Cmd_compute <ComputeMode> …` | Normalise the hole's text in the goal's scope. | no |
| `Cmd_helper_function <Rewrite> …` | Type signature for a helper function `h args` (the hole's text), ready to paste. | no |
| `Cmd_why_in_scope …` | Why a name is in scope, in the goal's scope. | no |
| `Cmd_show_module_contents <Rewrite> …` | A module's contents in the goal's scope. | no |
| `Cmd_highlight …` | Highlighting for text just put into a goal. | no |
| `Cmd_backend_hole <id> <range> "<text>" <backend> "<payload>"` | Backend-specific goal command. | — |

## Responses (`--interaction-json`, field `kind`)

| `kind` | Carries | Client should |
|---|---|---|
| `HighlightingInfo` | `direct: true` + `info.payload` (ranges, each with `atoms` such as `keyword`, `function`, `inductiveconstructor`, `bound`, `unsolvedmeta`, `terminationproblem`, and a `definitionSite`), or `direct: false` + `filepath` of a temp file with that JSON | apply semantic highlighting, record go-to-definition targets |
| `ClearHighlighting` | `tokenBased` | drop old highlighting |
| `Status` | `checked`, `showImplicitArguments`, `showIrrelevantArguments` | show state |
| `RunningInfo` | `message`, `debugLevel` | show progress ("Checking M (…)") |
| `ClearRunningInfo` | — | clear it |
| `DisplayInfo` | `info.kind`: `AllGoalsWarnings`, `Error`, `CompilationOk`, `Constraints`, `Context`, `GoalSpecific` (`goalInfo.kind` `GoalType` / `CurrentGoal` / `InferredType` / `NormalForm` / `HelperFunction`), `InferredType`, `NormalForm`, `ModuleContents`, `SearchAbout`, `WhyInScope`, `Auto`, `IntroNotFound`, `IntroConstructorUnknown`, `Time`, `Version` | show it (infoview), turn errors/warnings into diagnostics |
| `InteractionPoints` | goal ids with ranges | remember where the goals are |
| `GiveAction` | goal, `giveResult` (`str` or `paren: true/false`) | put the text in the hole (in parentheses if asked), remove the hole markers |
| `MakeCase` | goal, `variant` (`Function` / `ExtendedLambda`), `clauses` | replace the clause line (or the λ's clauses) with these |
| `SolveAll` | `solutions: [{interactionPoint, expression}]` | fill those holes |
| `Mimer` | `solution` (a string, or null if none was found) | proof search result for the goal it was asked about |
| `JumpToError` | `filepath`, `position` | move the cursor |
| `DoneAborting` / `DoneExiting` | — | — |

## The Emacs mode (agda2-mode)

Everything is driven through the commands above; Emacs adds buffer editing (replacing holes, rewriting clauses), the `*Agda information*` window, Unicode input, and highlighting.

### Prefix arguments choose the normalisation

Most info commands take a prefix: none → `Simplified`, `C-u` → `Instantiated`, `C-u C-u` → `Normalised`, `C-u C-u C-u` → `HeadNormal`. The `-asis` variants use `AsIs` instead of `Instantiated`. For `C-c C-n`, `C-u` ignores `abstract`, `C-u C-u` uses the `Show` instance (`UseShowInstance`), and `C-u C-u C-u` evaluates to head normal form. For `C-c C-SPC`, `C-u` gives `WithForce`.

### Key bindings (agda2-command-table)

"Local" means the cursor is in a goal and the goal's text is the argument. "Local/global" commands work in a goal, or at top level after asking for an expression in the minibuffer.

| Key | Emacs command | Interaction command |
|---|---|---|
| `C-c C-l` | load | `Cmd_load` |
| `C-c C-x C-c` | compile | `Cmd_compile` |
| `C-c C-x C-q` / `C-c C-x C-r` | quit / restart Agda | `Cmd_exit` / new process |
| `C-c C-x C-a` | abort | `Cmd_abort` |
| `C-c C-x C-d` | remove goals and highlighting | (Emacs only) |
| `C-c C-x C-h` / `C-c C-x C-i` | toggle implicit / irrelevant args | `ToggleImplicitArgs` / `ToggleIrrelevantArgs` |
| `C-c C-=` | show constraints | `Cmd_constraints` |
| `C-c C-?` | show goals | `Cmd_metas` |
| `C-c C-s` | solve (goal or all) | `Cmd_solveOne` / `Cmd_solveAll` |
| `C-c C-a` | auto (goal or all) | `Cmd_autoOne` / `Cmd_autoAll` |
| `C-c C-f` / `C-c C-b` | next / previous goal | (Emacs only) |
| `C-c C-SPC` | give | `Cmd_give` |
| `C-c C-m` | elaborate and give | `Cmd_elaborate_give` |
| `C-c C-r` | refine | `Cmd_refine_or_intro` |
| `C-c C-c` | case split | `Cmd_make_case` |
| `C-c C-t` | goal type | `Cmd_goal_type` |
| `C-c C-e` | context | `Cmd_context` |
| `C-c C-h` | helper function type | `Cmd_helper_function` |
| `C-c C-d` | infer type | `Cmd_infer` / `Cmd_infer_toplevel` |
| `C-c C-w` | why in scope | `Cmd_why_in_scope` / `…_toplevel` |
| `C-c C-,` | goal type and context | `Cmd_goal_type_context` |
| `C-c C-.` | … and inferred type | `Cmd_goal_type_context_infer` |
| `C-c C-;` | … and checked type | `Cmd_goal_type_context_check` |
| `C-c C-z` | search about | `Cmd_search_about_toplevel` |
| `C-c C-o` | module contents | `Cmd_show_module_contents` / `…_toplevel` |
| `C-c C-n` | compute normal form | `Cmd_compute` / `Cmd_compute_toplevel` |
| `C-c C-i` | backend command | `Cmd_backend_hole` / `Cmd_backend_top` |
| `C-c C-x C-s` | switch Agda version | (Emacs only) |
| `C-c C-x M-;` | comment/uncomment rest of buffer | (Emacs only) |
| `M-.` / mouse-2, `M-,` | go to definition, go back | uses `definitionSite` from highlighting |
| `TAB` / `S-TAB` | cycle indentation | (Emacs only, eri.el) |
| — | display version | `Cmd_show_version` |

Three more commands run without a key of their own. When agda2-mode starts in a buffer, it loads cached highlighting (`Cmd_load_highlighting_info`). When a give puts new text into a goal, it highlights that text (`Cmd_highlight`). After every save, it computes token-based highlighting of the file (`Cmd_tokenHighlighting "<file>" Keep`). Emacs turns Font Lock off: all of its highlighting comes from Agda.

Unicode input (`agda-input.el`) is a Quail input method with `\` as leader (`\to` → `→`, `\bN` → `ℕ`, `\Gl` → `λ`, `\_1` → `₁`, `\^1` → `¹`); the agdalicious plugin re-implements it.

## Checked live (Agda 2.8.0, `test/Example.agda`)

```
$ agda --interaction-json
IOTCM "/…/test/Example.agda" None Direct (Cmd_load "/…/test/Example.agda" [])
{"kind":"Status","status":{"checked":false,"showImplicitArguments":false,"showIrrelevantArguments":false}}
{"kind":"ClearRunningInfo"}
{"kind":"ClearHighlighting","tokenBased":"NotOnlyTokenBased"}
{"debugLevel":1,"kind":"RunningInfo","message":"Checking Example (/…/test/Example.agda).\n"}
{"info":{"errors":[],"invisibleGoals":[],"kind":"AllGoalsWarnings","visibleGoals":[{"constraintObj":{"id":0,"range":[{"end":{"col":12,"line":13,"pos":195},"start":{"col":8,"line":13,"pos":191}}]},"kind":"OfType","type":"A → A"}],"warnings":[]},"kind":"DisplayInfo"}
{"interactionPoints":[{"id":0,"range":[…]}],"kind":"InteractionPoints"}

IOTCM "/…/test/Example.agda" None Direct (Cmd_goal_type_context Simplified 0 noRange "")
{"info":{"goalInfo":{"boundary":[],"entries":[{"binding":"Set","inScope":true,"originalName":"A","reifiedName":"A"}],"kind":"GoalType","outputForms":[],"rewrite":"Simplified","type":"A → A","typeAux":{"kind":"GoalOnly"}},"interactionPoint":{"id":0,…},"kind":"GoalSpecific"},"kind":"DisplayInfo"}

IOTCM "/…/test/Example.agda" None Direct (Cmd_make_case 0 noRange "")
{"clauses":["id A x = ?"],"interactionPoint":{"id":0,…},"kind":"MakeCase","variant":"Function"}
```

`noRange` was enough for goal commands here: Agda found goal 0 by its id.
