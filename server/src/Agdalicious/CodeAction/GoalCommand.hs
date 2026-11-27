-- | The commands Agda has for a goal (the constructors of @Interaction'@ in
-- @src/full/Agda/Interaction/Base.hs@ that take one): give, refine, case
-- split, proof search, and those that show something about it. Where an
-- argument changes what a command does (giving with force, the λ an intro
-- writes, how to evaluate), each choice is a command of its own; types are
-- shown simplified, as the Emacs mode does by default.
--
-- An older Agda may lack some: the server asks the one it runs which it
-- knows ('Agdalicious.Agda.knows'). Not here: @Cmd_highlight@ (for editors
-- that highlight what is typed into a hole) and @Cmd_backend_hole@ (a
-- backend's own commands).
module Agdalicious.CodeAction.GoalCommand (GoalCommand (..), goalCommands, commandLine) where

import Data.Text (Text)
import Data.Text qualified as T

-- | A goal command: its title, and the Agda command with its arguments up to
-- the goal's (such as @Cmd_give WithoutForce@).
data GoalCommand = GoalCommand {menuTitle :: Text, agdaCommand :: Text}
  deriving (Show)

goalCommands :: [GoalCommand]
goalCommands =
  [ GoalCommand "Give" "Cmd_give WithoutForce"
  , GoalCommand "Give (with force)" "Cmd_give WithForce"
  , GoalCommand "Elaborate and give" "Cmd_elaborate_give Simplified"
  , GoalCommand "Refine" "Cmd_refine"
  , GoalCommand "Refine or introduce" "Cmd_refine_or_intro False"
  , GoalCommand "Introduce" "Cmd_intro False"
  , GoalCommand "Introduce (pattern-matching λ)" "Cmd_intro True"
  , GoalCommand "Case split" "Cmd_make_case"
  , GoalCommand "Auto (proof search)" "Cmd_autoOne AsIs"
  , GoalCommand "Solve constraints" "Cmd_solveOne Simplified"
  , GoalCommand "Goal type" "Cmd_goal_type Simplified"
  , GoalCommand "Context" "Cmd_context Simplified"
  , GoalCommand "Goal type and context" "Cmd_goal_type_context Simplified"
  , GoalCommand "Goal type, context and inferred type" "Cmd_goal_type_context_infer Simplified"
  , GoalCommand "Goal type, context and checked type" "Cmd_goal_type_context_check Simplified"
  , GoalCommand "Infer type" "Cmd_infer Simplified"
  , GoalCommand "Helper function type" "Cmd_helper_function AsIs"
  , GoalCommand "Evaluate to normal form" "Cmd_compute DefaultCompute"
  , GoalCommand "Evaluate to head normal form" "Cmd_compute HeadCompute"
  , GoalCommand "Evaluate, ignoring abstract" "Cmd_compute IgnoreAbstract"
  , GoalCommand "Evaluate and show (with its Show instance)" "Cmd_compute UseShowInstance"
  , GoalCommand "Why in scope" "Cmd_why_in_scope"
  , GoalCommand "Module contents" "Cmd_show_module_contents Simplified"
  ]

-- | The line that runs a goal command on a goal with this text in it.
commandLine :: GoalCommand -> Int -> Text -> String
commandLine c goal input = T.unpack (agdaCommand c) <> " " <> show goal <> " noRange " <> show (T.unpack input)
