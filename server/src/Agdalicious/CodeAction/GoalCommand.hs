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

-- | A goal command: its title, the Agda command with its arguments up to the
-- goal's (such as @Cmd_give WithoutForce@), and, if it works on what is in
-- the hole, what to ask for when the hole is empty (as the Emacs mode does).
data GoalCommand = GoalCommand {menuTitle :: Text, agdaCommand :: Text, prompt :: Maybe Text}
  deriving (Show)

goalCommands :: [GoalCommand]
goalCommands =
  [ GoalCommand "Give" "Cmd_give WithoutForce" (Just "expression to give")
  , GoalCommand "Give (with force)" "Cmd_give WithForce" (Just "expression to give")
  , GoalCommand "Elaborate and give" "Cmd_elaborate_give Simplified" (Just "expression to elaborate and give")
  , GoalCommand "Refine" "Cmd_refine" Nothing
  , GoalCommand "Refine or introduce" "Cmd_refine_or_intro False" Nothing
  , GoalCommand "Introduce" "Cmd_intro False" Nothing
  , GoalCommand "Introduce (pattern-matching λ)" "Cmd_intro True" Nothing
  , GoalCommand "Case split" "Cmd_make_case" (Just "pattern variables to case (empty for split on result)")
  , GoalCommand "Auto (proof search)" "Cmd_autoOne AsIs" Nothing
  , GoalCommand "Solve constraints" "Cmd_solveOne Simplified" Nothing
  , GoalCommand "Goal type" "Cmd_goal_type Simplified" Nothing
  , GoalCommand "Context" "Cmd_context Simplified" Nothing
  , GoalCommand "Goal type and context" "Cmd_goal_type_context Simplified" Nothing
  , GoalCommand "Goal type, context and inferred type" "Cmd_goal_type_context_infer Simplified" (Just "expression to type")
  , GoalCommand "Goal type, context and checked type" "Cmd_goal_type_context_check Simplified" (Just "expression to type")
  , GoalCommand "Infer type" "Cmd_infer Simplified" (Just "expression to type")
  , GoalCommand "Helper function type" "Cmd_helper_function AsIs" (Just "expression")
  , GoalCommand "Evaluate to normal form" "Cmd_compute DefaultCompute" (Just "expression to normalise")
  , GoalCommand "Evaluate to head normal form" "Cmd_compute HeadCompute" (Just "expression to normalise")
  , GoalCommand "Evaluate, ignoring abstract" "Cmd_compute IgnoreAbstract" (Just "expression to normalise")
  , GoalCommand "Evaluate and show (with its Show instance)" "Cmd_compute UseShowInstance" (Just "expression to normalise")
  , GoalCommand "Why in scope" "Cmd_why_in_scope" (Just "name")
  , GoalCommand "Module contents" "Cmd_show_module_contents Simplified" (Just "module name (empty for current module)")
  ]

-- | The line that runs a goal command on a goal with this text in it.
commandLine :: GoalCommand -> Int -> Text -> String
commandLine c goal input = T.unpack (agdaCommand c) <> " " <> show goal <> " noRange " <> show (T.unpack input)
