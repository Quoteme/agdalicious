module Basics where
-- <- keyword
--     ^ module

data ℕ : Set where
-- <- keyword.type
--   ^ type.definition
--       ^ type.builtin
  zero : ℕ
  -- <- constructor
  suc  : ℕ → ℕ
  --       ^ operator

record Pair (A B : Set) : Set where
--     ^ type.definition
--           ^ variable.parameter
  constructor _,_
  --          ^ constructor
  field
    fst : A
    -- <- variable.member

_+_ : ℕ → ℕ → ℕ
-- <- function
zero  + n = n
-- <- function
suc m + n = suc (m + n)

open import Agda.Builtin.Equality using (_≡_)
-- <- keyword.import
--          ^ module

x : ℕ
x = {! !}
--  ^ comment.warning

s = "str"
--  ^ string

record R : Set where
  field
    {impl} : ℕ
    -- ^ variable.member

f : ℕ
f = g
  module Helpers where
  --     ^ module
    g = zero
