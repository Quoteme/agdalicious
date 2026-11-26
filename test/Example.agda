module Example where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

-- try typing \bN, \to, \lambda here
id : (A : Set) → A → A
id A = {!!}
