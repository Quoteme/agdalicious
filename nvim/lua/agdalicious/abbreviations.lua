-- Starter set of abbreviations, following Agda's emacs input method
-- (agda-input.el). Keys are written without the leading backslash.
-- TODO: generate the full table from agda-input.el (or serve it from the LSP).
local A = {
  -- arrows
  ["to"] = "→", ["->"] = "→", ["r"] = "→", ["<-"] = "←", ["<->"] = "↔",
  ["=>"] = "⇒", ["<="] = "⇐", ["<=>"] = "⇔", ["mapsto"] = "↦", ["|->"] = "↦",
  -- quantifiers and logic
  ["forall"] = "∀", ["all"] = "∀", ["exists"] = "∃", ["ex"] = "∃",
  ["and"] = "∧", ["or"] = "∨", ["neg"] = "¬", ["not"] = "¬",
  ["top"] = "⊤", ["bot"] = "⊥", ["|-"] = "⊢", ["qed"] = "∎",
  -- relations
  ["=="] = "≡", ["=n"] = "≢", ["equiv"] = "≡", ["ne"] = "≠", ["neq"] = "≠",
  ["le"] = "≤", ["leq"] = "≤", ["ge"] = "≥", ["geq"] = "≥",
  ["~"] = "∼", ["~~"] = "≈", ["?="] = "≟", ["::"] = "∷",
  -- sets
  ["in"] = "∈", ["notin"] = "∉", ["sub"] = "⊂", ["subseteq"] = "⊆",
  ["sup"] = "⊃", ["supseteq"] = "⊇", ["cup"] = "∪", ["cap"] = "∩",
  ["u+"] = "⊎", ["uplus"] = "⊎", ["x"] = "×", ["times"] = "×",
  ["emptyset"] = "∅", ["o"] = "∘", ["circ"] = "∘", ["inf"] = "∞", ["infty"] = "∞",
  -- blackboard bold
  ["bN"] = "ℕ", ["bZ"] = "ℤ", ["bQ"] = "ℚ", ["bR"] = "ℝ", ["bC"] = "ℂ", ["bB"] = "𝔹",
  -- brackets
  ["<"] = "⟨", [">"] = "⟩", ["[["] = "⟦", ["]]"] = "⟧", ["{{"] = "⦃", ["}}"] = "⦄",
  -- greek
  ["lambda"] = "λ", ["Gl"] = "λ", ["Ga"] = "α", ["alpha"] = "α", ["Gb"] = "β", ["beta"] = "β",
  ["Gg"] = "γ", ["gamma"] = "γ", ["Gd"] = "δ", ["delta"] = "δ", ["Ge"] = "ε", ["epsilon"] = "ε",
  ["Gm"] = "μ", ["mu"] = "μ", ["Gp"] = "π", ["pi"] = "π", ["Gs"] = "σ", ["sigma"] = "σ",
  ["Gt"] = "τ", ["tau"] = "τ", ["Gf"] = "φ", ["phi"] = "φ", ["Gw"] = "ω", ["omega"] = "ω",
  ["GG"] = "Γ", ["Gamma"] = "Γ", ["GD"] = "Δ", ["Delta"] = "Δ", ["GS"] = "Σ", ["Sigma"] = "Σ",
  ["GP"] = "Π", ["Pi"] = "Π", ["GW"] = "Ω", ["Omega"] = "Ω",
  -- primes
  ["'"] = "′", ["''"] = "″",
}

-- sub- and superscript digits: \_1 -> ₁, \^1 -> ¹
local sub = { "₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉" }
local sup = { "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹" }
for d = 0, 9 do
  A["_" .. d] = sub[d + 1]
  A["^" .. d] = sup[d + 1]
end

return A
