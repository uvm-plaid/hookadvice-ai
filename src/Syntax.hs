module Syntax where

import UVMHS

-- p ∈ security <parameter>

-- α ∈ location
type Ltn = 𝕊

-- s ∈ subject
type Sub = 𝕊

-- ℓ ∈ label ⩴ α | s
data Lab = LtnL Ltn | SubL Sub
  deriving (Eq,Ord,Show)
makePrettySum ''Lab

-- κ ∈ knowledge ≜ label → security
type Knl p = Lab ⇰ p

-- η ∈ event ⩴ ℓ ↝ ℓ
--           | ℓ ⊔ ℓ ↝ α
--           | ℓ ⊓ p ↝ α
--           | CHECK(s,o,c)
data Event p =
    Flow Lab Lab
  | FlowJoin Lab Lab Ltn
  | FlowMeet Lab p Ltn
  | Check 𝕊 𝕊 𝕊
  deriving (Eq,Ord,Show)
makePrettySum ''Event
  
-- h ∈ Eff ⩴ ∅
--         | ε
--         | η
--         | H ¦ H
--         | H ; H
--         | x
--         | μx.H
data Eff p =
    Emp
  | Eps
  | Ev (Event p)
  | Eff p :| Eff p
  | Eff p :⨟ Eff p
  | Var 𝕊
  | Mu 𝕊 (Eff p)
  deriving (Eq,Ord,Show)
makePrettySum ''Eff
