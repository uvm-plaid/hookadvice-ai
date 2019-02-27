module Analyze where

import UVMHS
import Syntax

data BDD n a =
    LeafBDD a
  | NodeBDD n (BDD n a) (BDD n a)
makePrettySum ''BDD

-- node ∷ n → BDD n a → BDD n a → BDD n a
-- node l (BDD m x₁ x₂) (BDD n y₁ y₂)
-- -- l m n =
-- -- T T T x₁
-- -- T T F x₁
-- -- T F T x₂
-- -- T F F x₂
-- -- F T T y₁
-- -- F T F y₂
-- -- F F T y₁
-- -- F F F y₂
--   | (l ≡ m) ⩓ (l ≡ n) = NodeBDD l x₁ y₂
--   | (l ≡ m) ⩓ (l < n) = NodeBDD l x₁ (NodeBDD n y₁ y₂)
--   | (l ≡ m) ⩓ (l > n) = NodeBDD n (NodeBDD l x₁ y₁) (NodeBDD l x₁ y₂)
--   | (l < m) ⩓ (l ≡ n) = NodeBDD l (NodeBDD m x₁ x₂) y₂
--   | (l < m) ⩓ (l < n) = NodeBDD l (BDD m x₁ x₂) (BDD n y₁ y₂)
--   | (l < m) ⩓ (l > n) = undefined
--   | (l > m) ⩓ (l ≡ n) = undefined
--   | (l > m) ⩓ (l < n) = undefined
--   | (l > m) ⩓ (l > n) = undefined

instance Functor (BDD n) where
  map f (LeafBDD x) = LeafBDD $ f x
  map f (NodeBDD n x y) = NodeBDD n (map f x) (map f y)

binBDD ∷ (Ord n) ⇒ (a → b → c) → BDD n a → BDD n b → BDD n c
binBDD f (LeafBDD x) (LeafBDD y) = LeafBDD $ x `f` y
binBDD f (NodeBDD n x₁ x₂) (LeafBDD y) = NodeBDD n (map (`f` y) x₁) (map (`f` y) x₂)
binBDD f (LeafBDD x) (NodeBDD n y₁ y₂) = NodeBDD n (map (x `f`) y₁) (map (x `f`) y₂)
binBDD f (NodeBDD m x₁ x₂) (NodeBDD n y₁ y₂)
  | m < n     = NodeBDD m (binBDD f x₁ (NodeBDD n y₁ y₂)) (binBDD f x₂ (NodeBDD n y₁ y₂))
  | m ≡ n     = NodeBDD m (binBDD f x₁ y₁) (binBDD f x₂ y₂)
  | otherwise = NodeBDD n (binBDD f (NodeBDD m x₁ x₂) y₁) (binBDD f (NodeBDD m x₁ x₂) y₂)

instance ToIter a (BDD n a) where
  iter (LeafBDD x) = single x
  iter (NodeBDD _ x y) = iter x ⧺ iter y

instance (Bot l) ⇒ Bot (BDD n l) where bot = LeafBDD bot
instance (Ord n,Join l) ⇒ Join (BDD n l) where (⊔) = binBDD (⊔)
instance (Ord n,POrd l) ⇒ POrd (BDD n l) where x ⊑ y = and $ iter $ binBDD (⊑) x y

type AEvent p = (Lab ∧ Lab) ⇰ p
type CAEvent p = 𝑃 (AEvent p)

-- -- η♯ ∈ event♯ ≜ label × label ↦ security
-- data AEvent p = 
--     PureEv (𝑃 ((Lab ∧ Lab) ⇰ p))
--   | CondEv (𝕊 ∧ 𝕊 ∧ 𝕊) (AEvent p) (AEvent p)
-- 
-- makePrettySum ''AEvent
-- 
-- instance (Ord p) ⇒ Bot (AEvent p) where bot = PureEv pø
-- instance (Ord p) ⇒ Join (AEvent p) where
--   PureEv θAs₁ ⊔ PureEv θAs₂ = PureEv $ θAs₁ ∪ θAs₂
--   CondEv c ae₁ ae₂ ⊔ PureEv θAs = CondEv c (ae₁ ⊔ PureEv θAs) (ae₂ ⊔ PureEv θAs)
--   PureEv θAs ⊔ CondEv c ae₁ ae₂ = CondEv c (PureEv θAs ⊔ ae₁) (PureEv θAs ⊔ ae₂)
--   CondEv c₁ ae₁₁ ae₁₂ ⊔ CondEv c₂ ae₂₁ ae₂₂ 
--     | c₁ < c₂   = CondEv c₁ (ae₁₁ ⊔ CondEv c₂ ae₂₁ ae₂₂) (ae₁₂ ⊔ CondEv c₂ ae₂₁ ae₂₂)
--     | c₁ ≡ c₂   = CondEv c₁ (ae₁₁ ⊔ ae₂₁) (ae₁₂ ⊔ ae₂₂)
--     | otherwise = CondEv c₂ (CondEv c₁ ae₁₁ ae₁₂  ⊔ ae₂₁) (CondEv c₁ ae₁₁ ae₁₂  ⊔ ae₂₂)
-- instance (Ord p) ⇒ POrd (AEvent p) where
--   PureEv θAs₁ ⊑ PureEv θAs₂ = θAs₁ ⊆ θAs₂
--   CondEv _ ae₁ ae₂ ⊑ PureEv θAs = (ae₁ ⊑ PureEv θAs) ⩓ (ae₂ ⊑ PureEv θAs)
--   PureEv θAs ⊑ CondEv _ ae₁ ae₂ = (PureEv θAs ⊑ ae₁) ⩓ (PureEv θAs ⊑ ae₂)
--   CondEv c₁ ae₁₁ ae₁₂ ⊑ CondEv c₂ ae₂₁ ae₂₂ 
--     | c₁ < c₂   = (ae₁₁ ⊑ CondEv c₂ ae₂₁ ae₂₂) ⩓ (ae₁₂ ⊑ CondEv c₂ ae₂₁ ae₂₂)
--     | c₁ ≡ c₂   = (ae₁₁ ⊑ ae₂₁) ⩓ (ae₁₂ ⊑ ae₂₂)
--     | otherwise = (CondEv c₁ ae₁₁ ae₁₂  ⊑ ae₂₁) ⩓ (CondEv c₁ ae₁₁ ae₁₂ ⊑ ae₂₂)

eventA ∷ (Ord p,Top p) ⇒ Event p → BDD (𝕊∧𝕊∧𝕊) (CAEvent p)
eventA = \case
  Flow ℓ₁ ℓ₂ → LeafBDD $ single $ (ℓ₁ :* ℓ₂) ↦ top
  FlowJoin ℓ₁ ℓ₂ α → LeafBDD $ single $ ((ℓ₁ :* LtnL α) ↦ top) ⩌ ((ℓ₂ :* LtnL α) ↦ top)
  FlowMeet ℓ p α → LeafBDD $ single $ (ℓ :* LtnL α) ↦ p
  Check s o c → NodeBDD (s :* o :* c) (LeafBDD (single dø)) (LeafBDD pø)

(⋉) ∷ (Ord p,Join p,Meet p) ⇒ AEvent p → AEvent p → AEvent p
θA₁ ⋉ θA₂ = joins $ 
  [ θA₁
  , θA₂
  , joins $ do
      ℓ₁₁ :* ℓ₁₂ :* p₁ ← list θA₁
      ℓ₂₁ :* ℓ₂₂ :* p₂ ← list θA₂
      case ℓ₁₂ ≡ ℓ₂₁ of
        True → return $ (ℓ₁₁ :* ℓ₂₂) ↦ (p₁ ⊓ p₂)
        False → return dø
    ]

(⋉⋆) ∷ (Ord p,Join p,Meet p) ⇒ CAEvent p → CAEvent p → CAEvent p
θAs₁ ⋉⋆ θAs₂ = joins
  [ θAs₁
  , pow $ do
      θA₁ ← list θAs₁
      θA₂ ← list θAs₂
      return $ θA₁ ⋉ θA₂
  ]

(∝) ∷ (Ord p,Join p,Meet p) ⇒ BDD (𝕊∧𝕊∧𝕊) (CAEvent p) → BDD (𝕊∧𝕊∧𝕊) (CAEvent p) → BDD (𝕊∧𝕊∧𝕊) (CAEvent p)
(∝) = binBDD (⋉⋆)

interpAbsR ∷ (Ord p,Top p,Join p,Meet p) ⇒ 𝕊 ⇰ BDD (𝕊∧𝕊∧𝕊) (CAEvent p) → Eff p → BDD (𝕊∧𝕊∧𝕊) (CAEvent p)
interpAbsR γ = \case
  Emp → LeafBDD pø
  Eps → LeafBDD $ single dø
  Ev η → eventA η
  h₁ :| h₂ → interpAbsR γ h₁ ⊔ interpAbsR γ h₂
  h₁ :⨟ h₂ → interpAbsR γ h₁ ∝ interpAbsR γ h₂
  Var x → γ ⋕! x
  Mu x h → lfp bot $ \ ηA → interpAbsR ((x ↦ ηA) ⩌ γ) h

interpAbs ∷ (Ord p,Top p,Join p,Meet p) ⇒ Eff p → BDD (𝕊∧𝕊∧𝕊) (𝑃 (Lab ∧ p ∧ Lab))
interpAbs h =
  let hAB = interpAbsR dø h
  in mapOn hAB $ \ hA → pow $ do
    ℓ₁  :* ℓ₂ :* p ← list $ joins hA
    return $ ℓ₁ :* p :* ℓ₂

interpAbsPost ∷ (Ord p,Top p,Join p,Meet p) ⇒ Eff p → BDD (𝕊∧𝕊∧𝕊) (𝑃 (Sub ∧ p ∧ Sub))
interpAbsPost h = 
  mapOn (interpAbs h) $ \ hA → pow $ do
    ℓ₁ :* p :* ℓ₂ ← list hA
    case (ℓ₁,ℓ₂) of
      (SubL s₁,SubL s₂) | s₁ ≢ s₂ → return $ s₁ :* p :* s₂
      _ → mzero

printPost ∷ BDD (𝕊∧𝕊∧𝕊) (𝑃 (Sub ∧ Bool ∧ Sub)) → Doc
printPost (LeafBDD rs)
  | isEmpty rs = ppLit "<none>"
  | otherwise = ppVertical $ mapOn (list rs) $ \ (s₁ :* p :* s₂) →
  case p of
    True → ppHorizontal $ list
      [ ppText s₁ , ppPun "↝" , ppText s₂ ]
    False → 
      let pD = ppLit "L"
      in ppHorizontal $ list
        [ ppText s₁ , ppPun "⊓" , pD , ppPun "↝" , ppText s₂ ]
printPost (NodeBDD (s :* o :* p) x y) = ppVertical $ list
  [ concat [ppText "CHECK(",ppText s,ppText ",",ppText o,ppText ",",ppText p,ppText ")"]
  , concat [ppSpace 2,ppAlign $ printPost x]
  , concat [ppText "¬CHECK(",ppText s,ppText ",",ppText o,ppText ",",ppText p,ppText ")"]
  , concat [ppSpace 2,ppAlign $ printPost y]
  ]
