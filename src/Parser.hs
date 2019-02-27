module Parser where

import UVMHS
import Syntax

data Token =
    TokenName 𝕊
  | TokenLiteral 𝕊
  | TokenComment
  | TokenSpace
  deriving (Eq,Ord,Show)

makePrisms ''Token
makePrettyUnion ''Token

tokKeywords ∷ 𝐿 𝕊
tokKeywords = list
  ["∅","emp"
  ,"ε","eps"
  ,"μ","mu"
  ,"H","L" 
  ,"CHECK"
  ]

tokPunctuation ∷ 𝐿 𝕊
tokPunctuation = list
  ["↝","~>"
  ,"⊔","\\/"
  ,"⊓","/\\"
  ,";","|",".","(",")",","
  ]

tokComment ∷ Parser ℂ ()
tokComment = pNew "comment" $ do
  void $ pWord "--"
  void $ pMany $ pSatisfies "not newline" $ \ c → c ≢ '\n'
  void $ pLit '\n'

tokBlComment ∷ Parser ℂ ()
tokBlComment = pNew "multiline comment" $ do
  () ← void $ pWord "{-"
  afterOther
  where
    afterOther = tries
      [ do () ← void $ pSatisfies "non-delimiter" $ \ c → c ∉ pow ['{','-']
           afterOther
      , do () ← void $ pLit '{'
           afterBrack
      , do () ← void $ pLit '-'
           afterDash
      ]
    afterBrack = tries
      [ do () ← void $ pSatisfies "non-delimiter" $ \ c → c ∉ pow ['{','-']
           afterOther
      , do () ← void $ pLit '{'
           afterBrack
      , do () ← void $ pLit '-'
           () ← afterOther
           afterOther
      ]
    afterDash = tries
      [ do () ← void $ pSatisfies "non-delimiter" $ \ c → c ∉ pow ['{','-','}']
           afterOther
      , do () ← void $ pLit '{'
           afterBrack
      , do () ← void $ pLit '-'
           afterDash
      , do void $ pLit '}'
      ]

tokEff ∷ 𝐿 (Parser ℂ Token)
tokEff = list $ concat
  [ TokenLiteral ^∘ pRender (FG darkYellow) ∘ pRender BD ∘ pWord ^$ tokKeywords
  , TokenLiteral ^∘ pRender (FG darkGray) ∘ pWord ^$ tokPunctuation
  , single $ TokenName ^$ pName
  , map (const TokenComment) ∘ pRender (FG gray) ∘ pRender IT ^$ list [tokComment,tokBlComment]
  , single $ const TokenSpace ^$ pWhitespace
  ]

tokSkip ∷ Token → 𝔹
tokSkip = \case
  TokenSpace → True
  TokenComment → True
  _ → False

parName ∷ Parser Token 𝕊
parName = pShaped "name" $ view tokenNameL

parLit ∷ 𝕊 → Parser Token ()
parLit = void ∘ pLit ∘ TokenLiteral

parSub ∷ Parser Token Sub
parSub = do
  x ← parName
  guard $ (x ⋕! 1) ≡ 's'
  return x

parLtn ∷ Parser Token Ltn
parLtn = do
  x ← parName
  guard $ (x ⋕! 1) ∈ pow ['α','a']
  return x

parLabel ∷ Parser Token Lab
parLabel = tries
  [ SubL ^$ parSub
  , LtnL ^$ parLtn
  ]

parSecurity ∷ Parser Token Bool
parSecurity = tries
  [ do parLit "H" ; return True
  , do parLit "L" ; return False
  ]

parFlow ∷ Parser Token ()
parFlow = tries
  [ parLit "↝"
  , parLit "~>"
  ]

parJoin ∷ Parser Token ()
parJoin = tries
  [ parLit "⊔"
  , parLit "\\/"
  ]

parMeet ∷ Parser Token ()
parMeet = tries
  [ parLit "⊓"
  , parLit "/\\"
  ]

parEvent ∷ Parser Token (Event Bool)
parEvent = tries
  [ do ℓ₁ ← parLabel
       parFlow
       ℓ₂ ← parLabel
       return $ Flow ℓ₁ ℓ₂
  , do ℓ₁ ← parLabel
       parJoin
       ℓ₂ ← parLabel
       parFlow
       α ← parLtn
       return $ FlowJoin ℓ₁ ℓ₂ α
  , do ℓ ← parLabel
       parMeet
       p ← parSecurity
       parFlow
       α ← parLtn
       return $ FlowMeet ℓ p α
  , do parLit "CHECK"
       parLit "("
       s ← parSub
       parLit ","
       o ← parName
       parLit ","
       p ← parName
       parLit ")"
       return $ Check s o p
  ]

parEmp ∷ Parser Token ()
parEmp = tries
  [ parLit "∅"
  , parLit "emp"
  ]

parEps ∷ Parser Token ()
parEps = tries
  [ parLit "ε"
  , parLit "eps"
  ]

parMu ∷ Parser Token ()
parMu = tries
  [ parLit "μ"
  , parLit "mu"
  ]

parVar ∷ Parser Token 𝕊
parVar = do
  x ← parName
  guard $ (x ⋕! 1) ∉ pow ['s','α','a']
  return x



parEff ∷ Parser Token (Eff Bool)
parEff = mixfixParser $ concat
  [ mix $ MixTerminal $ do
      parLit "("
      h ← parEff
      parLit ")"
      return h
  , mix $ MixTerminal $ const Emp ^$ parEmp
  , mix $ MixTerminal $ const Eps ^$ parEps
  , mix $ MixTerminal $ Var ^$ parVar
  , mix $ MixTerminal $ Ev ^$ parEvent
  , mix $ MixInfixL 5 $ do
      parLit "|"
      return (:|)
  , mix $ MixInfixL 6 $ do
      parLit ";"
      return (:⨟)
  , mix $ MixPrefix 2 $ do
      parMu
      x ← parVar
      parLit "."
      return $ Mu x
  ]
