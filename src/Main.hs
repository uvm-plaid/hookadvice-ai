module Main where

import UVMHS

import Syntax ()
import Parser
import Analyze

main ∷ IO ()
main = do
  as ← args
  case tohs $ list as of
    ("batch":fns) → do
      lg ← mfoldWith fns null $ \ fn lg → do
        s ← read $ fn ⧺ ".eff"
        ts ← tokenizeIO tokEff $ stream $ list $ tokens s
        e ← parseIO (pSkip tokSkip $ pFinal $ pWithContext "exp" parEff) $ stream ts
        let a = interpAbsPost $ extract e
        let s' = printPost a
        write (fn ⧺ ".out") $ pprender $ ppNoFormat s'
        return $ ppVertical $ list
          [ lg
          , ppHeader fn
          , ppHorizontal $ list 
              [ ppBD $ ppText "input:" 
              , pretty $ concat
                  [ execParserContext $ unInputContext $ withContextPrefix $ annotatedTag e
                  , execParserContext $ unExpressionContext $ withContextDisplay $ annotatedTag e
                  ]
              ]
          , ppHorizontal $ list 
              [ ppBD $ ppText "output:" 
              , s' 
              ]
          ]
      pprint lg
    [fn] → do
      do pprint $ ppHeader "READING" ; flushOut
      s ← read $ fn
      do pprint $ ppHeader "TOKENIZING" ; flushOut
      ts ← tokenizeIO tokEff $ stream $ list $ tokens s
      do pprint $ ppHeader "PARSING" ; flushOut
      e ← parseIO (pSkip tokSkip $ pFinal parEff) $ stream ts
      do pprint $ ppHeader "ANALYZING" ; flushOut
      let a = interpAbsPost e
      do pprint $ ppHeader "DONE" ; flushOut
      let s' = pretty $ printPost a
      pprint s' ; flushOut
    _ → do
      pprint $ ppHeader "USAGE"
      out $ "hook <file>"
