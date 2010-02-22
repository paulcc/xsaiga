module T_G1 (specs, tests) where

import ParserResults hiding (pp)
import PP(ps)
import Extensions


-------- tomita 1: 8 rules -------------

data Label = S | NP | PP | VP deriving (Show, Enum)
instance MemoLabel Label 
-- instance PP Label where pp = text . show




s  = memoize S  $     buildNary <$> np <*> vp
np = memoize NP $     termT "n" 
                  <+> buildNary <$> termT "d" <*> termT "n" 
		  <+> buildNary <$> np <*> pp
pp = memoize PP $     buildNary <$> termT "p" <*> np
vp = memoize VP $     buildNary <$> termT "v" <*> np 
                  <+> buildNary <$> vp <*> pp

start = s

------ END OF tomita 1 ----------

specs
 = [ (,) "T1f"  $ ( (format format_table toEnum .) . runALP start
                  , tests
		  )
   , (,) "T1"   $ ( (format format_result toEnum .) . runALP start
                  , tests
		  )
   ]

--- INPUT -------------------------

tests :: GrammarTests String
tests 
 = [ (,) "pp_ambig" $ Parametric "pp_ambig" $ pdn_sentences
   , (,) "fixed"    $ Sentences             $ test_sentences
   ]

-- for pp ambiguity
pdn_sentences
 = \i -> map (:[]) $ "nvdn" ++ concat (replicate i "pdn")	

test_sentences
 = map words 
   [ "n v d n p d n"
   , "n v d n p d n p d n"
   ]
