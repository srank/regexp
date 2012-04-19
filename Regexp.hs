module Regexp (Regexp(Literal, 
                      AnyChar,
                      Or, 
                      OneOrMore, 
                      ZeroOrMore,
                      Sequence,
                      Optional, 
                      AtEnd, 
                      AtStart)) where

data Regexp = Literal String | 
              AnyChar |
              Or Regexp Regexp |
              OneOrMore Regexp |
              ZeroOrMore Regexp |
              Sequence Regexp Regexp |
              Optional Regexp |            
              AtStart Regexp |
              AtEnd Regexp
              deriving (Eq, Show)

