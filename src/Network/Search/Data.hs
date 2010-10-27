
module Network.Search.Data
       ( SearchParameter(..)
       ) where

data SearchParameter = SortParameter String Bool
                     | GroupField String
                     | PagingFilter Int Int
                     | FacetFilter String String
                     | FieldSearch String String
                     | Keyword String
