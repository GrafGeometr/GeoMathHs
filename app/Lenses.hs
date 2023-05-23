module Lenses (
    makeLenses, makeAllLenses,
    module Control.Lens
) where

import Control.Lens hiding (makeLenses)
import Language.Haskell.TH (mkName,  nameBase, Name, DecsQ)

makeLenses :: Name -> DecsQ
makeLenses = makeLensesWith $ lensRules & lensField .~ \_ _ n -> [TopName . mkName $ '_':nameBase n]

makeAllLenses :: [Name] -> DecsQ
makeAllLenses = fmap concat . traverse makeLenses
