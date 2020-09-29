module TreesThatGrow.Domain where

import Data.Text
import Data.Map

data LocalizationStatus = Unlocalized | Localized

data User localizationStatus = User {
  firstName :: Text,
  lastName :: Text,
  shoppingBasket :: Map Int (Item localizationStatus)
}

data Item localizationStatus = Item {
  identifier :: Int,
  name :: ItemName localizationStatus
}

type family ItemName (localizationStatus :: LocalizationStatus)
type instance ItemName 'Unlocalized = ()
type instance ItemName 'Localized = Text