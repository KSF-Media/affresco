module Mosaico.Routes where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match, lit, root, str)

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | ArticlePage String
  | MenuPage
derive instance eqR :: Eq MosaicoPage

frontpageRoute :: Match MosaicoPage
frontpageRoute = Frontpage <$ root

articleRoute :: Match MosaicoPage
articleRoute = ArticlePage <$> (lit "" *> lit "artikel" *> str)

menuRoute :: Match MosaicoPage
menuRoute = MenuPage <$ lit "" <* lit "meny"

routes :: Match MosaicoPage
routes =
  articleRoute <|> menuRoute <|> frontpageRoute