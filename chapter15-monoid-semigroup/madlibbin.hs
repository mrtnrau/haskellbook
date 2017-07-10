module MadLibbin where

import           Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' ex adv noun adj =
  ex <> "! he said: " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinbetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinbetter' ex adv noun adj = mconcat libbin
  where libbin = [ ex, "! he sait: "
                 , adv, " as he jumped into his car "
                 , noun, "and drove off with his "
                 , adj, " wife."]
