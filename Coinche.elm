module Coinche where

import Html exposing (text)
import Generic exposing (cartesian)
import Random exposing (..)

import String


import Debug

type alias Carte = (Rang,Couleur)
type Couleur = Coeur | Carreau | Pique | Trefle
type Rang = As | Roi | Dame | Valet | Dix | Neuf | Huit | Sept

type alias Donne = List Carte

couleurs = [Coeur , Carreau , Pique , Trefle]
rangs = [As , Roi , Dame , Valet , Dix , Neuf , Huit , Sept]
jeu = cartesian (,) rangs couleurs

melange unJeu =
    let melangeAvec (seed,unJeu) =
        case unJeu of
            [] -> (seed,[])
            [carte] -> (seed,[carte])
            prems :: reste ->
                let longueur = (List.length reste)-1
                    (index,seed') = generate (int 0 longueur) seed
                    (debut,fin) = (List.take index reste, List.drop index reste)
                    (Just sorti) = List.head fin
                    nouveauReste = List.concat [debut, prems :: (List.drop 1 fin)]
                    (seed'',resteMelange) = melangeAvec (seed',nouveauReste)
                in (seed',sorti :: resteMelange)
    in snd <| melangeAvec (Random.initialSeed 0,unJeu)

donne = List.take 8 (melange jeu)

main = evaluer donne

evaluer : Donne -> Html.Html
evaluer uneDonne =
    text <| String.concat ["Je passe, avec ", toString uneDonne]