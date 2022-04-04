module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Battle exposing (getRandomNumberFromRange, Formation(..), Element(..), terraStats, playableTerra, dirk, lockeStats, terraAttacker, lockeTarget, playableLocke, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random
import Task

type alias Model =
    { damage : Int 
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed }


initialModel : Random.Seed -> Model
initialModel seed =
    { damage = 0
    , initialSeed = seed
    , currentSeed = seed }


type Msg
    = FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets
    | SwordPhysicalAttackSingleTarget


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        FireSpellAgainstSingleTarget ->
            let
                { power } = fireSpell
                
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) PlayerMagicalAttack NotElement [] power dirk terraAttacker lockeTarget
            in
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )
        FireSpellAgainstMultipleTargets ->
            let
                { power } = fireSpell
                
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) PlayerMagicalMultipleAttack NotElement [] power dirk terraAttacker lockeTarget
            in
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )
        SwordPhysicalAttackSingleTarget ->
            let
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) PlayerPhysicalAttack NotElement [] (SpellPower 0) dirk terraAttacker lockeTarget
            in
            
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )



view : Model -> Html Msg
view model =
    div []
        [ div [][text ("Damage: " ++ (String.fromInt model.damage))]
        , button [onClick FireSpellAgainstSingleTarget ][text "Fire Spell Single Target"]
        , div [][]
        , button [onClick FireSpellAgainstMultipleTargets ][text "Fire Spell Multiple Targets"]
        , div [][]
        , button [onClick SwordPhysicalAttackSingleTarget][text "Sword Physical Attack Single Target"]
        ]


init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel (Random.initialSeed 42) , Cmd.none )

subscriptions : a -> Sub msg
subscriptions _ =
    Sub.none

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
