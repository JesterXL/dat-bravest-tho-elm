module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Battle exposing (getRandomNumberFromRange, terra, playableTerra, locke, playableLocke, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random
import Task

type alias Model =
    { count : Int
    , damage : Int 
    , initialSeed : Random.Seed }


initialModel : Random.Seed -> Model
initialModel seed =
    { count = 0
    , damage = 0
    , initialSeed = seed }


type Msg
    = Increment
    | Decrement
    | FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            ({ model | count = model.count + 1 }, Cmd.none)

        Decrement ->
            ({ model | count = model.count - 1 }, Cmd.none)

        FireSpellAgainstSingleTarget ->
            let
                { power } = fireSpell
                
                damage = getDamage model.initialSeed PlayerMagicalAttack power (MagicPower 1) (Level 1) playableTerra playableLocke
            in
            ( { model | damage = damage }, Cmd.none )
        FireSpellAgainstMultipleTargets ->
            let
                { power } = fireSpell
                equippedRelics = { leftHand = Earring, rightHand = Earring }
                
                damage = getDamage model.initialSeed PlayerMagicalMultipleAttack power (MagicPower 1) (Level 1) playableTerra playableLocke
            in
            ( { model | damage = damage }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        , div [][]
        , button [onClick FireSpellAgainstSingleTarget ][text "Fire Spell Single Target"]
        , div [][text ("Magic Damage: " ++ (String.fromInt model.damage))]
        , button [onClick FireSpellAgainstMultipleTargets ][text "Fire Spell Multiple Targets"]
        , div [][text ("Magic Damage: " ++ (String.fromInt model.damage))]
        ]


init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel (Random.initialSeed 42) , Cmd.none )

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
