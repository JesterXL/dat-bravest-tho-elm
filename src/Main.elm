module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Battle exposing (getRandomNumberFromRange, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random


type alias Model =
    { count : Int
    , randomNumber : Int
    , damage : Int }


initialModel : Model
initialModel =
    { count = 0
    , randomNumber = 0
    , damage = 0 }


type Msg
    = Increment
    | Decrement
    | GetRandomNumber
    | GotRandomNumber Int
    | GetDamage


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            ({ model | count = model.count + 1 }, Cmd.none)

        Decrement ->
            ({ model | count = model.count - 1 }, Cmd.none)

        GetRandomNumber ->
            (model, Random.generate GotRandomNumber (getRandomNumberFromRange 1 4) )
        GotRandomNumber randomInt ->
            ( { model | randomNumber = randomInt }, Cmd.none )
        GetDamage ->
            let
                { power } = fireSpell
                equippedRelics = { leftHand = Earring, rightHand = Earring }
                
                damage = getDamage PlayerMagicalAttack power (MagicPower 1) (Level 1) equippedRelics
            in
            ( { model | damage = damage }, Cmd.none )



view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        , div [][]
        , button [onClick GetRandomNumber ][text "Get Random Number"]
        , div [][text ("Random Number: " ++ (String.fromInt model.randomNumber))]
        , button [onClick GetDamage ][text "Get Magic Damage"]
        , div [][text ("Magic Damage: " ++ (String.fromInt model.damage))]
        ]


init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel , Cmd.none )

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
