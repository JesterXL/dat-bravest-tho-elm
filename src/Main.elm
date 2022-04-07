module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Battle exposing (getRandomNumberFromRange, AttackMissesDeathProtectedTargets(..), Formation(..), Element(..),getHit, HitResult(..), hitResultToString, terraStats, playableTerra, dirk, lockeStats, terraAttacker, lockeTarget, playableLocke, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random
import Task

type alias Model =
    { damage : Int 
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , hitResult : HitResult }


initialModel : Random.Seed -> Model
initialModel seed =
    { damage = 0
    , initialSeed = seed
    , currentSeed = seed
    , hitResult = Miss }


type Msg
    = FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets
    | SwordPhysicalAttackSingleTarget
    | AttemptToHit


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        FireSpellAgainstSingleTarget ->
            let
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) (PlayerMagicalAttack fireSpell) NotElement [] terraAttacker lockeTarget
            in
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )
        FireSpellAgainstMultipleTargets ->
            let
                
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) (PlayerMagicalAttack fireSpell) NotElement [] terraAttacker lockeTarget
            in
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )
        SwordPhysicalAttackSingleTarget ->
            let
                ( damage, newSeed ) = 
                    getDamage model.currentSeed (NormalFormation False) (PlayerPhysicalAttack dirk) NotElement [] terraAttacker lockeTarget
            in
            
            ( { model | damage = damage, currentSeed = newSeed }, Cmd.none )
        AttemptToHit ->
            let
                (hitResult, newSeed) =
                    getHit model.currentSeed (PlayerPhysicalAttack dirk) (NormalFormation False) (AttackMissesDeathProtectedTargets False) terraAttacker lockeTarget
            in
            ( { model | hitResult = hitResult, currentSeed = newSeed }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [][text ("Damage: " ++ (String.fromInt model.damage))]
        , button [onClick FireSpellAgainstSingleTarget ][text "Fire Spell Single Target"]
        , div [][]
        , button [onClick FireSpellAgainstMultipleTargets ][text "Fire Spell Multiple Targets"]
        , div [][]
        , button [onClick SwordPhysicalAttackSingleTarget][text "Sword Physical Attack Single Target"]
        , div [][]
        , div [][text ("Hit Result: " ++ (hitResultToString model.hitResult))]
        , button [onClick AttemptToHit][text "Attempt Hit"]
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
