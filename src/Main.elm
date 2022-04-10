module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Battle exposing (getRandomNumberFromRange, AttackMissesDeathProtectedTargets(..), Formation(..), Element(..),getHit, HitResult(..), hitResultToString, terraStats, playableTerra, dirk, lockeStats, terraAttacker, lockeTarget, playableLocke, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random
import Animator
import Animator.Inline
import Time

type alias Model =
    { damage : Int 
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , hitResult : HitResult 
    , paused : Bool
    , faded : Animator.Timeline Bool }

animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            -- we tell the animator how
            -- to get the checked timeline using .checked
            .faded
            -- and we tell the animator how
            -- to update that timeline as well
            (\newFaded model ->
                { model | faded = newFaded }
            )

initialModel : Random.Seed -> Model
initialModel seed =
    { damage = 0
    , initialSeed = seed
    , currentSeed = seed
    , hitResult = Miss 
    , faded = Animator.init False
    , paused = False }


type Msg
    = FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets
    | SwordPhysicalAttackSingleTarget
    | AttemptToHit
    | TogglePause
    | Tick Time.Posix
    | Fade Bool

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
        
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none)
        Tick newTime ->
            ( model |> Animator.update newTime animator, Cmd.none )
        Fade newFaded ->
            ( { model | faded =
                            model.faded
                                |> Animator.go Animator.slowly newFaded}, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ div [][Html.text ("Damage: " ++ (String.fromInt model.damage))]
        , button [onClick FireSpellAgainstSingleTarget ][Html.text "Fire Spell Single Target"]
        , div [][]
        , button [onClick FireSpellAgainstMultipleTargets ][Html.text "Fire Spell Multiple Targets"]
        , div [][]
        , button [onClick SwordPhysicalAttackSingleTarget][Html.text "Sword Physical Attack Single Target"]
        , div [][]
        , div [][Html.text ("Hit Result: " ++ (hitResultToString model.hitResult))]
        , button [onClick AttemptToHit][Html.text "Attempt Hit"]
        , div [][]
        , button [onClick (Fade False)][Html.text "Fade False"]
        , div [][]
        , button [onClick (Fade True)][Html.text "Fade True"]
        , div [][
            
            img [src "src/Sabin.png"
            , Animator.Inline.xy
                model.faded
                (\ faded -> if faded == False then
                    { x = Animator.at 0, y = Animator.at 0 }
                else
                    { x = Animator.at 120, y = Animator.at 0 }
                )
                            
            ][]
                        
        ]
        ]


init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel (Random.initialSeed 42) , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    animator
    |> Animator.toSubscription Tick model

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
