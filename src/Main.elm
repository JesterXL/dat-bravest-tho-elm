module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Browser.Events exposing (onAnimationFrameDelta)
import Battle exposing (getRandomNumberFromRange, AttackMissesDeathProtectedTargets(..), Formation(..), Element(..),getHit, HitResult(..), hitResultToString, terraStats, playableTerra, dirk, lockeStats, terraAttacker, lockeTarget, playableLocke, getDamage, fireSpell, Attack(..), SpellPower(..), MagicPower(..), Level(..), Relic(..), EquippedRelics)
import Random
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color exposing (Color)

type alias Model =
    { damage : Int 
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , hitResult : HitResult 
    , frame : Float
    , sprites : Load Texture }

type Load a
    = Loading
    | Success a
    | Failure

initialModel : Random.Seed -> Model
initialModel seed =
    { damage = 0
    , initialSeed = seed
    , currentSeed = seed
    , hitResult = Miss 
    , frame = 0
    , sprites = Loading }


type Msg
    = FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets
    | SwordPhysicalAttackSingleTarget
    | AttemptToHit
    | AnimationFrame Float
    | TextureLoaded (Maybe Texture)

spriteSheetCellSize =
    24

spriteSheetCellSpace =
    16


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
        AnimationFrame delta ->
            ( { model | frame = model.frame + delta / 10 }
            , Cmd.none
            )
        TextureLoaded textureMaybe ->
            let
                _ = Debug.log "textureMaybe" textureMaybe
            in
            case textureMaybe of
                Nothing ->
                    ({ model | sprites = Failure} , Cmd.none)
                Just loadedText ->
                    ( { model | sprites = Success (Texture.sprite
                                                    { x = 0
                                                    , y = 0
                                                    , width = 16
                                                    , height = 24 
                                                    }
                                                    loadedText) }, Cmd.none )

w : number
w =
    256 * 2
h : number
h =
    224 * 2


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
        , div [][
            Canvas.toHtmlWith
                { width = w
                , height = h
                , textures = textures }
                []
                (shapes [ fill Color.black] [ rect ( 0, 0 ) w h ]
                    :: (case model.sprites of
                            Loading ->
                                [ renderText "Loading sprite sheet" ]

                            Success sprite ->
                                let
                                    _ = Debug.log "drawing image" sprite
                                in
                                [ texture [transform [scale 2.0 2.0] ] ( 30, 30 ) sprite ]

                            Failure ->
                                [ renderText "Failed to load sprite sheet!" ]
                    )
                )

        ]
        ]

textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "src/Sabin.png" TextureLoaded]

renderText : String -> Renderable
renderText txt =
    Canvas.text
        [ font { size = 48, family = "sans-serif" }
        , align Center
        , maxWidth w
        ]
        ( w / 2, h / 2 - 24 )
        txt


init : () -> (Model, Cmd Msg)
init _ =
    ( initialModel (Random.initialSeed 42) , Cmd.none )

subscriptions _ =
    onAnimationFrameDelta AnimationFrame

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
