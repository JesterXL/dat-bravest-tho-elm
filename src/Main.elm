module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas.Texture exposing (sprite)
import Color
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Html.Lazy
import Random
import Time


type alias Model =
    { damage : Int
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , hitResult : HitResult
    , paused : Bool
    , faded : Animator.Timeline Bool
    , encounter : Maybe Encounter
    , time : Int
    }


type alias Encounter =
    { enemies : List SpriteMonster
    , formation : Formation
    }


type alias SpriteMonster =
    { stats : MonsterStats
    , image : String
    , width : Int
    , height : Int
    }


basicEncounter : Encounter
basicEncounter =
    { enemies = [ rhobite, rhobite, rhobite ]
    , formation = NormalFormation False
    }


rhobite : SpriteMonster
rhobite =
    { stats =
        { type_ = Rhobite
        , level = Level 10
        , hitPoints = HitPoints 135
        , magicPoints = MagicPoints 40
        , xp = XP 53
        , gold = Gold 110
        , battlePower = BattlePower 9
        , vigor = Vigor 10
        , hitRate = HitRate 100
        , defense = Defense 70
        , evade = Evade 0
        , magicPower = MagicPower 10
        , speed = Speed 30
        , stamina = Stamina 0
        , magicDefense = MagicDefense 140
        , mblock = MBlock 0
        , stolenItems = [ Potion ]
        , droppedItems = [ Tonic, Potion, DriedMeat ]
        , absorbs = []
        , noEffect = []
        , weak = [ WaterElement ]
        }
    , image = "src/rhobite.png"
    , width = 32
    , height = 32
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching
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
    , paused = False
    , encounter = Just basicEncounter
    , time = 0
    }


type Msg
    = FireSpellAgainstSingleTarget
    | FireSpellAgainstMultipleTargets
    | SwordPhysicalAttackSingleTarget
    | AttemptToHit
    | TogglePause
    | Tick Time.Posix
    | Fade Bool
    | Frame Float


update : Msg -> Model -> ( Model, Cmd Msg )
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
                ( hitResult, newSeed ) =
                    getHit model.currentSeed (PlayerPhysicalAttack dirk) (NormalFormation False) (AttackMissesDeathProtectedTargets False) terraAttacker lockeTarget
            in
            ( { model | hitResult = hitResult, currentSeed = newSeed }, Cmd.none )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Tick newTime ->
            let
                updatedTime =
                    model.time + Time.posixToMillis newTime

                _ =
                    Debug.log "updatedTime" updatedTime
            in
            ( model |> Animator.update newTime animator, Cmd.none )

        Frame timePassed ->
            let
                _ =
                    Debug.log "timePassed" timePassed

                newTime =
                    Time.millisToPosix (round timePassed)
            in
            ( model |> Animator.update newTime animator, Cmd.none )

        Fade newFaded ->
            ( { model
                | faded =
                    model.faded
                        |> Animator.go (Animator.seconds 3) newFaded
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , viewEcounter model
        ]


viewEcounter : Model -> Html Msg
viewEcounter model =
    case model.encounter of
        Nothing ->
            div [] [ text "Waiting for Encounter to be created." ]

        Just encounter ->
            div []
                [ viewEncounterEnemies encounter
                , viewSabinMove model
                ]


viewEncounterEnemies : Encounter -> Html Msg
viewEncounterEnemies encounter =
    div []
        (List.indexedMap
            viewEnemy
            encounter.enemies
        )


viewEnemy : Int -> SpriteMonster -> Html Msg
viewEnemy index enemy =
    Animator.Css.node "div"
        (Animator.init True)
        [ Animator.Css.transform <|
            \state ->
                Animator.Css.xy
                    { x = 0
                    , y = (toFloat index + 1) * 32
                    }
        ]
        [ style "position" "absolute"
        , style "top" "0px"
        , style "top" "0px"

        -- , style "width" (String.fromInt enemy.width ++ "px")
        -- , style "height" (String.fromInt enemy.height ++ "px")
        , style "width" "32px"
        , style "height" "32px"
        , style "background-image" ("url('" ++ enemy.image ++ "')")
        , style "background-repeat" "no-repeat"

        -- , style "background-position" "-20px -62px"
        , class "pixel-art"
        ]
        []


viewSabinMove : Model -> Html Msg
viewSabinMove model =
    Animator.Css.node "div"
        model.faded
        [ Animator.Css.transform <|
            \state ->
                case state of
                    False ->
                        Animator.Css.xy
                            { x = 120
                            , y = 70
                            }

                    True ->
                        Animator.Css.xy
                            { x = 720
                            , y = 0
                            }
        ]
        [ style "position" "absolute"
        , style "top" "0px"
        , style "top" "0px"
        , style "width" "16px"
        , style "height" "24px"
        , style "background-image" "url('src/Sabin.png')"
        , style "background-repeat" "no-repeat"

        -- , style "transform-origin" "30% 50%"
        , style "background-position" "-20px -62px"
        , class "pixel-art"
        ]
        []


pauseButton : Bool -> Html Msg
pauseButton paused =
    if paused == True then
        button [ onClick TogglePause ] [ Html.text "Unpause" ]

    else
        button [ onClick TogglePause ] [ Html.text "Pause" ]


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');
body, html {
    margin: 0;
    padding:0;
    border:0;
    display:block;
    position: relative;
    width: 100%;
    height: 100%;
}
.pixel-art {
    image-rendering: pixelated;
    image-rendering: -moz-crisp-edges;
    image-rendering: crisp-edges;
}
""" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel (Random.initialSeed 42), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if model.paused == False then
    --     animator
    --         |> Animator.toSubscription Tick model
    -- else
    --     Sub.none
    Sub.batch [ onAnimationFrameDelta Frame ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
