module Main exposing (main)

import Animator
import Animator.Css
import Animator.Inline
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), CharacterStats, Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, PlayableCharacter, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableSabin, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (sprite)
import Color
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Html.Lazy
import Random
import Time


type alias Model =
    { initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , hitResult : HitResult
    , paused : Bool
    , faded : Animator.Timeline Bool
    , encounter : Maybe Encounter
    , time : Int
    , battleTimer : BattlerTimer
    , sprites : Load Sprites
    }


type Load a
    = Loading
    | Success a
    | Failure


type alias Sprites =
    { sabin : Canvas.Texture.Texture }


type alias Encounter =
    { enemies : List SpriteMonster
    , characters : List SpriteCharacter
    , formation : Formation
    , state : EncounterState
    }


type EncounterState
    = ATBsCharging
    | BattleAnimation
    | CharacterSelecting


type alias SpriteMonster =
    { stats : MonsterStats
    , image : String
    , width : Int
    , height : Int
    }


type alias SpriteCharacter =
    { playableCharacter : PlayableCharacter
    , image : String
    , width : Int
    , height : Int
    , atbGauge : ATBGauge
    }


basicEncounter : Encounter
basicEncounter =
    { enemies = [ rhobite, rhobite, rhobite ]
    , characters = [ sabin ]
    , formation = NormalFormation False
    , state = ATBsCharging
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
    , image = "rhobite.png"
    , width = 32
    , height = 32
    }


sabin : SpriteCharacter
sabin =
    { playableCharacter = playableSabin
    , image = "Sabin.png"
    , width = 16
    , height = 24
    , atbGauge = ATBGaugeCharging 0
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
    { initialSeed = seed
    , currentSeed = seed
    , hitResult = Miss
    , faded = Animator.init False
    , paused = False
    , encounter = Just basicEncounter
    , time = 0
    , battleTimer = { millisecondsPassed = 0, counter = 0 }
    , sprites = Loading
    }


type alias BattlerTimer =
    { millisecondsPassed : Int
    , counter : Int
    }


type ATBGauge
    = ATBGaugeCharging Int
    | ATBGaugeReady


type Msg
    = TogglePause
    | Tick Time.Posix
    | Fade Bool
    | Frame Float
    | TextureLoaded (Maybe Canvas.Texture.Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Tick newTime ->
            let
                updatedTime =
                    model.time + Time.posixToMillis newTime

                -- _ =
                --     Debug.log "updatedTime" updatedTime
            in
            ( model |> Animator.update newTime animator, Cmd.none )

        Frame timePassed ->
            let
                timePassedInt =
                    round timePassed

                currentBattleTimer =
                    model.battleTimer

                updatedBattleTimerMilliseconds =
                    { currentBattleTimer
                        | millisecondsPassed =
                            model.battleTimer.millisecondsPassed + timePassedInt
                    }

                updatedBattleTimerTimer =
                    { updatedBattleTimerMilliseconds
                        | counter = updatedBattleTimerMilliseconds.millisecondsPassed // 30
                        , millisecondsPassed = remainderBy 30 updatedBattleTimerMilliseconds.millisecondsPassed
                    }

                updatedEncounter =
                    case model.encounter of
                        Nothing ->
                            Nothing

                        Just encounter ->
                            let
                                updatedEncounterLoop =
                                    List.repeat updatedBattleTimerTimer.counter 0
                                        |> List.foldl
                                            (\_ encounterAccum ->
                                                { encounterAccum
                                                    | characters = updateATBGauges encounter.characters
                                                }
                                            )
                                            encounter

                                -- _ =
                                --     Debug.log "updatedEncounterLoop" updatedEncounterLoop.characters
                            in
                            Just updatedEncounterLoop

                -- updateATBGauges
                -- _ =
                --     Debug.log "updatedBattleTimerTimer" updatedBattleTimerTimer
                -- (96 * (Speed + 20)) / 16
                -- 65536
                updatedModel =
                    { model
                        | battleTimer = updatedBattleTimerTimer
                        , time = model.time + timePassedInt
                        , encounter = updatedEncounter
                    }
            in
            ( updatedModel |> Animator.update (Time.millisToPosix timePassedInt) animator, Cmd.none )

        Fade newFaded ->
            ( { model
                | faded =
                    model.faded
                        |> Animator.go (Animator.seconds 3) newFaded
              }
            , Cmd.none
            )

        TextureLoaded Nothing ->
            ( { model | sprites = Failure }, Cmd.none )

        TextureLoaded (Just texture) ->
            ( { model
                | sprites =
                    Success
                        { sabin =
                            Canvas.Texture.sprite
                                { x = 20
                                , y = 62
                                , width = 16
                                , height = 24
                                }
                                texture
                        }
              }
            , Cmd.none
            )



-- This is wrong... I need to keep track of
-- the whole timer internally, then reset it each
-- time I get a count
-- incrementBattleTimer : Int -> ( Int, Int )
-- incrementBattleTimer totalGameTime =
--     let
--         counts =
--             totalGameTime // 30
--         remainderTime =
--             remainderBy 30 totalGameTime
--     in
--     ( counts, remainderTime )
-- (96 * (Speed + 20)) / 16


updateATBGauges : List SpriteCharacter -> List SpriteCharacter
updateATBGauges spriteCharacters =
    List.map
        (\spriteCharacter ->
            case spriteCharacter.atbGauge of
                ATBGaugeCharging currentCharge ->
                    if currentCharge < 65536 then
                        case spriteCharacter.playableCharacter.stats.speed of
                            Speed speed ->
                                let
                                    updatedCharge =
                                        currentCharge + (96 * (speed + 20)) // 16
                                in
                                if updatedCharge >= 65536 then
                                    { spriteCharacter | atbGauge = ATBGaugeReady }

                                else
                                    { spriteCharacter | atbGauge = ATBGaugeCharging updatedCharge }

                    else
                        spriteCharacter

                ATBGaugeReady ->
                    spriteCharacter
        )
        spriteCharacters


view : Model -> Html Msg
view model =
    -- viewEcounter model
    drawCanvas model



-- , pauseButton model.paused
-- , viewMenu model


drawCanvas : Model -> Html Msg
drawCanvas model =
    case model.encounter of
        Nothing ->
            Canvas.toHtmlWith
                { width = 300
                , height = 200
                , textures = []
                }
                [ class "pixel-art" ]
                []

        Just encounter ->
            Canvas.toHtmlWith
                { width = 300
                , height = 200
                , textures = [ Canvas.Texture.loadFromImageUrl "Sabin.png" TextureLoaded ]
                }
                []
                (shapes [ fill (Color.rgb 0.85 0.92 1) ] [ rect ( 0, 0 ) 300 200 ]
                    :: (case model.sprites of
                            Loading ->
                                [ Canvas.text
                                    [ font { size = 48, family = "sans-serif" }, align Center ]
                                    ( 50, 50 )
                                    "Loading..."
                                ]

                            Success sprites ->
                                [ Canvas.texture
                                    []
                                    ( 200, 100 )
                                    sprites.sabin
                                ]

                            Failure ->
                                [ Canvas.text
                                    [ font { size = 48, family = "sans-serif" }, align Center ]
                                    ( 50, 50 )
                                    "Failed to load textures."
                                ]
                       )
                    ++ List.concatMap
                        (\spriteCharacter ->
                            drawBar spriteCharacter.atbGauge
                        )
                        encounter.characters
                )


drawBar : ATBGauge -> List Canvas.Renderable
drawBar atbGauge =
    case atbGauge of
        ATBGaugeCharging currentCharge ->
            let
                percentage =
                    toFloat currentCharge / 65536.0 * 100
            in
            [ shapes [ fill Color.yellow ] [ rect ( 0, 0 ) percentage 20 ]
            , shapes [ stroke Color.black ] [ rect ( 0, 0 ) 100 20 ]
            ]

        ATBGaugeReady ->
            [ shapes [ fill Color.yellow, stroke Color.black ] [ rect ( 0, 0 ) 100 20 ] ]


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
        , style "background-image" "url('Sabin.png')"
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


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "w-[300px] h-[320px] bg-blue-800 border-4 border-solid rounded-md" ]
        [ button [ class "text-white" ]
            [ Html.text "Attack" ]
        , button
            []
            [ Html.text "Magic" ]
        , button
            []
            [ Html.text "Items" ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel (Random.initialSeed 42), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused == False then
        -- animator |> Animator.toSubscription Tick model
        Sub.batch [ onAnimationFrameDelta Frame ]

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
