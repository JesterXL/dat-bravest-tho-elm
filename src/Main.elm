module Main exposing (main)

import Animator exposing (color)
import Array exposing (Array)
import Battle exposing (Attack(..), AttackMissesDeathProtectedTargets(..), BattlePower(..), CharacterStats, Defense(..), Element(..), EquippedRelics, Evade(..), Formation(..), Gold(..), HitPoints(..), HitRate(..), HitResult(..), Item(..), Level(..), MBlock(..), MagicDefense(..), MagicPoints(..), MagicPower(..), Monster(..), MonsterStats, PlayableCharacter, Relic(..), Speed(..), SpellPower(..), Stamina(..), Vigor(..), XP(..), dirk, fireSpell, getDamage, getHit, getRandomNumberFromRange, hitResultToString, lockeStats, lockeTarget, playableLocke, playableSabin, playableTerra, terraAttacker, terraStats)
import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Canvas exposing (Point, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Text exposing (TextAlign(..), align, font)
import Canvas.Texture exposing (sprite)
import Color
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Time


type alias Model =
    { initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , paused : Bool
    , time : Int
    , gameSetupStatus : GameSetupStatus
    , battleTimer : BattleTimer
    , enemies : List SpriteMonster
    , characters : List SpriteCharacter
    , formation : Formation
    , battleState : BattleState
    , queuedActions : List BattleAction
    , leftPressed : Bool
    , rightPressed : Bool
    , upPressed : Bool
    , downPressed : Bool
    , menuItems : List MenuItem
    , selectionTargets : Array Point
    , selectionIndex : Int
    , selectionTarget : Maybe Point
    }


type alias MenuItem =
    { x : Float, y : Float, text : String }


type alias Point =
    { x : Float, y : Float }


type GameSetupStatus
    = SettingUp
    | SetupComplete Sprites
    | SetupFailed


type BattleState
    = Intro
    | Charging
    | CharacterOrMonsterReady
    | CharacterMagicOrAbilityOrItem
    | MonsterDying
    | AllCharactersDied


type BattleAction
    = Attack
    | Spell
    | Item
    | Defend
    | Abiity


type alias Sprites =
    { cursor : Canvas.Texture.Texture
    , sabin : Canvas.Texture.Texture
    , rhobite : Canvas.Texture.Texture
    }


charactersATBIsReady : List SpriteCharacter -> Bool
charactersATBIsReady characters =
    let
        readyCharacters =
            List.filter
                (\spriteCharacter ->
                    case spriteCharacter.atbGauge of
                        ATBGaugeReady ->
                            True

                        ATBGaugeCharging _ ->
                            False
                )
                characters
    in
    List.isEmpty readyCharacters == False


type alias SpriteMonster =
    { stats : MonsterStats
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias SpriteCharacter =
    { playableCharacter : PlayableCharacter
    , image : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , atbGauge : ATBGauge
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
    , x = 20
    , y = 40
    , width = 32
    , height = 32
    }


sabin : SpriteCharacter
sabin =
    { playableCharacter = playableSabin
    , image = "Sabin.png"
    , x = 200
    , y = 100
    , width = 16
    , height = 24
    , atbGauge = ATBGaugeCharging 0
    }


initialModel : Random.Seed -> Model
initialModel seed =
    { initialSeed = seed
    , currentSeed = seed
    , paused = False
    , time = 0
    , gameSetupStatus = SettingUp
    , battleTimer = { millisecondsPassed = 0, counter = 0 }
    , enemies = [ rhobite, rhobite, rhobite ]
    , characters = [ sabin ]
    , formation = NormalFormation False
    , battleState = Intro
    , queuedActions = []
    , leftPressed = False
    , rightPressed = False
    , upPressed = False
    , downPressed = False
    , menuItems = [ { x = 50, y = 220, text = "Attack" }, { x = 50, y = 240, text = "Magic" }, { x = 50, y = 260, text = "Items" } ]
    , selectionTargets = Array.fromList [ { x = 50, y = 220 }, { x = 50, y = 240 }, { x = 50, y = 260 } ]
    , selectionIndex = 0
    , selectionTarget = Nothing
    }


type alias BattleTimer =
    { millisecondsPassed : Int
    , counter : Int
    }


type ATBGauge
    = ATBGaugeCharging Int
    | ATBGaugeReady


type Direction
    = LeftPressed
    | RightPressed
    | LeftReleased
    | RightReleased
    | UpPressed
    | UpReleased
    | DownPressed
    | DownReleased
    | EnterPressed
    | EnterReleased
    | Other


keyDecoderPressed : Decode.Decoder Msg
keyDecoderPressed =
    Decode.map toDirectionPressed (Decode.field "key" Decode.string)


toDirectionPressed : String -> Msg
toDirectionPressed string =
    let
        _ =
            Debug.log "key pressed" string
    in
    case string of
        "ArrowLeft" ->
            MoveCursor LeftPressed

        "ArrowRight" ->
            MoveCursor RightPressed

        "ArrowUp" ->
            MoveCursor UpPressed

        "ArrowDown" ->
            MoveCursor DownPressed

        "Enter" ->
            MoveCursor EnterPressed

        _ ->
            MoveCursor Other


toDirectionReleased : String -> Msg
toDirectionReleased string =
    case string of
        "ArrowLeft" ->
            MoveCursor LeftReleased

        "ArrowRight" ->
            MoveCursor RightReleased

        "ArrowUp" ->
            MoveCursor UpReleased

        "ArrowDown" ->
            MoveCursor DownReleased

        "Enter" ->
            MoveCursor EnterReleased

        _ ->
            MoveCursor Other


keyDecoderReleased : Decode.Decoder Msg
keyDecoderReleased =
    Decode.map toDirectionReleased (Decode.field "key" Decode.string)


selectNextCursorTarget : Direction -> Model -> Model
selectNextCursorTarget direction model =
    case direction of
        LeftPressed ->
            let
                ( updatedTarget, updatedIndex ) =
                    if model.selectionIndex > 0 then
                        ( Array.get (model.selectionIndex - 1) model.selectionTargets, model.selectionIndex - 1 )

                    else
                        ( Array.get (Array.length model.selectionTargets - 1) model.selectionTargets, Array.length model.selectionTargets - 1 )
            in
            { model | selectionTarget = updatedTarget, selectionIndex = updatedIndex }

        RightPressed ->
            let
                ( updatedTarget, updatedIndex ) =
                    if model.selectionIndex > Array.length model.selectionTargets - 1 then
                        ( Array.get 0 model.selectionTargets, 0 )

                    else
                        ( Array.get (model.selectionIndex + 1) model.selectionTargets, model.selectionIndex + 1 )
            in
            { model | selectionTarget = updatedTarget, selectionIndex = updatedIndex }

        -- DownPressed ->
        -- UpPressed ->
        _ ->
            model


type Msg
    = TogglePause
    | Frame Float
    | TextureLoaded (Maybe Canvas.Texture.Texture)
    | MoveCursor Direction
    | VisibilityChange Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Frame timePassed ->
            case model.gameSetupStatus of
                SettingUp ->
                    ( model, Cmd.none )

                SetupFailed ->
                    ( model, Cmd.none )

                SetupComplete sprites ->
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

                        -- updatedCharacters =
                        --     List.repeat updatedBattleTimerTimer.counter 0
                        --         |> List.map
                        --             (\character  ->
                        --                 { encounterAccum
                        --                     | characters = updateATBGauges model.encounter.characters
                        --                 }
                        --             )
                        --             model.characters
                        updatedCharacters =
                            updateATBGauges model.characters

                        updatedModel =
                            { model
                                | time = model.time + timePassedInt
                                , gameSetupStatus = SetupComplete sprites
                                , battleTimer = updatedBattleTimerTimer
                                , characters = updatedCharacters
                            }
                    in
                    if charactersATBIsReady updatedCharacters then
                        ( { updatedModel | battleState = CharacterOrMonsterReady }, Cmd.none )

                    else
                        ( updatedModel, Cmd.none )

        TextureLoaded Nothing ->
            ( { model | gameSetupStatus = SetupFailed }, Cmd.none )

        TextureLoaded (Just texture) ->
            ( { model
                | gameSetupStatus =
                    SetupComplete
                        { cursor =
                            Canvas.Texture.sprite
                                { x = 68
                                , y = 217
                                , width = 32
                                , height = 32
                                }
                                texture
                        , sabin =
                            Canvas.Texture.sprite
                                { x = 20
                                , y = 62
                                , width = 16
                                , height = 24
                                }
                                texture
                        , rhobite =
                            Canvas.Texture.sprite
                                { x = 140
                                , y = 200
                                , width = 32
                                , height = 32
                                }
                                texture
                        }
                , battleTimer = { millisecondsPassed = 0, counter = 0 }
              }
            , Cmd.none
            )

        MoveCursor direction ->
            case direction of
                LeftPressed ->
                    ( selectNextCursorTarget direction { model | leftPressed = True }, Cmd.none )

                RightPressed ->
                    ( selectNextCursorTarget direction { model | rightPressed = True }, Cmd.none )

                LeftReleased ->
                    ( selectNextCursorTarget direction { model | leftPressed = False }, Cmd.none )

                RightReleased ->
                    ( selectNextCursorTarget direction { model | rightPressed = False }, Cmd.none )

                UpPressed ->
                    ( selectNextCursorTarget direction { model | upPressed = True }, Cmd.none )

                UpReleased ->
                    ( selectNextCursorTarget direction { model | upPressed = False }, Cmd.none )

                DownPressed ->
                    ( selectNextCursorTarget direction { model | downPressed = True }, Cmd.none )

                DownReleased ->
                    ( selectNextCursorTarget direction { model | downPressed = False }, Cmd.none )

                EnterPressed ->
                    ( model, Cmd.none )

                EnterReleased ->
                    ( model, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        VisibilityChange vis ->
            case vis of
                Hidden ->
                    ( { model | paused = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False }, Cmd.none )


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
    case model.gameSetupStatus of
        SettingUp ->
            Canvas.toHtmlWith
                { width = gameWidth
                , height = gameHeight
                , textures = [ Canvas.Texture.loadFromImageUrl "Sabin.png" TextureLoaded ]
                }
                []
                [ Canvas.text
                    [ font { size = 48, family = "sans-serif" }, align Center ]
                    ( 50, 50 )
                    "Loading textures..."
                ]

        SetupFailed ->
            Canvas.toHtmlWith
                { width = gameWidth
                , height = gameHeight
                , textures = []
                }
                []
                [ Canvas.text
                    [ font { size = 48, family = "sans-serif" }, align Center ]
                    ( 50, 50 )
                    "Setup failed."
                ]

        SetupComplete sprites ->
            Canvas.toHtmlWith
                { width = gameWidth
                , height = gameHeight
                , textures = []
                }
                []
                (shapes [ fill (Color.rgb 0.85 0.92 1) ] [ rect ( 0, 0 ) gameWidthFloat gameHeightFloat ]
                    :: [ Canvas.texture
                            []
                            ( toFloat sabin.x, toFloat sabin.y )
                            sprites.sabin
                       ]
                    ++ (List.indexedMap
                            (\index spriteCharacter ->
                                drawBar spriteCharacter.atbGauge index
                            )
                            model.characters
                            |> List.concatMap
                                (\bar -> bar)
                       )
                    ++ (List.indexedMap
                            (\index spriteMonster ->
                                drawEnemy sprites spriteMonster index
                            )
                            model.enemies
                            |> List.concatMap
                                (\monster -> monster)
                       )
                    ++ drawMenu model model.battleTimer sprites
                    ++ drawCursor model sprites
                    ++ drawDebug model
                )


gameWidth : Int
gameWidth =
    480


gameHeight : Int
gameHeight =
    320


gameWidthFloat : Float
gameWidthFloat =
    toFloat gameWidth


gameHeightFloat : Float
gameHeightFloat =
    toFloat gameHeight


drawBar : ATBGauge -> Int -> List Canvas.Renderable
drawBar atbGauge index =
    let
        offsetIndex =
            toFloat index + 0
    in
    case atbGauge of
        ATBGaugeCharging currentCharge ->
            let
                percentage =
                    toFloat currentCharge / 65536.0 * 100
            in
            [ shapes [ fill Color.yellow ] [ rect ( 0, offsetIndex * 20 ) percentage 20 ]
            , shapes [ stroke Color.black ] [ rect ( 0, offsetIndex * 20 ) 100 20 ]
            ]

        ATBGaugeReady ->
            [ shapes [ fill Color.yellow, stroke Color.black ] [ rect ( 0, offsetIndex * 20 ) 100 20 ] ]


drawEnemy : Sprites -> SpriteMonster -> Int -> List Canvas.Renderable
drawEnemy sprites spriteMonster index =
    let
        offsetIndex =
            toFloat index + 0
    in
    [ Canvas.texture
        []
        -- ( 20, 40 + offsetIndex * 40 )
        ( toFloat spriteMonster.x, toFloat spriteMonster.y )
        sprites.rhobite
    ]


pauseButton : Bool -> Html Msg
pauseButton paused =
    if paused == True then
        button [ onClick TogglePause ] [ Html.text "Unpause" ]

    else
        button [ onClick TogglePause ] [ Html.text "Pause" ]


drawMenu : Model -> BattleTimer -> Sprites -> List Canvas.Renderable
drawMenu model battleTimer sprites =
    case model.battleState of
        CharacterOrMonsterReady ->
            [ shapes [ fill Color.blue ] [ rect ( 20, 200 ) 300 100 ]
            , shapes [ stroke Color.white ] [ rect ( 20, 200 ) 300 100 ]
            ]
                ++ List.map
                    (\menuItem ->
                        Canvas.text
                            [ font { size = 26, family = "Final Fantasy VI SNESa" }, align Left, fill Color.white ]
                            ( menuItem.x, menuItem.y )
                            menuItem.text
                    )
                    model.menuItems

        _ ->
            [ shapes [ fill Color.blue ] [ rect ( 20, 200 ) 300 100 ]
            , shapes [ stroke Color.white ] [ rect ( 20, 200 ) 300 100 ]
            ]


drawCursor : Model -> Sprites -> List Canvas.Renderable
drawCursor model sprites =
    -- case model.selectionCursor of
    --     Hidden ->
    --         []
    --     Shown stuff ->
    case model.selectionTarget of
        Nothing ->
            []

        Just { x, y } ->
            [ Canvas.texture
                []
                ( x - 32, y - 13 )
                sprites.cursor
            ]


drawDebug : Model -> List Canvas.Renderable
drawDebug model =
    [ Canvas.text
        [ font { size = 14, family = "sans-serif" }, align Left, fill Color.black ]
        ( 200, 15 )
        ("Battle State: "
            ++ battleStateToString model.battleState
        )
    , Canvas.text
        [ font { size = 14, family = "sans-serif" }, align Left, fill Color.black ]
        ( 200, 30 )
        ("Cursor Direction: "
            ++ getCursorDirectionToString model
        )
    ]


getCursorDirectionToString : Model -> String
getCursorDirectionToString { leftPressed, rightPressed, upPressed, downPressed } =
    if leftPressed == True then
        "Left"

    else if rightPressed == True then
        "Right"

    else if upPressed == True then
        "Up"

    else if downPressed == True then
        "Down"

    else
        "None"


battleStateToString : BattleState -> String
battleStateToString battleState =
    case battleState of
        Intro ->
            "Intro"

        Charging ->
            "Charging"

        CharacterOrMonsterReady ->
            "CharacterOrMonsterReady"

        CharacterMagicOrAbilityOrItem ->
            "CharacterMagicOrAbilityOrItem"

        MonsterDying ->
            "MonsterDying"

        AllCharactersDied ->
            "AllCharactersDied"


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel (Random.initialSeed 42), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused == False then
        -- animator |> Animator.toSubscription Tick model
        Sub.batch
            [ onVisibilityChange VisibilityChange
            , onAnimationFrameDelta Frame
            , onKeyDown keyDecoderPressed
            , onKeyUp keyDecoderReleased
            ]

    else
        onVisibilityChange VisibilityChange


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
