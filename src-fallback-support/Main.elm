module Main exposing (main)

import Function.Extra as Fn
import Html exposing (..)
import Html.App as Html
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode exposing ((|:))
import I18n.I18n as I18n exposing (Language(..), Translator)
import I18n.Phrases as Phrases exposing (Phrase)


{-| Swap these comments at build… `elm-reactor` can’t pass
in flags :(
-}



--main : Program Flags


main : Program Never
main =
    --Html.programWithFlags
    Html.program
        -- { init = init
        { init = initWoFlags
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- TYPES


type alias Flags =
    { language : String
    }


type Color
    = Red
    | Green
    | Blue


{-| Derivation and Lens would clean this up
-}
colorToPhrase : Color -> Phrase
colorToPhrase color =
    case color of
        Red ->
            Phrases.Red

        Green ->
            Phrases.Green

        Blue ->
            Phrases.Blue


colorFromString : String -> Maybe Color
colorFromString cstr =
    case cstr of
        "Red" ->
            Just Red

        "Green" ->
            Just Green

        "Blue" ->
            Just Blue

        _ ->
            Nothing


languageFromString : String -> Maybe Language
languageFromString lang =
    case lang of
        "EnUk" ->
            Just EnUk

        "EnUs" ->
            Just EnUs

        "EsMx" ->
            Just EsMx

        "Klingon" ->
            Just Klingon

        _ ->
            Nothing



-- MODEL


type alias Model =
    { name' : String
    , color : Color
    , language : Language
    }


{-| singleton, or pure, for the model cmd tuple
-}
singleton : Model -> ( Model, Cmd Msg )
singleton =
    flip (,) Cmd.none


{-| For testing with Elm Reactor (which doesn't handle flags)
-}
initWoFlags : ( Model, Cmd Msg )
initWoFlags =
    singleton <| Model "Gwendela" Red EnUs


{-| Turn our langauge string flag into 'real' Language
and create the model cmd tuple
-}
init : Flags -> ( Model, Cmd Msg )
init { language } =
    I18n.toLanguage language
        |> Model "Gwendela" Red
        |> singleton



-- MSG


type Msg
    = ChangeLanguage Language
    | ChangeColor Color
    | InputName String


{-| Used in a <select>'s onchange
-}
changeColorDecoder : Decoder Msg
changeColorDecoder =
    Decode.succeed ChangeColor
        |: Decode.map (colorFromString >> Maybe.withDefault Blue) targetValue


{-| Used in a <select>'s onchange
-}
changeLanguageDecoder : Decoder Msg
changeLanguageDecoder =
    Decode.succeed ChangeLanguage
        |: Decode.map (languageFromString >> Maybe.withDefault EnUs) targetValue



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    singleton <|
        case msg of
            ChangeLanguage lang ->
                { model | language = lang }

            ChangeColor color ->
                { model | color = color }

            InputName name' ->
                { model | name' = name' }



-- VIEW


langOption : ( Language, Bool ) -> Html Msg
langOption ( lang, sel ) =
    option
        [ selected sel
        , value <| toString lang
        ]
    <|
        flip (::) [] <|
            case lang of
                EnUk ->
                    text "en_UK 🇬🇧"

                EnUs ->
                    text "en_US 🇺🇸"

                EsMx ->
                    text "es_MX 🇲🇽"

                Klingon ->
                    text "tlh 👽"


viewLanguageChanger : Language -> Html Msg
viewLanguageChanger language =
    select [ on "change" changeLanguageDecoder ] <|
        List.map (Fn.map2 (,) identity ((==) language) >> langOption)
            [ EnUs
            , EnUk
            , EsMx
            , Klingon
            ]


viewName : Translator -> String -> Html Msg
viewName translate name' =
    div []
        [ label [ for "name" ] [ text <| translate Phrases.Name ]
        , input
            [ type' "text"
            , name "name"
            , value name'
            , onInput InputName
            ]
            []
        , h1 []
            [ text << translate <| Phrases.Greeting name' ]
        ]


colorOption : Translator -> ( Color, Bool ) -> Html Msg
colorOption translate ( col, sel ) =
    option
        [ selected sel
        , value <| toString col
        ]
        [ text << translate <| colorToPhrase col ]


colorToStyle : Color -> ( String, String )
colorToStyle color =
    (\hue -> ( "color", "hsl(" ++ toString hue ++ ", 55%, 45%)" )) <|
        case color of
            Red ->
                0

            Green ->
                120

            Blue ->
                195


viewColorChanger : Translator -> Color -> Html Msg
viewColorChanger translate color =
    div []
        [ label [ for "color" ] [ text <| translate Phrases.Color ]
        , select
            [ name "color"
            , on "change" changeColorDecoder
            ]
          <|
            List.map
                (Fn.map2 (,) identity ((==) color) >> colorOption translate)
                [ Red, Green, Blue ]
        , h1 [ style [ colorToStyle color ] ]
            [ text
                << translate
                << Phrases.TextColor
                << translate
              <|
                colorToPhrase color
            ]
        ]


view : Model -> Html Msg
view ({ name', color, language } as model) =
    let
        -- This is our translation function that we will pass around
        translate : Translator
        translate =
            I18n.translate language
    in
        div []
            [ viewLanguageChanger language
            , viewName translate name'
            , viewColorChanger translate color
              -- purposefully not a <pre>
            , div
                [ style
                    [ ( "white-space", "pre" )
                    , ( "font-family", "Fantasque Sans Mono, monospace" )
                    ]
                ]
                [ text <| "App State: " ++ toString model ]
            ]
