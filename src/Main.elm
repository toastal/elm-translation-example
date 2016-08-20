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


--main : Program Flags


main : Program Never
main =
    --Html.programWithFlags
    Html.program
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


changeColorDecoder : Decoder Msg
changeColorDecoder =
    Decode.succeed ChangeColor
        |: Decode.map (colorFromString >> Maybe.withDefault Blue) targetValue


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


viewLanguageChanger : Language -> Html Msg
viewLanguageChanger language =
    select [ on "change" changeLanguageDecoder ] <|
        List.map (Fn.map2 (,) identity ((==) language) >> langOption)
            [ EnUs, EnUk, EsMx ]


viewName : Translator -> String -> Html Msg
viewName translate name' =
    div []
        [ label [ for "name" ] [ text "Name" ]
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
        , on "change" changeColorDecoder
        ]
        [ text << translate <| colorToPhrase col ]


viewColorChanger : Translator -> Color -> Html Msg
viewColorChanger translate color =
    div []
        [ select [] <|
            List.map
                (Fn.map2 (,) identity ((==) color) >> colorOption translate)
                [ Red, Green, Blue ]
        , h1 []
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
            , pre [] [ text << toString <| model ]
            ]
