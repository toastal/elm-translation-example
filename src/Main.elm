module Main exposing (main)

import Html exposing (..)
import Html.App as Html
import Html.Attributes as Attr exposing (..)
import I18n.I18n as I18n exposing (Language(EnUs))


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
    { language : Maybe String
    }


type Color
    = Red
    | Blue
    | Green



-- MODEL


type alias Model =
    { name : String
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


init : Flags -> ( Model, Cmd Msg )
init { language } =
    language
        |> Maybe.map I18n.toLanguage
        |> Maybe.withDefault EnUs
        |> Model "Gwendela" Red
        |> singleton



-- MSG


type Msg
    = ChangeLanguage Language
    | ChangeColor Color
    | UpdateName String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    singleton <|
        case msg of
            ChangeLanguage lang ->
                { model | language = lang }

            ChangeColor color ->
                { model | color = color }

            UpdateName name ->
                { model | name = name }



-- VIEW


view : Model -> Html Msg
view { name, color, language } =
    div [] [ text "Hello World" ]
