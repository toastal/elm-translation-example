module I18n.I18n exposing (Language, Translator, toLanguage, translate)

import Regex
import String
import I18n.Languages.EnUs as EnUs
import I18n.Phrases exposing (Phrase)


{-| Supported Languages
-}
type Language
    = EnUk
    | EnUs
    | EsMx


{-| Can take a string like "en_US" and give us back one of
our support languages … also has defaults
-}
toLanguage : String -> Language
toLanguage lang =
    let
        -- change for your use case
        codeFinder : String -> List String
        codeFinder =
            List.map String.toLower
                << List.take 2
                << Regex.split (Regex.AtMost 2) (Regex.regex "[^A-Za-z]")

        locale : ( Maybe String, Maybe String )
        locale =
            case codeFinder lang of
                a :: b :: _ ->
                    ( Just a, Just b )

                a :: _ ->
                    ( Just a, Nothing )

                _ ->
                    ( Nothing, Nothing )
    in
        case ( language, country ) of
            ( Just "en", Just "uk" ) ->
                EnUk

            ( Just "en", _ ) ->
                EnUs

            ( Just "es", _ ) ->
                EsMx

            _ ->
                EnUs


{-| Alias to pass around our application
-}
type alias Translator =
    Phrase -> String


{-| Top-level function that we will partially apply with
a langage and hand around the application for translations
-}
translate : Language -> Translator
translate lang =
    case lang of
        EnUs ->
            EnUs.translate

        _ ->
            EnUs.translate
