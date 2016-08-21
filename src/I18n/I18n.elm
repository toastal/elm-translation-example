module I18n.I18n exposing (Language(..), Translator, toLanguage, translate)

import Regex
import String
import I18n.Languages.EnUk as EnUk
import I18n.Languages.EnUs as EnUs
import I18n.Languages.EsMx as EsMx
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
        -- will split our string on non-chars, take the first
        -- 2 matches, and lowercase them
        codeFinder : String -> List String
        codeFinder =
            List.map String.toLower
                << List.take 2
                << Regex.split (Regex.AtMost 2) (Regex.regex "[^A-Za-z]")

        -- pattern that regex into Tuple2 of Maybes
        -- containing the ( language code, country code )
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
        -- Using pattern matching and wildcards, we can
        -- choose the appropriate language and fallbacks
        case locale of
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
        EnUk ->
            EnUk.translate

        EnUs ->
            EnUs.translate

        EsMx ->
            EsMx.translate
