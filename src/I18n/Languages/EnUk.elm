module I18n.Languages.EnUk exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Greeting name ->
            "Cheerio, " ++ name ++ "!"

        TextColor color ->
            "The colour of this text is " ++ color "."

        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"
