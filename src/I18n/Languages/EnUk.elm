module I18n.Languages.EnUk exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Greeting name ->
            "Cheerio, " ++ name ++ "!"

        Name ->
            "Name"

        TextColor color ->
            "The colour of this text is " ++ color ++ "."

        Color ->
            "Colour"

        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"
