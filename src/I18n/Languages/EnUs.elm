module I18n.Languages.EnUs exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Greeting name ->
            "Yo, " ++ name ++ "!"

        TextColor color ->
            "The color of this text is " ++ color ++ "."

        Color ->
            "Color"

        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"
