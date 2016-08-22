module I18n.Languages.EnUs exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> Maybe String
translate phrase =
    case phrase of
        Greeting name ->
            Just <| "Yo, " ++ name ++ "!"

        Name ->
            Just "Name"

        TextColor color ->
            Just <| "The color of this text is " ++ color ++ "."

        Color ->
            Just "Color"

        Red ->
            Just "red"

        Blue ->
            Just "blue"

        Green ->
            Just "green"
