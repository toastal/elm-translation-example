module I18n.Languages.EnUk exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> Maybe String
translate phrase =
    case phrase of
        Greeting name ->
            Just <| "Cheerio, " ++ name ++ "!"

        Name ->
            Just "Name"

        TextColor color ->
            Just <| "The colour of this text is " ++ color ++ "."

        Color ->
            Just "Colour"

        Red ->
            Just "red"

        Blue ->
            Just "blue"

        Green ->
            Just "green"
