module I18n.Languages.EsMx exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Greeting name ->
            "¡Hola, " ++ name ++ "!"

        TextColor color ->
            "El color de este texto es de color " ++ color ++ "."

        Color ->
            "Color"

        Red ->
            "rojo"

        Blue ->
            "azul"

        Green ->
            "verde"
