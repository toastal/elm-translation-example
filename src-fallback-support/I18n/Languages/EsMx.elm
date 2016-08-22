module I18n.Languages.EsMx exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> Maybe String
translate phrase =
    case phrase of
        Greeting name ->
            Just <| "¡Hola, " ++ name ++ "!"

        Name ->
            Just "Nombre"

        TextColor color ->
            Just <| "El color de este texto es de color " ++ color ++ "."

        Color ->
            Just "Color"

        Red ->
            Just "rojo"

        Blue ->
            Just "azul"

        Green ->
            Just "verde"
