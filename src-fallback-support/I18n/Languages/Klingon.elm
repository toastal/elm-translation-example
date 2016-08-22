module I18n.Languages.Klingon exposing (translate)

import I18n.Phrases exposing (Phrase(..))


translate : Phrase -> Maybe String
translate phrase =
    case phrase of
        Greeting name ->
            Just <| name ++ " nuqneH."

        _ ->
            Nothing
