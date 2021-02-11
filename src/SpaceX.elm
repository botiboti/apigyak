module SpaceX exposing (..)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, field, list, map8, maybe, string)


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Loading
    | Failure Http.Error
    | Succes (List Mission)


type alias Mission =
    { mission_name : String
    , mission_id : String
    , manufacturers : List String
    , payload_ids : List String
    , wiki : String
    , website : String
    , twitter : Maybe String
    , descprition : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getMissions )


type Msg
    = GotMissions (Result Http.Error (List Mission))


getMissions : Cmd Msg
getMissions =
    Http.get
        { url = "https://api.spacexdata.com/v3/missions"
        , expect = Http.expectJson GotMissions missionListDecoder
        }


missionDecoder : Decoder Mission
missionDecoder =
    map8 Mission
        (field "mission_name" string)
        (field "mission_id" string)
        (field "manufacturers" (JD.list string))
        (field "payload_ids" (JD.list string))
        (field "wikipedia" string)
        (field "website" string)
        (maybe (field "twitter" string))
        (field "description" string)


missionListDecoder : Decoder (List Mission)
missionListDecoder =
    JD.list missionDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMissions res ->
            case res of
                Err error ->
                    ( Failure error, Cmd.none )

                Ok missions ->
                    ( Succes missions, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewMission : Mission -> Html Msg
viewMission m =
    let
        td_ elements =
            styled td
                [ padding (px 15) ]
                []
                elements
    in
    styled tr
        [ borderTop3 (px 1) solid (hex "dee2e6") ]
        []
        [ td_ [ text m.mission_name ]
        , td_ [ text m.mission_id ]
        , td_ [ text (String.join ", " m.manufacturers) ]
        , td_ [ a [ href m.wiki ] [ text m.wiki ] ]
        , td_ [ a [ href m.website ] [ text m.website ] ]
        , td_
            [ m.twitter
                |> Maybe.map (\x -> a [ href x ] [ text x ])
                |> Maybe.withDefault (text "")
            ]
        ]


viewMissions : List Mission -> Html Msg
viewMissions missions =
    let
        th_ elements =
            styled th
                [ padding (px 15)
                , textAlign left
                , backgroundColor (hex "212529")
                , color <| rgb 255 255 255
                ]
                []
                elements
    in
    Html.Styled.table
        [ css
            [ borderCollapse collapse
            , margin (px 10)
            , fontFamilies [ "Arial" ]
            , fontSize (px 16)
            ]
        ]
        [ thead []
            [ tr
                [ css
                    [ borderBottom3 (px 2) solid (hex "dee2e6")
                    , borderTop3 (px 1) solid (hex "dee2e6")
                    ]
                ]
                [ th_ [ text "Name" ]
                , th_ [ text "ID" ]
                , th_ [ text "Manufacturers" ]
                , th_ [ text "Wiki" ]
                , th_ [ text "Website" ]
                , th_ [ text "Twitter" ]
                ]
            ]
        , tbody []
            (List.map viewMission missions)
        ]


view : Model -> Html Msg
view model =
    case model of
        Succes missions ->
            div []
                [ viewMissions missions ]

        Loading ->
            div [] [ text "Loading..." ]

        Failure error ->
            div [] [ text "Error" ]
