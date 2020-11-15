port module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode as JD
import String as S


port playerPlay : () -> Cmd msg
port playerPause : () -> Cmd msg

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type PlayerState = Playing | Paused
type alias AppState = 
  { allStations : (List Station) 
  , stations : List Station
  , currentStation : Station  
  , playerState : PlayerState
  }


type Msg = RequestStationsResult (Result Http.Error (List Station))
         | PlayerPlay
         | PlayerPause
         | SearchChanged String
         | ChangeStation Station

stationsHttpResult : Result Http.Error (List Station) -> List Station
stationsHttpResult res =
  case res of 
    (Err _) -> [] 
    (Ok s) -> s

init : () -> (AppState , Cmd Msg)
init _ = ( { allStations = [] 
           , stations = [] 
           , currentStation = { name = "Select a station" , stream = "" , categories = [] , thumbnail = ""} 
           , playerState = Paused 
           } 
         , Http.get
            { url = "https://visancosmin.github.io/RadioListener/stations.json"
            , expect = Http.expectJson RequestStationsResult (JD.list stationDecoder)
            } 
         )


update : Msg -> AppState -> (AppState,Cmd Msg)
update msg model =
  case msg of 
    (RequestStationsResult res) -> ( { model | allStations = stationsHttpResult res 
                                             , stations = stationsHttpResult res } , Cmd.none)
    PlayerPlay -> ({model | playerState = Playing } , playerPlay () )
    PlayerPause -> ({model | playerState = Paused } , playerPause () )
    (SearchChanged s) -> ({model | stations = List.filter (filterStation s) model.allStations}
                         , Cmd.none)
    (ChangeStation station) -> ({model | currentStation = station , playerState = Playing} , Cmd.none)


view : AppState -> Html Msg
view state =
    div [ HA.class "body"]
        [ div [ HA.class "navbar"]
              [ H.img [ HA.src "https://www.flaticon.com/svg/static/icons/svg/3111/3111899.svg" ] []
              , H.input [ HA.type_ "text" , HE.onInput SearchChanged ] []
              ] 
        , div [ HA.class "content"]
              (List.map (viewStation state.currentStation) state.stations)
        , div [ HA.class "sidebar"]
              [ div [ HA.class "panel"]
                    [ div [HA.class "favorite-stations"]
                          [ H.h2 [] [text "Favorite Stations"]
                          , div [ HA.class "stations-list" ] []
                          ]
                    , div [HA.class "player"]
                          [ case state.playerState of 
                              Playing -> div [HA.class "player-play" , onClick PlayerPause] [ H.span [] [text "❚❚"] ]
                              Paused -> div [HA.class "player-play" , onClick PlayerPlay] [ H.span [] [text "►"] ] 
                          , H.audio [ HA.id "audio-player" , HA.src state.currentStation.stream , HA.autoplay True ] []
                          , div [HA.class "player-info"] 
                                [ H.h2 [] [ text state.currentStation.name ]
                                , H.input [HA.type_ "range" , HA.min "0" , HA.max "1" , HA.value "1" , HA.step "0.05" ] []
                                ]
                          , div [HA.class "player-favorite"] [text "❤"]
                          ]
                    ]
              ]
        ]

{-
  div [ HA.class "body"]
    [ H.audio [ HA.id "audio-player" , HA.src "https://live.kissfm.ro:8443/kissfm.aacp" ] []
    , case state.playerState of
        Playing -> H.button [ onClick PlayerPause ] [ text "Play/Pause music" ]
        Paused -> H.button [ onClick PlayerPlay ] [ text "Play/Pause music" ]
    , H.input [ HE.onInput SearchChanged ] []
    , div [] ([ text "View" ] ++ (List.map viewStation state.stations))
    ]
}

-}







viewStation : Station -> Station -> Html Msg
viewStation currentStation station = 
  div [ HA.class "card" ]
      [ H.img [ HA.src ("https:" ++ station.thumbnail) ] []
      , div [ HA.class "card-content" ]
            [ H.h2 [HA.title station.name] [text station.name]
            , div [ HA.class "tags"]
                  (List.map viewTag (List.intersperse " ● " station.categories))
            , div [ HA.class "actions" ]
                  [ let playingClass = if (currentStation.name == station.name) then "play-selected" else "" 
                    in div [ HA.class "play" , HA.class playingClass , onClick (ChangeStation station) ] [text "► PLAY"]
                  , div [ HA.class "love" ] [text "❤ LOVE"]
                  ]
            ]
      ]

{-
  div [] 
      [ H.p [] [ text station.name ]
      , H.img [ HA.src ("https:" ++ station.thumbnail) 
              , HA.style "width" "100px"
              , HA.style "height" "100px" 
              ] []
      , H.ul [] (List.map viewTag station.categories )
      ]
}
-}
viewTag : String -> Html Msg 
viewTag tag = H.span [] [ text tag ]

type alias Station = 
  { name : String 
  , stream : String 
  , thumbnail : String 
  , categories : List String
  }

filterStation : String -> Station -> Bool 
filterStation search station = (S.contains search (S.toLower station.name)) || (List.member search station.categories)

stationDecoder : Decoder Station 
stationDecoder = JD.map4 Station
  (JD.field "name" JD.string)
  (JD.field "stream" JD.string)
  (JD.field "logo" JD.string)
  (JD.field "tags" (JD.list JD.string))
