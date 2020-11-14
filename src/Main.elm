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
  , currentStation : Maybe Station  
  , playerState : PlayerState
  }


type Msg = RequestStationsResult (Result Http.Error (List Station))
         | PlayerPlay
         | PlayerPause
         | SearchChanged String

stationsHttpResult : Result Http.Error (List Station) -> List Station
stationsHttpResult res =
  case res of 
    (Err _) -> [] 
    (Ok s) -> s

init : () -> (AppState , Cmd Msg)
init _ = ( { allStations = [] 
           , stations = [] 
           , currentStation = Nothing 
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


view : AppState -> Html Msg
view state =
    div [ HA.class "body"]
        [ div [ HA.class "navbar"]
              [ H.img [ HA.src "https://www.flaticon.com/svg/static/icons/svg/3111/3111899.svg" ] []
              , H.input [ HA.type_ "text" , HE.onInput SearchChanged ] []
              ] 
        , div [ HA.class "content"]
              (List.map viewStation state.stations)
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







viewStation : Station -> Html Msg
viewStation station = 
  div [ HA.class "card" ]
      [ H.img [ HA.src ("https:" ++ station.thumbnail) ] []
      , div [ HA.class "card-content" ]
            [ H.h2 [] [text station.name]
            , div [ HA.class "tags"]
                  (List.map viewTag (List.intersperse " ● " station.categories))
            , div [ HA.class "actions" ]
                  [ div [ HA.class "play" ] [text "► PLAY"]
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
