module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode as JD

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }

type alias AppState = { stations : (List Station) , currentStation : Maybe Station }


type Msg = RequestStationsResult (Result Http.Error (List Station))

stationsHttpResult : Result Http.Error (List Station) -> List Station
stationsHttpResult res =
  case res of 
    (Err _) -> [] 
    (Ok s) -> s

init : () -> (AppState , Cmd Msg)
init _ = ( { stations = [] , currentStation = Nothing } 
         , Http.get
            { url = "https://visancosmin.github.io/RadioListener/stations.json"
            , expect = Http.expectJson RequestStationsResult (JD.list stationDecoder)
            } 
         )


update : Msg -> AppState -> (AppState,Cmd Msg)
update msg model =
  case msg of 
    (RequestStationsResult res) -> ({ model | stations = stationsHttpResult res } , Cmd.none)


view : AppState -> Html Msg
view state =
  div []
    [ div [] ([ text "View" ] ++ (List.map viewStation state.stations))
    ]









viewStation : Station -> Html Msg
viewStation station = 
  div [] 
      [ H.p [] [ text station.name ]
      , H.img [ HA.src ("https:" ++ station.thumbnail) 
              , HA.style "width" "100px"
              , HA.style "height" "100px" 
              ] []
      , H.ul [] (List.map viewTag station.categories )
      ]
viewTag : String -> Html Msg 
viewTag tag = H.li [] [ text tag ]

type alias Station = 
  { name : String 
  , stream : String 
  , thumbnail : String 
  , categories : List String
  }

stationDecoder : Decoder Station 
stationDecoder = JD.map4 Station
  (JD.field "name" JD.string)
  (JD.field "stream" JD.string)
  (JD.field "logo" JD.string)
  (JD.field "tags" (JD.list JD.string))
