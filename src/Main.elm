port module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Json.Encode as JE
import Json.Decode as JD
import String as S


port playerPlay : () -> Cmd msg
port playerPause : () -> Cmd msg
port setStorage : Value -> Cmd msg
port playerVolume : String -> Cmd msg

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
  , favoriteStations : List Station
  , playerVolume : String
  }


type Msg = RequestStationsResult (Result Http.Error (List Station))
         | PlayerPlay
         | PlayerPause
         | SearchChanged String
         | ChangeStation Station
         | AddRemoveFavoriteStation Station
         | ChangeVolume String

stationsHttpResult : Result Http.Error (List Station) -> List Station
stationsHttpResult res =
  case res of 
    (Err _) -> [] 
    (Ok s) -> s

init : Value -> (AppState , Cmd Msg)
init jsonFavorite = 
           ( { allStations = [] 
           , stations = [] 
           , currentStation = { name = "Select a station" , stream = "" , categories = [] , thumbnail = ""} 
           , playerState = Paused 
           , favoriteStations = case (JD.decodeValue (JD.list stationDecoder) jsonFavorite) of
                                          (Err _) -> []
                                          (Ok s) -> s  
           , playerVolume = "0.5"
           } 
         , Http.get
            { url = "http://www.radiolistener.net/stations.json"
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
    (AddRemoveFavoriteStation station) -> 
      let newFavoriteStations = appendIfNotIn station model.favoriteStations
      in ({model | favoriteStations = newFavoriteStations } , setStorage ((JE.list stationEncoder) newFavoriteStations))
    (ChangeVolume volume) -> ({model | playerVolume = volume} , playerVolume volume)

view : AppState -> Html Msg
view state =
    div [ HA.class "body"]
        [ div [ HA.class "navbar"]
              [ H.img [ HA.src "https://www.flaticon.com/svg/static/icons/svg/3111/3111899.svg" ] []
              , H.input [ HA.type_ "text" , HE.onInput SearchChanged ] []
              ] 
        , div [ HA.class "content"]
              (List.map (viewStation state.favoriteStations state.currentStation) state.stations)
        , div [ HA.class "sidebar"]
              [ div [ HA.class "panel"]
                    [ div [HA.class "favorite-stations"]
                          [ H.h2 [] [text "Favorite Stations"]
                          , div [ HA.class "stations-list" ] 
                                (List.map (viewFavoriteStation state.currentStation) state.favoriteStations)
                          ]
                    , div [HA.class "player"]
                          [ case state.playerState of 
                              Playing -> div [HA.class "player-play" , onClick PlayerPause] [ H.span [] [text "❚❚"] ]
                              Paused -> div [HA.class "player-play" , onClick PlayerPlay] [ H.span [] [text "►"] ] 
                          , H.audio [ HA.id "audio-player" , HA.src state.currentStation.stream , HA.autoplay True ] []
                          , div [HA.class "player-info"] 
                                [ H.h2 [] [ text state.currentStation.name ]
                                , H.input [HA.type_ "range" , HA.min "0" , HA.max "1" , HA.value state.playerVolume , HA.step "0.05" , HE.onInput ChangeVolume ] []
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







viewStation : List Station -> Station -> Station -> Html Msg
viewStation favoriteStations currentStation station = 
  div [ HA.class "card" ]
      [ H.img [ HA.src ("https:" ++ station.thumbnail) ] []
      , div [ HA.class "card-content" ]
            [ H.h2 [HA.title station.name] [text station.name]
            , div [ HA.class "tags"]
                  (List.map viewTag (List.intersperse " ● " station.categories))
            , div [ HA.class "actions" ]
                  [ let playingClass = if (currentStation.name == station.name) then "play-selected" else "" 
                    in div [ HA.class "play" , HA.class playingClass , onClick (ChangeStation station) ] [text "► PLAY"]
                  , let favoriteClass = if (List.member station favoriteStations) then "love-selected" else ""
                    in div [ HA.class "love" , HA.class favoriteClass , onClick (AddRemoveFavoriteStation station) ] [text "❤ LOVE"]
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

viewFavoriteStation : Station -> Station -> Html Msg 
viewFavoriteStation currentStation station = 
  div [HA.class "favorite-item"]
      [ H.img [ HA.src ("https:" ++ station.thumbnail) ] []
      , H.h2 [] [text station.name]
      , let playingClass = if (currentStation.name == station.name) then "play-selected" else ""
        in div [ HA.class "play" , HA.class playingClass , onClick (ChangeStation station) ] [text "► PLAY"]
      ]

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

stationEncoder : Station -> Value
stationEncoder station = 
  JE.object 
    [ ("name" , JE.string station.name)
    , ("stream" , JE.string station.stream)
    , ("logo", JE.string station.thumbnail)
    , ("tags" , JE.list JE.string station.categories)
    ]


appendIfNotIn : Station -> List Station -> List Station
appendIfNotIn station list = 
  if (List.member station list) == True 
    then List.filter ((/=) station) list 
    else List.append list [station]