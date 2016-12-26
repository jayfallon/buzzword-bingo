module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

-- model

type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    }


type alias Entry =
        { id : Int
        , phrase : String
        , points : Int
        , marked : Bool
        }

type alias Score =
    { id : Int
    , name : String
    , score: Int
    }

initialModel : Model
initialModel =
    { name = " Jay"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing 
    }

type Msg 
    = NewGame 
    | Mark Int 
    | NewRandom Int 
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message = 
                    "Your score of "
                    ++ (toString score.score)
                    ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message}, Cmd.none)   

        NewScore (Err error) ->
            let
                message = 
                    "Error posting your score: "
                    ++ (toString error)
            in
                ( { model | alertMessage = Just message}, Cmd.none)         
            
        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none)

        NewEntries (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.NetworkError ->
                            "Is the server running?"
                        
                        Http.BadStatus response ->
                            (toString response.status)

                        Http.BadPayload message _ ->
                            "Decoding Failed: " ++ message

                        _ ->
                            (toString error)      

            in
                ( { model | alertMessage = Just errorMessage }, Cmd.none )
        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )        
                        
        Mark id ->
            let 
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else 
                        e
            in
                ( { model | entries = List.map markEntry model.entries }, Cmd.none )

--decoders

entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)

scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)            

encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ("name", Encode.string model.name)
        , ("score", Encode.int (sumMarkedPoints model.entries))
        ]        

-- commands

apiUrlPrefix : String
apiUrlPrefix =
    --"http://localhost:3000/"
    "https://bingo-api-server-zzmgqogyek.now.sh/"


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom ( Random.int 1 100 )            

entriesUrl : String
entriesUrl =
    apiUrlPrefix ++ "random-entries"

postScore : Model -> Cmd Msg
postScore model =
    let
        url = 
            apiUrlPrefix ++ "scores"

        body =
            encodeScore model
                |> Http.jsonBody


        request =
            Http.post url body scoreDecoder    

    in    
        Http.send NewScore request

getEntries : Cmd Msg
getEntries =
    --Http.send NewEntries (Http.getString entriesUrl)
    (Decode.list entryDecoder)
        |> Http.get entriesUrl
        |> Http.send NewEntries    

    --send : (Result Http.Error String -> Msg) -> Request String -> Cmd Msg

-- view    

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game # " ++ (toString gameNumber)


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text

    in    
        h2 [id "info", class "classy"]
            [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]      


viewFooter : Html Msg
viewFooter =
    footer []              
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered by Elm"]
        ]

viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ("marked", entry.marked)], onClick (Mark entry.id) ]
        [ span [ class "phrase"] [ text entry.phrase ]
        , span [ class "points"] [ text (toString entry.points) ]
        ]      

viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
        ul [] listOfEntries

sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum

viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]

view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ viewHeader "BUZZWORD BINGO"
    , viewPlayer model.name model.gameNumber
    , viewAlertMessage model.alertMessage
    , viewEntryList model.entries
    , viewScore ( sumMarkedPoints model.entries )
    , div [ class "button-group" ]
        [ button [ onClick NewGame ] [ text "New Game" ]
        , if sumMarkedPoints model.entries > 0 
            then button [ onClick ShareScore ] [ text "Share Score" ] 
            else button [disabled True][text "Share Score"]
        ]
    , div [ class "debug" ] [ text (toString model) ]
    , viewFooter
    ]

viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseAlert ] [ text "X"]
                , text message
                ] 
        Nothing ->
            text ""

main : Program Never Model Msg
main = 
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none )
        }
      
