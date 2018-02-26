port module Main exposing (..)

import Html exposing (a, Html, div, h1, text)
import Html.Attributes exposing (class, type_, href)
import Html.Events exposing (onClick)
import Navigation
import UrlParser exposing ((</>))


main : Program (Maybe String) Model Msg
main =
    Navigation.programWithFlags urlParser
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


-- Ports


port setStorage : String -> Cmd msg


port removeStorage : String -> Cmd msg

-- MODEL


type alias Model =
    { route : Route
    , code : String
    , token : String
    , jwt : String
    }


initialModel : Route -> Model
initialModel route =
    { route = route
    , code = ""
    , token = ""
    , jwt = ""
    }



-- Start from a location, not necessarily "/".


init : Maybe String -> Navigation.Location -> ( Model, Cmd Msg )
init maybeString location =
    ( findRouteOrGoHome location, Cmd.none )


findRouteOrGoHome : Navigation.Location -> Model
findRouteOrGoHome location =
    case Debug.log "Landing on: " (UrlParser.parsePath routeParser location) of
        Nothing ->
            (initialModel HomeRoute)

        Just route ->
            updateModel (initialModel route)


updateModel model =
    case model.route of
        CodeRoute str ->
            { model | code = str }

        TokenRoute str ->
            { model | token = str }

        JwtRoute str ->
            { model | jwt = str }

        HomeRoute ->
            model

        NotFound ->
            model



-- ROUTES


type Route
    = HomeRoute
    | CodeRoute String
    | TokenRoute String
    | JwtRoute String
    | NotFound



-- UPDATE


type Msg
    = FollowRoute Route

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, commands ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ commands, setStorage (parametersToString newModel) ]
    )

parametersFromString : String -> Model
parametersFromString string =
    let
        list =
            Debug.log "list" (String.split ":" string)
    in
    {
        route = HomeRoute
        , code = (pick 1 list)
        , token = (pick 2 list)
        , jwt = (pick 3 list)
    }


pick : Int -> List String -> String
pick n list =
    if n == 1 then
        case List.head list of
            Just head ->
                head

            Nothing ->
                ""
    else
        case List.tail list of
            Just tail ->
                pick (n - 1) tail

            Nothing ->
                ""

parametersToString : Model -> String
parametersToString model =
    let
        p =
            Debug.log "Parameters to String" model
    in
    String.join ":"
        [ model.code
        ,model.token
        ,model.jwt
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Update message: " msg of
        FollowRoute route ->
            case route of
                CodeRoute str ->
                    ( { model | code = str }, Cmd.none )

                TokenRoute str ->
                    ( { model | token = str }, Cmd.none )

                JwtRoute str ->
                    ( { model | jwt = str }, Cmd.none )

                HomeRoute ->
                    ( model, Cmd.none )

                NotFound ->
                    ( model, Cmd.none )



-- PARSING
-- The URL parser mentioned in the program entry point. Takes a Location and
-- parse it to see where to go next.


urlParser : Navigation.Location -> Msg
urlParser location =
    let
        l =
            Debug.log "location" location

        parsed =
            UrlParser.parsePath routeParser location
    in
    case Debug.log "parsed" parsed of
        Nothing ->
            FollowRoute NotFound

        Just route ->
            FollowRoute route



-- Try all parsers we have. The first parser to say yes will determine the route value.


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute homeParser
        , UrlParser.map CodeRoute codeParser
        , UrlParser.map TokenRoute tokenParser
        , UrlParser.map JwtRoute jwtParser
        ]


homeParser : UrlParser.Parser a a
homeParser =
    UrlParser.oneOf
        [ UrlParser.s "index.html"
        , UrlParser.s ""
        ]


codeParser : UrlParser.Parser (String -> a) a
codeParser =
    UrlParser.s "code" </> UrlParser.string


tokenParser : UrlParser.Parser (String -> a) a
tokenParser =
    UrlParser.s "token" </> UrlParser.string


jwtParser : UrlParser.Parser (String -> a) a
jwtParser =
    UrlParser.s "jwt" </> UrlParser.string



-- VIEW
-- In this example, we have a header at the top, a menu bar below and then the page specific content.


view : Model -> Html Msg
view model =
    div []
        [ header
        , viewData model
        ]


header : Html Msg
header =
    div [] [ h1 [] [ text "Demo client" ]
     , a[  href "https://curity-slave-oauth-test1.ocp.hh.atg.se/oauth/authorize?client_id=atg&response_type=code&scope=read&redirect_uri=http://localhost:8080/code"]
     [
        text "Login"
        ]
     ]


viewData model =
    div []
        [ div []
            [ text ("Code: " ++ model.code)
            ]
        , div []
            [ text ("Token: " ++ model.token)
            ]
        , div []
            [ text ("JWT: " ++ model.jwt)
            ]
        ]



-- SUBSCRIPTIONS
-- No subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
