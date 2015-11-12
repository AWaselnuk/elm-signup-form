-- declares that this is the SignupForm module, which is how other modules
-- will reference this one if they want to import it and reuse its code.
module SignupForm where

-- Elm’s "import" keyword works mostly like "require" in node.js.
-- The “exposing (..)” option says that we want to bring the Html module’s contents
-- into this file’s current namespace, so that instead of writing out
-- Html.form and Html.label we can use "form" and "label" without the "Html."
import Html exposing (..)
import Html.Events exposing (..)
import StartApp
import Effects
import Http
import Task exposing (Task)
import Json.Decode exposing (succeed)

-- With this import we are only bringing a few specific functions into our
-- namespace, specifically "id", "type'", "for", "value", and "class".
import Html.Attributes exposing (id, type', for, value, class)

--------
-- MODEL
--------

type alias Model =
  { email: String
  , password: String
  , storename: String
  , errors: Errors
  }

type alias Errors =
  { email: String
  , password: String
  , storename: String
  , storenameTaken: Bool
  }

type alias Email = String
type alias Password = String
type alias Storename = String

type Action
  = Validate
  | SetEmail Email
  | SetPassword Password
  | SetStorename Storename
  | StorenameTaken
  | StorenameAvailable

---------
-- VIEW
---------

-- The view function which takes a model as its only argument
view : Signal.Address Action -> Model -> Html
view actionDispatcher model =
  form
    [ id "form-signup" ]
    [ h1 [ class "text-center" ] [ text "Start your free 999-day trial of Shopify" ]
    , emailInputView actionDispatcher model
    , passwordInputView actionDispatcher model
    , storenameInputView actionDispatcher model
    , div
      [
        class "btn btn-submit"
      , onClick actionDispatcher Validate
      ]
      [ text "Create your store" ]
    ]

emailInputView : Signal.Address Action -> Model -> Html
emailInputView actionDispatcher model =
  div
    [ class "form-control" ]
    [
      label [ for "email" ] [ text "Email address " ]
    , input
      [ id "email"
      , type' "text"
      , value model.email
      , on "input" targetValue (\email -> Signal.message actionDispatcher (SetEmail email))
      ] []
    , div [ class "error"] [ text model.errors.email ]
    ]

passwordInputView : Signal.Address Action -> Model -> Html
passwordInputView actionDispatcher model =
  div
    [ class "form-control" ]
    [
      label [ for "password"] [text "Password " ]
    , input
      [ id "password"
      , type' "Password"
      , value model.password
      , on "input" targetValue (\password -> Signal.message actionDispatcher (SetPassword password))
      ] []
    , div [ class "error"] [ text model.errors.password ]
    ]

storenameInputView : Signal.Address Action -> Model -> Html
storenameInputView actionDispatcher model =
  div
    [ class "form-control" ]
    [
      label [ for "storename" ] [ text "Your store name" ]
    , input
      [ id "storename"
      , type' "text"
      , value model.storename
      , on "input" targetValue (\storename -> Signal.message actionDispatcher (SetStorename storename))
      ] []
    , div [ class "error" ] [ text (viewStorenameErrors model) ]
    ]

viewStorenameErrors : Model -> String
viewStorenameErrors model =
  if model.errors.storenameTaken then
    "That storename is taken!"
  else
    model.errors.storename

--------
-- UPDATE
--------

-- The update function takes an action and a model and returns
-- a new, updated model
-- and a description of any effects we want done (i.e. fire AJAX, start animation)
update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Validate ->
      let
        url =
          "https://api.github.com/users/" ++ model.storename
        request =
          Http.get (succeed StorenameTaken) url
        neverFailingRequest =
          Task.onError request (\err -> Task.succeed StorenameAvailable)
      in
        ({ model | errors <- getErrors model }, Effects.task neverFailingRequest)
    SetEmail email ->
      ( { model | email <- email }, Effects.none )
    SetPassword password ->
      ( { model | password <- password }, Effects.none)
    SetStorename storename ->
      ( { model | storename <- storename }, Effects.none)
    StorenameTaken ->
      ( withStorenameTaken True model, Effects.none )
    StorenameAvailable ->
      ( withStorenameTaken False model, Effects.none )


-- TODO: Use a proper submit input and intercept the form submit evet
-- https://groups.google.com/forum/#!msg/elm-discuss/W3X_m1mE70w/02J3Jf4dCQAJ
getErrors : Model -> Errors
getErrors model =
  { email =
      if model.email == "" then
        "Please enter a email!"
      else
        ""
  , password =
      if model.password == "" then
        "Please enter a password!"
      else
        ""
  , storename =
      if model.storename == "" then
        "Please enter a storename"
      else
        ""
  , storenameTaken =
      model.errors.storenameTaken
  }

withStorenameTaken : Bool -> Model -> Model
withStorenameTaken isTaken model =
  let
    currentErrors =
      model.errors
    newErrors =
      { currentErrors | storenameTaken <- isTaken }
  in
   { model | errors <- newErrors }

--------
-- RUN APP
--------

initialErrors : Errors
initialErrors =
  { email = "", password = "", storename = "", storenameTaken = False }

initialModel : Model
initialModel =
  { email = "", password = "", storename = "", errors = initialErrors }

-- This sets up the elm architecture using StartApp which wraps some boilerplate wiring
app : StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html

-- Some sort of wiring up for the Effects signals?
port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks
