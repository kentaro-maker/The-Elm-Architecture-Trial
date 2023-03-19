-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  div[] [
      checkPasswordPattern model
    , checkPasswordLength model
    , checkPasswordMatch model
  ]
    
checkPasswordMatch : Model -> Html msg
checkPasswordMatch model = 

  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK: Passwords Match" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]

checkPasswordLength : Model -> Html msg
checkPasswordLength model = 
  
  if String.length model.password > 8 then
    div [ style "color" "green" ] [ text "OK: Password is long enough." ]
  else
    div [ style "color" "red" ] [ text "Password is too short!" ]
    
checkPasswordPattern : Model -> Html msg
checkPasswordPattern model =
  div[][ 
    getIsDigitHtml model.password
    , getIsUpperHtml model.password
    , getIsLowerHtml model.password
  ]
  
getIsDigitHtml : String -> Html msg
getIsDigitHtml string = 
  if String.any Char.isDigit string == True then
    div [ style "color" "green" ] [ text "OK: Number is included." ]
  else
    div [ style "color" "red" ] [ text "Need at least one number!" ]
     
getIsUpperHtml : String -> Html msg
getIsUpperHtml string = 
  if String.any Char.isUpper string == True then
    div [ style "color" "green" ] [ text "OK: Upper alphabet is included." ]
  else
    div [ style "color" "red" ] [ text "Need at least one upper alphabet!" ]
  
getIsLowerHtml : String -> Html msg
getIsLowerHtml string = 
  if String.any Char.isLower string == True then
    div [ style "color" "green" ] [ text "OK: Lower alphabet is included." ]
  else
    div [ style "color" "red" ] [ text "Need at least one lower alphabet!" ]
    
