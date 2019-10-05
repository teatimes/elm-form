module Form.Views exposing (..)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, on, keyCode, onSubmit)

import Form exposing (Form)
import Form.Input as Input
import Form.Error exposing (Error, ErrorValue)

import Form.Option as Option exposing (Option, GroupedRadio)
import Maybe.Extra exposing (isJust)
import Api.Error exposing (Error)

import List.Extra
import Dict exposing (Dict)
import Json.Decode as Decode
import Utils

onEnter : Attribute Form.Msg
onEnter =
  let
    isEnter code =
      if code == 13 then
        Decode.succeed Form.Submit
      else
        Decode.fail "not ENTER"
  in
      on "keydown" (Decode.andThen isEnter keyCode)

formField : String -> Maybe String -> Html Form.Msg -> String -> Html Form.Msg
formField label_ error input classes =
  div [classList [ ("error", isJust error)]
      , class (classes ++ " field")
      ]
    [ label [] [text label_]
    , input
    , errorMessage error
    ]

inputField : String ->  Form.FieldState e String -> List Error -> String -> List (Attribute Form.Msg) ->Html Form.Msg
inputField label_ state errors classes attributes =
  let
    error = getError state errors
  in
    formField label_ error (Input.textInput state (List.append [onEnter] attributes)) classes

passwordInputField : String -> Form.FieldState e String -> List Error -> Html Form.Msg
passwordInputField label_ state errors =
  let
    error = getError state errors
  in
    formField label_ error (Input.passwordInput state [onEnter]) ""

inputFieldWithOptions : String -> List (String, String) -> Form.FieldState e String -> List Error -> Html Form.Msg
inputFieldWithOptions label_ options state errors =
  let
    error = getError state errors
    optionsDict = Dict.fromList options
    placeholder_ = Dict.get "placeholder" optionsDict |> Maybe.withDefault ""
    classes = Dict.get "classes" optionsDict |> Maybe.withDefault ""
  in
    formField label_ error (Input.textInput state [placeholder placeholder_, onEnter]) classes

inputFieldNumber : String -> Form.FieldState e String -> List Error -> Html Form.Msg
inputFieldNumber label_ state errors=
  let
    error = getError state errors
    -- TODO: fork out from for and add option for NumberInput
  in
    formField label_ error (Input.textInput state [style "text-align" "right"]) ""

passwordField : String -> Form.FieldState e String -> List Error -> String -> Html Form.Msg
passwordField label_ state errors classes =
  let
    error = getError state errors
  in
    formField label_ error (Input.passwordInput state []) classes

passwordFieldWithAttributes : String -> Form.FieldState e String -> List Error -> List (Attribute Form.Msg) -> Html Form.Msg
passwordFieldWithAttributes label_ state errors attributes =
  let
    error = getError state errors
  in
    formField label_ error (Input.passwordInput state attributes) ""

selectField : String -> Form.FieldState e String -> List (String, String) -> List Error -> String -> Html Form.Msg
selectField label_ state options errors classes =
  let
    error = getError state errors
  in
    formField label_ error (Input.selectInput options state []) classes

selectFieldWithAttributes : String -> Form.FieldState e String -> List (String, String) -> List Error -> String -> List (Attribute Form.Msg) -> Html Form.Msg
selectFieldWithAttributes label_ state options errors classes attributes =
  let
    error = getError state errors
  in
    formField label_ error (Input.selectInput options state attributes) classes


groupedSelectField : String -> Form.FieldState e String -> List (Option GroupedRadio) -> List Error -> String -> Html Form.Msg
groupedSelectField label_ state options errors classes =
  let
    error = getError state errors
  in
    formField label_ error (Input.groupedSelectInput options state []) classes

groupedSelectFieldWithAttributes : String -> Form.FieldState e String -> List (Option GroupedRadio) -> List Error -> String -> List (Attribute Form.Msg) -> Html Form.Msg
groupedSelectFieldWithAttributes label_ state options errors classes attributes =
  let
    error = getError state errors
  in
    formField label_ error (Input.groupedSelectInput options state attributes) classes


toggleField : String -> Form.FieldState e Bool -> List Error -> String -> List (Attribute Form.Msg) -> Html Form.Msg
toggleField label_ state errors classes attrs =
  let
    icon = case state.value of
      Just True -> "toggle on icon"
      _ -> "toggle off icon"
  in

  div [class "toggle"]
    [ label []
      [ text label_
      , i [ class icon] []
        --[ Input.checkboxInput state [ ]
      , Input.checkboxInput state [ style "display" "none"]
      ]
    ]

checkboxField : String -> Form.FieldState e Bool -> List Error -> String ->  List (Attribute Form.Msg) ->  Html Form.Msg
checkboxField label_ state errors classes attrs =
  div [class (classes ++ " field")]
    [ div [class "checkbox" ]
      [ label []
        [ Input.checkboxInput state attrs
        , text label_
        ]
      ]
    ]

errorMessage : Maybe String -> Html Form.Msg
errorMessage maybeError =
  case maybeError of
    Just error ->
      p [ class "error-message"]
        [ text error ]

    Nothing ->
      span [class "error-message"]
        [ text "\u{2007}"]


errorMessageToString : ErrorValue e -> String
errorMessageToString error =
  case error of
    Form.Error.InvalidString ->
      "Ikke gyldig streng"
    Form.Error.InvalidEmail ->
      "Ikke gyldig email"
    Form.Error.InvalidFormat ->
      "Ikke gyldig format"
    Form.Error.LongerStringThan i ->
      "Må være lengre enn " ++ (String.fromInt i) ++ " tegn"
    Form.Error.ShorterStringThan i ->
      "Må være kortere enn " ++ (String.fromInt i) ++ " tegn"
    Form.Error.Empty ->
      "Kan ikke være tomt"
    Form.Error.InvalidInt ->
      "Ikke gyldig tall"
    -- TODO: not optimal
    Form.Error.CustomError e ->
      --customErrorMessageToString (Debug.toString error)
      "Mangler tilpasset feilmelding"
    _ ->
      "Mangler tilpasset feilmelding: " --++ Debug.toString (error)

customErrorMessageToString : String -> String
customErrorMessageToString customError =
  case customError of
    "CustomError InvalidAmount" -> "Ugyldig beløp. Bruk , for å skille mellom kroner og ører"
    "CustomError InvalidAccountNr" -> "Ugyldig kontonr. Må bestå av 11 sifre (kan bli skilt med .)"
    "CustomError NoneSelected" -> "Ingen transaksjoner er valgt"
    _ -> "Noe er ugyldig"

getError : Form.FieldState e String -> List Error -> Maybe String
getError field errors =
  case (fieldError errors field.path) of
    Just error -> Just error
    Nothing ->
      case field.liveError of
        Just e -> e |> errorMessageToString |> Just
        Nothing -> Nothing

fieldError : List Error -> String -> Maybe String
fieldError errors fieldName =
  case List.Extra.find (\error -> Tuple.first error == fieldName) errors of
    Just error -> Just (Tuple.second error)
    Nothing -> Nothing
