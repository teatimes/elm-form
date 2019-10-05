module Form.Option exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)

import Json.Encode as Encode
import Form exposing (Form)
import Form.Validate as Form
import Form.Field exposing (Field)

-- MODEL

type Option o
  = Option o

type alias Radio =
  { id : Int
  , value : String
  }

type alias Checkbox =
  { id : Int
  , value : String
  , checked : Bool
  }

type alias GroupedRadio =
  { name : String
  , subs : List (Option Radio)
  }

-- INFO

id : Option { o | id : Int} -> Int
id (Option option) =
  option.id

idAsString : Option { o | id : Int} -> String
idAsString (Option option) =
  option.id |> String.fromInt

value : Option { o | value: String} -> String
value (Option option) =
  option.value

checked : Option Checkbox -> Bool
checked (Option checkbox) =
  checkbox.checked

name : Option GroupedRadio -> String
name (Option groupedOption) =
  groupedOption.name

subs : Option GroupedRadio -> List (Option Radio)
subs (Option groupedOption) =
  groupedOption.subs

-- DECODER
radioListDecoder : Decoder (List (Option Radio))
radioListDecoder =
  Decode.list radioDecoder

radioDecoder : Decoder (Option Radio)
radioDecoder =
  Decode.map Option (Decode.succeed Radio
    |> Pipeline.required "id" Decode.int
    |> Pipeline.required "name" Decode.string
  )

groupedRadioListDecoder : Decoder (List (Option GroupedRadio))
groupedRadioListDecoder =
  Decode.list groupedRadioDecoder

groupedRadioDecoder : Decoder (Option GroupedRadio)
groupedRadioDecoder =
  Decode.map Option (Decode.succeed GroupedRadio
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "subs" (Decode.list radioDecoder)
  )

checkboxListDecoder : Decoder (List (Option Checkbox))
checkboxListDecoder =
  Decode.list checkboxDecoder

checkboxDecoder : Decoder (Option Checkbox)
checkboxDecoder =
  Decode.map Option (Decode.succeed Checkbox
    |> Pipeline.required "id" Decode.int
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "checked" Decode.bool
  )

-- ENCODER
radioEncoder : Option Radio -> Encode.Value
radioEncoder option =
  Encode.object
    [ ("id", Encode.int (id option))
    , ("value", Encode.string (value option))
    ]

checkboxEncoder : Option Checkbox -> Encode.Value
checkboxEncoder option =
  Encode.object
    [ ("id", Encode.int (id option))
    , ("value", Encode.string (value option))
    , ("checked", Encode.bool (checked option))
    ]

-- TRANSFORM

radioListToCheckboxList : List (Option Radio) -> List (Option Radio) -> List (Option Checkbox)
radioListToCheckboxList selectedOptions possibleOptions =
  List.map (\o -> radioToCheckbox selectedOptions o) possibleOptions

radioToCheckbox : List (Option Radio) -> Option Radio -> Option Checkbox
radioToCheckbox  selectedOptions option =
  let
    checkbox =
      { id = (id option)
      , value = (value option)
      , checked = List.any (\o -> (id o) == (id option)) selectedOptions
      }
  in
    Option checkbox

toTuples : List (Option Radio) -> List (String, String)
toTuples options =
  List.map (\option -> (String.fromInt (id option), (value option)) ) options

-- FORM

checkboxValidate : Form.Validation () (Option Checkbox)
checkboxValidate =
  Form.map Option checkValidate


checkValidate : Form.Validation () Checkbox
checkValidate =
    Form.succeed Checkbox
      |> Form.andMap (Form.field "id" (Form.int))
      |> Form.andMap (Form.field "value" (Form.string ))
      |> Form.andMap (Form.field "selected" (Form.bool |> Form.defaultValue False))

checkboxListFields :  List (Option Checkbox) -> List Field
checkboxListFields options =
  (List.map checkboxFields options)

checkboxFields : Option Checkbox -> Field
checkboxFields option =
  Form.Field.group
  [ ("id", Form.Field.string (idAsString option))
  , ("value", Form.Field.string (value option))
  , ("checked", Form.Field.bool (checked option))
  ]
initialCheckboxListFields : List (Option Radio) -> Bool -> List Field
initialCheckboxListFields options defaultChecked =
  (List.map (initialCheckboxFields defaultChecked) options)

initialCheckboxFields : Bool -> Option Radio -> Field
initialCheckboxFields defaultChecked option =
  Form.Field.group
  [ ("id", Form.Field.string (idAsString option))
  , ("value", Form.Field.string (value option))
  , ("checked", Form.Field.bool defaultChecked)
  ]

initEmptyTuple : (String, String)
initEmptyTuple =
  ("-1", "")

initEmptyRadioOption : Option Radio
initEmptyRadioOption =
  Option { id = -1, value = ""}

initEmptyGroupedRadioOption : Option GroupedRadio
initEmptyGroupedRadioOption =
  Option { name = "", subs = [ initEmptyRadioOption] }

selectedCheckboxOptionsToRadio : List (Option Checkbox) -> List (Option Radio)
selectedCheckboxOptionsToRadio options =
  List.filter (\o -> checked o) options |> List.map (\co -> Option {id = id co, value = value co})

initRadio : Int -> String -> Option Radio
initRadio id_ value_ =
  Option {id = id_, value = value_}

initGroupedRadioOption : String -> List (Option Radio) -> Option GroupedRadio
initGroupedRadioOption name_ subs_ =
  Option { name = name_, subs = subs_}

removeEmptyOption : List (Option Radio) -> List (Option Radio)
removeEmptyOption options =
  List.filter (\o -> value o /= "") options

addToSubs : Option GroupedRadio -> Option Radio -> Option GroupedRadio
addToSubs groupedRadio newOption =
  let
    updatedSubs = List.append [newOption] (subs groupedRadio)
  in
  Option
    { name = name groupedRadio
    , subs = List.append [newOption] (subs groupedRadio) |> sortOptionsByName
    }

sortOptionsByName : List (Option Radio) -> List (Option Radio)
sortOptionsByName options =
  List.sortBy (\o -> value o) options

addEmptyOption : List (Option Radio) -> List (Option Radio)
addEmptyOption options =
  initEmptyRadioOption :: options