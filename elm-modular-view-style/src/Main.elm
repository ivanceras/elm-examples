module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes

import Parent.View


import Parent



main =
  App.program { 
     init = Parent.init
    , view = Parent.View.view
    , update = Parent.update
    , subscriptions = Parent.subscriptions
   }

 



