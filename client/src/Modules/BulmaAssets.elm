module Modules.BulmaAssets exposing (..)

import Bulma.Elements as BulmaE
import Bulma.Form as BulmaF
import Bulma.Modifiers as BulmaM

import Html as Html
import Html.Attributes as HtmlA

--------------------------------------------------------------------------------
-- Modifier Defaults

type alias InputModifiers msg =
    { size : BulmaM.Size
    , state : BulmaM.State
    , color : BulmaM.Color
    , expanded : Bool
    , rounded : Bool
    , readonly : Bool
    , disabled : Bool
    , iconLeft :
        Maybe ( BulmaM.Size, List (Html.Attribute msg), BulmaE.IconBody msg )
    , iconRight :
        Maybe ( BulmaM.Size, List (Html.Attribute msg), BulmaE.IconBody msg )
    }

defaultInputMods : InputModifiers msg
defaultInputMods =
    { size = BulmaM.Large
    , state = BulmaM.Active
    , color = BulmaM.Default
    , expanded = True
    , rounded = True
    , readonly = False
    , disabled = False
    , iconLeft = Nothing
    , iconRight = Nothing
    }

defaultButtonMods : BulmaE.ButtonModifiers msg
defaultButtonMods =
    { disabled = False
    , outlined = False
    , rounded = True
    , inverted = False
    , size     = BulmaM.Large
    , state    = BulmaM.Blur
    , static = False
    , color    = BulmaM.Default
    , iconLeft = Nothing
    , iconRight = Nothing
    }

--------------------------------------------------------------------------------
-- Modifier Instances

popupInputMods : InputModifiers msg
popupInputMods =
    let iconLeft =
            Just (BulmaM.Large, [], Html.i [HtmlA.class "fas fa-comment"] [])
    in { defaultInputMods | iconLeft = iconLeft }

cancelButtonMods : BulmaE.ButtonModifiers msg
cancelButtonMods =
    { defaultButtonMods | color = BulmaM.Light }

okButtonMods : BulmaE.ButtonModifiers msg
okButtonMods =
    { defaultButtonMods | color = BulmaM.Primary }



