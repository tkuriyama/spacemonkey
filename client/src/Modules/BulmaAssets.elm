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

type alias TextAreaModifiers =
    { size : BulmaM.Size
    , state : BulmaM.State
    , color : BulmaM.Color
    , readonly : Bool
    , disabled : Bool
    }

--------------------------------------------------------------------------------

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

defaultTextAreaMods : TextAreaModifiers
defaultTextAreaMods =
    { size = BulmaM.Standard
    , state = BulmaM.Active
    , color = BulmaM.Default
    , readonly = False
    , disabled = False
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

editButtonMods : BulmaE.ButtonModifiers msg
editButtonMods =
    { defaultButtonMods | color = BulmaM.Info }


cancelButtonMods : BulmaE.ButtonModifiers msg
cancelButtonMods =
    { defaultButtonMods | color = BulmaM.Light }

okButtonMods : BulmaE.ButtonModifiers msg
okButtonMods =
    { defaultButtonMods | color = BulmaM.Primary }

stdInputMods : InputModifiers msg
stdInputMods =
    let iconLeft =
            Just (BulmaM.Large, [], Html.i [HtmlA.class "fas fa-comment"] [])
    in { defaultInputMods | iconLeft = iconLeft }

linkInputMods : InputModifiers msg
linkInputMods =
    let iconLeft =
            Just (BulmaM.Large, [], Html.i [HtmlA.class "fas fa-link"] [])
    in { defaultInputMods | iconLeft = iconLeft }

textEditAreaMods : TextAreaModifiers
textEditAreaMods =
    { defaultTextAreaMods | readonly = False }

viewTextAreaMods : TextAreaModifiers
viewTextAreaMods =
    { defaultTextAreaMods | readonly = True }
