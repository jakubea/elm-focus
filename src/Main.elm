module Main exposing (..)

import Browser
import Css
import Focus
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


---- MODEL ----


type alias Model =
    { billingAddress : Address
    , deliveryAddress : Address
    }


type alias Address =
    { firstName : String
    , lastName : String
    , street : String
    , zip : String
    , city : String
    }


init : ( Model, Cmd Msg )
init =
    ( { billingAddress = Address "" "" "" "" ""
      , deliveryAddress = Address "" "" "" "" ""
      }
    , Cmd.none
    )



---- UPDATE ----


type InputId
    = FirstName
    | LastName
    | Street
    | Zip
    | City


type AddressType
    = Billing InputId
    | Delivery InputId


type Msg
    = InsertedValue AddressType String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertedValue field value ->
            ( setValue model field value, Cmd.none )


setValue : Model -> AddressType -> String -> Model
setValue model field value =
    Focus.set (getFocus field) value model


getValue : Model -> AddressType -> String
getValue model field =
    Focus.get (getFocus field) model



-- FOCI


getFocus : AddressType -> Focus.Focus Model String
getFocus field =
    let
        billingAddressFocus =
            Focus.create .billingAddress (\f model -> { model | billingAddress = f model.billingAddress })

        deliveryAddressFocus =
            Focus.create .deliveryAddress (\f model -> { model | deliveryAddress = f model.deliveryAddress })
    in
        case field of
            Billing inputId ->
                (Focus.compose billingAddressFocus (inputFocus inputId))

            Delivery inputId ->
                (Focus.compose deliveryAddressFocus (inputFocus inputId))


inputFocus : InputId -> Focus.Focus Address String
inputFocus inputId =
    case inputId of
        FirstName ->
            Focus.create .firstName (\f address -> { address | firstName = f address.firstName })

        LastName ->
            Focus.create .lastName (\f address -> { address | lastName = f address.lastName })

        Street ->
            Focus.create .street (\f address -> { address | street = f address.street })

        Zip ->
            Focus.create .zip (\f address -> { address | zip = f address.zip })

        City ->
            Focus.create .city (\f address -> { address | city = f address.city })



---- VIEW ----


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "With Focus" ]
        , rowView
            [ addressFormView "Billing address" Billing model
            , addressFormView "Delivery address" Delivery model
            ]
        , rowView
            [ addressView "Billing address" Billing model
            , addressView "Delivery address" Delivery model
            ]
        ]


addressFormView : String -> (InputId -> AddressType) -> Model -> Html Msg
addressFormView heading addressType model =
    columnView
        [ Html.h3 [] [ Html.text heading ]
        , inputView "First name" model <| addressType FirstName
        , inputView "Last name" model <| addressType LastName
        , inputView "Street" model <| addressType Street
        , inputView "Zip" model <| addressType Zip
        , inputView "City" model <| addressType City
        ]


inputView : String -> Model -> AddressType -> Html Msg
inputView text model field =
    let
        value =
            getValue model field
    in
        Html.div
            [ Attributes.css
                [ Css.padding <| Css.px 5
                , Css.width <| Css.pct 100
                , Css.displayFlex
                , Css.flexDirection Css.row
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ Html.label [] [ Html.text <| text ++ ": " ]
            , Html.input [ Events.onInput <| InsertedValue field, Attributes.value value ] []
            ]


addressView : String -> (InputId -> AddressType) -> Model -> Html Msg
addressView heading addressType model =
    let
        value field =
            getValue model <| addressType field
    in
        columnView <|
            [ Html.h3 [] [ Html.text heading ]
            , boldSpanView "First name: " <| value FirstName
            , boldSpanView "Last name: " <| value LastName
            , boldSpanView "Street: " <| value Street
            , boldSpanView "ZIP: " <| value Zip
            , boldSpanView "City: " <| value City
            ]


boldSpanView : String -> String -> Html Msg
boldSpanView label value =
    Html.span []
        [ Html.span [ Attributes.css [ Css.fontWeight Css.bold ] ] [ Html.text label ]
        , Html.span [] [ Html.text value ]
        ]


rowView : List (Html Msg) -> Html Msg
rowView =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceAround
            ]
        ]


columnView : List (Html Msg) -> Html Msg
columnView =
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.alignItems Css.flexStart
            , Css.flexDirection Css.column
            , Css.padding <| Css.px 20
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> Html.toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
