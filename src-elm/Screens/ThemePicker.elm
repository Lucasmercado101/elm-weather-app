module Screens.ThemePicker exposing (..)

import Api.Address
import Api.Weather
import Cmd.Extra exposing (pure)
import Components exposing (statCard)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, height, none, padding, paddingXY, paragraph, px, rgb255, row, scrollbarY, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html
import Html.Attributes
import Html.Events
import List.Nonempty as NEList exposing (Nonempty(..))
import Localizations exposing (Language)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Ports
import Time exposing (Posix, Zone)
import Utils exposing (..)



-- MSG


type ThemePickerMsg
    = GoToMainScreen
    | BeginEditingTheme Theme
    | ChangedPrimaryColor String
    | ChangedSecondaryColor String
    | ApplyTheme Theme Bool
    | CancelCustomizingTheme
    | MoveCustomThemeAbove Theme
    | MoveCustomThemeBelow Theme
    | DeleteCustomTheme Theme



-- MODEL


type alias ThemePickerModel =
    { language : Language
    , currentTheme : Theme
    , customThemes : Maybe CustomThemes
    , location : Location
    , zone : Zone
    , apiData : ( Api.Weather.WeatherData, Posix )
    , currentAddress : Maybe Api.Address.Address

    --
    , customizingTheme :
        Maybe
            { originalTheme : Theme
            , customTheme : Theme
            }

    -- Parent will check this
    , exitScreen : Bool
    }



-- SUBSCRIPTIONS


themePickerSubscriptions : ThemePickerModel -> Sub ThemePickerMsg
themePickerSubscriptions _ =
    Sub.none



-- UPDATE


themePickerUpdate : ThemePickerMsg -> ThemePickerModel -> ( ThemePickerModel, Cmd ThemePickerMsg )
themePickerUpdate msg model =
    case msg of
        GoToMainScreen ->
            { model | exitScreen = True }
                |> pure

        MoveCustomThemeAbove theme ->
            case model.customThemes of
                Just customThemes ->
                    { model | customThemes = Just (moveLeft customThemes theme) } |> pure

                Nothing ->
                    model |> pure

        MoveCustomThemeBelow theme ->
            case model.customThemes of
                Just customThemes ->
                    { model | customThemes = Just (moveRight customThemes theme) } |> pure

                Nothing ->
                    model |> pure

        DeleteCustomTheme theme ->
            case model.customThemes of
                Just customThemes ->
                    let
                        newCustomThemes : Maybe (Nonempty ( PrimaryColor, SecondaryColor ))
                        newCustomThemes =
                            if NEList.length customThemes == 1 then
                                Nothing

                            else
                                Just
                                    (NEList.filter
                                        (\( primary, secondary ) ->
                                            not (primary == Tuple.first theme && secondary == Tuple.second theme)
                                        )
                                        theme
                                        customThemes
                                    )
                    in
                    { model | customThemes = newCustomThemes }
                        |> pure

                Nothing ->
                    model |> pure

        BeginEditingTheme ( primary, secondary ) ->
            { model
                | customizingTheme =
                    Just
                        { originalTheme = ( primary, secondary )
                        , customTheme = ( primary, secondary )
                        }
            }
                |> pure

        ChangedPrimaryColor newPrimaryColor ->
            case model.customizingTheme of
                Nothing ->
                    model |> pure

                Just { originalTheme, customTheme } ->
                    { model
                        | customizingTheme =
                            Just
                                { originalTheme = originalTheme
                                , customTheme =
                                    ( newPrimaryColor |> hexToColor |> Result.withDefault (Tuple.first originalTheme)
                                    , Tuple.second customTheme
                                    )
                                }
                    }
                        |> pure

        ChangedSecondaryColor newSecondaryColor ->
            case model.customizingTheme of
                Nothing ->
                    model
                        |> pure

                Just { originalTheme, customTheme } ->
                    { model
                        | customizingTheme =
                            Just
                                { originalTheme = originalTheme
                                , customTheme =
                                    ( Tuple.first customTheme
                                    , newSecondaryColor |> hexToColor |> Result.withDefault (Tuple.second originalTheme)
                                    )
                                }
                    }
                        |> pure

        CancelCustomizingTheme ->
            { model | customizingTheme = Nothing }
                |> pure

        ApplyTheme ( primary, secondary ) isCustomTheme ->
            let
                primaryColors : { red : Float, green : Float, blue : Float, alpha : Float }
                primaryColors =
                    Element.toRgb primary

                secondaryColors : { red : Float, green : Float, blue : Float, alpha : Float }
                secondaryColors =
                    Element.toRgb secondary
            in
            ( { model
                | currentTheme = ( primary, secondary )
                , customizingTheme = Nothing
                , customThemes =
                    if isCustomTheme then
                        case model.customThemes of
                            Just allCustomThemes ->
                                ( primary, secondary )
                                    |> addCustomTheme allCustomThemes
                                    |> Just

                            Nothing ->
                                ( primary, secondary )
                                    |> NEList.singleton
                                    |> Just

                    else
                        model.customThemes
              }
            , Cmd.batch
                [ Ports.changedTheme
                    ( ( primaryColors.red, primaryColors.green, primaryColors.blue )
                    , ( secondaryColors.red, secondaryColors.green, secondaryColors.blue )
                    )
                , if isCustomTheme then
                    case model.customThemes of
                        Just themes ->
                            Ports.saveCustomThemes
                                (( ( primaryColors.red, primaryColors.green, primaryColors.blue )
                                 , ( secondaryColors.red, secondaryColors.green, secondaryColors.blue )
                                 )
                                    :: customThemesToPort themes
                                )

                        Nothing ->
                            Ports.saveCustomThemes
                                [ ( ( primaryColors.red, primaryColors.green, primaryColors.blue )
                                  , ( secondaryColors.red, secondaryColors.green, secondaryColors.blue )
                                  )
                                ]

                  else
                    Cmd.none
                ]
            )


themePickerInit : Language -> Theme -> Zone -> Location -> ( Api.Weather.WeatherData, Posix ) -> Maybe Api.Address.Address -> Maybe (Nonempty Theme) -> ThemePickerModel
themePickerInit lang currentTheme zone location apiData currentAddress customThemes =
    { language = lang
    , currentTheme = currentTheme
    , customThemes = customThemes
    , customizingTheme = Nothing
    , location = location
    , zone = zone
    , apiData = apiData
    , currentAddress = currentAddress

    -- Parent will check this
    , exitScreen = False
    }



-- VIEW


themePickerView : ThemePickerModel -> Element ThemePickerMsg
themePickerView ({ language, currentTheme, customThemes } as model) =
    let
        ( currentPrimaryColor, currentSecondaryColor ) =
            currentTheme
    in
    column [ width fill, height fill ]
        [ row
            [ width fill
            , height (px 52)
            , Background.color currentSecondaryColor
            ]
            [ button
                [ height fill
                , Font.color currentPrimaryColor
                , paddingX 8
                ]
                { label = el [ centerX, centerY ] (Icons.chevron_left 40 Inherit |> Element.html)
                , onPress = Just GoToMainScreen
                }
            , el [ width fill, Font.alignRight, Font.color currentPrimaryColor, Font.bold, paddingRight 15 ] (text (Localizations.theme language))
            ]

        -- Divider
        , el [ width fill, height (px 2), Background.color currentPrimaryColor ] none
        , column
            [ width fill
            , height fill
            , scrollbarY
            , Background.color currentPrimaryColor
            ]
            (let
                defaultThemes : List ( Color, Color )
                defaultThemes =
                    [ -- Dark
                      ( rgb255 25 20 20, rgb255 29 185 84 )
                    , ( rgb255 32 38 46, rgb255 205 88 136 )
                    , ( rgb255 3 0 28, rgb255 182 234 218 )
                    , ( rgb255 0 24 14, rgb255 255 170 207 )
                    , ( rgb255 42 45 52, rgb255 48 197 255 )
                    , ( rgb255 51 44 57, rgb255 240 238 237 )
                    , ( rgb255 49 51 56, white )
                    , ( rgb255 57 50 50, rgb255 228 130 87 )
                    , ( rgb255 205 88 136, rgb255 32 38 46 )
                    , ( rgb255 1 127 1, rgb255 22 22 22 )
                    , ( rgb255 120 1 22, rgb255 247 181 56 )
                    , ( rgb255 88 101 242, black )
                    , ( rgb255 235 69 95, rgb255 43 52 103 )
                    , ( rgb255 36 55 99, rgb255 255 110 49 )
                    , ( rgb255 13 0 90, rgb255 3 201 136 )
                    , ( rgb255 43 52 103, rgb255 235 69 95 )
                    , ( rgb255 69 60 103, rgb255 242 247 161 )
                    , ( rgb255 22 22 22, rgb255 0 165 0 )
                    , ( rgb255 155 188 15, rgb255 15 56 15 )
                    , ( black, white )

                    -- Light
                    , ( defaultPrimary, black )
                    , ( rgb255 66 198 255, black )
                    , ( rgb255 255 101 212, black )
                    , ( white, black )
                    , ( rgb255 240 238 237, rgb255 201 44 109 )
                    , ( rgb255 249 245 231, rgb255 167 114 125 )
                    , ( rgb255 238 233 218, rgb255 96 150 180 )
                    , ( rgb255 167 114 125, rgb255 249 245 231 )
                    , ( rgb255 96 150 180, rgb255 238 233 218 )
                    , ( rgb255 239 0 0, black )
                    , ( rgb255 212 246 204, rgb255 239 91 12 )
                    , ( rgb255 255 170 207, rgb255 0 24 14 )
                    , ( rgb255 255 27 143, rgb255 242 227 241 )
                    , ( rgb255 255 227 244, rgb255 255 27 143 )
                    , ( rgb255 240 238 237, rgb255 51 44 57 )
                    , ( rgb255 29 155 240, white )
                    , ( rgb255 59 89 153, rgb255 248 248 248 )
                    ]

                toDemoCards : List ( Color, Color ) -> List (Element ThemePickerMsg)
                toDemoCards =
                    List.map
                        (\themeColors ->
                            let
                                currentlyCustomizingTheme : ( Color, Color ) -> Element ThemePickerMsg
                                currentlyCustomizingTheme ( customFirstColor, customSecondColor ) =
                                    column [ width fill, Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 } ]
                                        [ row [ Font.bold, paddingXY 15 8, width fill ]
                                            [ el [ width fill ] (text (Localizations.primaryColor model.language ++ ":"))
                                            , Html.input
                                                [ Html.Attributes.type_ "color"
                                                , Html.Attributes.style "all" "unset"
                                                , Html.Attributes.style "height" "45px"
                                                , Html.Attributes.style "width" "45px"

                                                -- NOTE: purposefully not preventing
                                                -- the color from being too dark
                                                , Html.Attributes.value
                                                    (toRgb customFirstColor
                                                        |> (\{ blue, green, red } ->
                                                                List.map toHex
                                                                    [ round (red * 255)
                                                                    , round (green * 255)
                                                                    , round (blue * 255)

                                                                    -- don't know how to do alpha so i'm just omitting it here
                                                                    -- , round (alpha * 255)
                                                                    ]
                                                                    |> (::) "#"
                                                                    |> String.concat
                                                           )
                                                    )
                                                , Html.Events.onInput ChangedPrimaryColor
                                                ]
                                                []
                                                |> Element.html
                                            ]
                                        , row [ Font.bold, paddingXY 15 8, width fill ]
                                            [ el [ width fill ] (text (Localizations.secondaryColor model.language ++ ":"))
                                            , Html.input
                                                [ Html.Attributes.type_ "color"
                                                , Html.Attributes.style "all" "unset"
                                                , Html.Attributes.style "height" "45px"
                                                , Html.Attributes.style "width" "45px"

                                                -- NOTE: purposefully not preventing
                                                -- the color from being too dark
                                                , Html.Attributes.value
                                                    (toRgb customSecondColor
                                                        |> (\{ blue, green, red } ->
                                                                List.map toHex
                                                                    [ round (red * 255)
                                                                    , round (green * 255)
                                                                    , round (blue * 255)

                                                                    -- don't know how to do alpha so i'm just omitting it here
                                                                    -- , round (alpha * 255)
                                                                    ]
                                                                    |> (::) "#"
                                                                    |> String.concat
                                                           )
                                                    )
                                                , Html.Events.onInput ChangedSecondaryColor
                                                ]
                                                []
                                                |> Element.html
                                            ]
                                        , row [ width fill, Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 } ]
                                            [ button
                                                [ paddingY 12
                                                , Font.size 22
                                                , width fill
                                                , Font.center
                                                , height fill
                                                ]
                                                { label = text (Localizations.cancel model.language), onPress = Just CancelCustomizingTheme }
                                            , verticalDivider
                                            , button [ width fill, height fill ]
                                                { label = el [ centerX, Font.size 22, Font.center, width fill ] (text (Localizations.apply model.language))
                                                , onPress =
                                                    Just
                                                        (ApplyTheme ( customFirstColor, customSecondColor )
                                                            (not (( customFirstColor, customSecondColor ) == themeColors))
                                                        )
                                                }
                                            ]
                                        ]

                                themeApplied : Bool
                                themeApplied =
                                    themeColors == model.currentTheme

                                verticalDivider : Element msg
                                verticalDivider =
                                    el [ width fill, height fill, width (px 2), Background.color (Tuple.second themeColors) ] none

                                initialButtons : Element ThemePickerMsg
                                initialButtons =
                                    row [ width fill, Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 } ]
                                        [ button
                                            [ paddingY 12
                                            , Font.size 22
                                            , width fill
                                            , Font.center
                                            , height fill
                                            ]
                                            { label = text (Localizations.edit model.language)
                                            , onPress = Just (BeginEditingTheme themeColors)
                                            }
                                        , verticalDivider
                                        , if themeApplied then
                                            el
                                                [ width fill
                                                , height fill
                                                , Font.color (Tuple.first themeColors)
                                                , Background.color (Tuple.second themeColors)
                                                ]
                                                (el
                                                    [ centerX
                                                    , centerY
                                                    , Font.size 22
                                                    , Font.center
                                                    ]
                                                    (text (Localizations.applied model.language))
                                                )

                                          else
                                            let
                                                applyPreExistingTheme : ThemePickerMsg
                                                applyPreExistingTheme =
                                                    ApplyTheme themeColors False
                                            in
                                            button [ width fill, height fill ]
                                                { label = el [ centerX, Font.size 22, Font.center, width fill ] (text (Localizations.apply model.language))
                                                , onPress = Just applyPreExistingTheme
                                                }
                                        ]

                                customThemeTopButtons : Maybe (Element ThemePickerMsg)
                                customThemeTopButtons =
                                    model.customThemes
                                        |> Maybe.andThen
                                            (\themes ->
                                                if NEList.member themeColors themes then
                                                    Just (topButtons (MoveCustomThemeBelow themeColors) (MoveCustomThemeAbove themeColors) (DeleteCustomTheme themeColors))

                                                else
                                                    Nothing
                                            )
                            in
                            case model.customizingTheme of
                                Just { customTheme, originalTheme } ->
                                    if themeColors == originalTheme then
                                        themePreviewCard model.language customTheme (currentlyCustomizingTheme customTheme) customThemeTopButtons

                                    else
                                        themePreviewCard model.language themeColors initialButtons customThemeTopButtons

                                Nothing ->
                                    themePreviewCard model.language themeColors initialButtons customThemeTopButtons
                        )
             in
             (case customThemes of
                Just _ ->
                    [ paragraph [ Font.color currentSecondaryColor, padding 8, Font.heavy, Font.size 26 ] [ text (Localizations.customThemes model.language) ] ]

                Nothing ->
                    []
             )
                ++ ((case customThemes of
                        Just val ->
                            val |> NEList.toList

                        Nothing ->
                            []
                    )
                        |> toDemoCards
                   )
                ++ (case customThemes of
                        Just _ ->
                            [ paragraph [ Font.color currentSecondaryColor, padding 8, Font.heavy, Font.size 26 ] [ text (Localizations.themes model.language) ] ]

                        Nothing ->
                            []
                   )
                ++ (defaultThemes |> toDemoCards)
            )
        ]


themePreviewCard : Language -> Theme -> Element ThemePickerMsg -> Maybe (Element ThemePickerMsg) -> Element ThemePickerMsg
themePreviewCard language ( cardThemePrimaryColor, cardThemeSecondaryColor ) bottomElements topElements =
    el [ padding 8, width fill ]
        (column
            [ width fill
            , Background.color cardThemePrimaryColor
            , Font.color cardThemeSecondaryColor
            , Border.width 2
            , Border.color cardThemeSecondaryColor
            ]
            [ topElements |> Maybe.withDefault none
            , row
                [ padding 15
                , spacing 8
                , width fill
                ]
                [ column
                    [ width fill ]
                    [ paragraph [ Font.size 42, Font.heavy, paddingBottom 18 ] [ text "21°" ]
                    , paragraph [ Font.heavy, width fill, paddingBottom 8 ] [ text (Localizations.dailySummary language) ]
                    , paragraph [ Font.size 16, width fill ] [ Localizations.nowItFeels language (Just 33.4) (Just 21.1) ]
                    ]
                , el [ Background.color cardThemeSecondaryColor, Border.rounded 12, padding 12 ]
                    (statCard cardThemePrimaryColor
                        Icons.visibility
                        (Localizations.visibility language)
                        "25km/h"
                    )
                ]
            , bottomElements
            ]
        )


topButtons : ThemePickerMsg -> ThemePickerMsg -> ThemePickerMsg -> Element ThemePickerMsg
topButtons onMoveDown onMoveUp onDelete =
    row
        [ width fill
        , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        ]
        [ button []
            { label =
                el
                    [ padding 6
                    , Font.size 22
                    , Font.center
                    , height fill
                    , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
                    ]
                    (Icons.arrow_downward 30 Inherit |> Element.html)
            , onPress = Just onMoveDown
            }
        , button [ height fill ]
            { label =
                el
                    [ padding 6
                    , Font.size 22
                    , Font.center
                    , height fill
                    , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
                    ]
                    (Icons.arrow_upward 30 Inherit |> Element.html)
            , onPress = Just onMoveUp
            }
        , el [ width fill ] none
        , button [ height fill ]
            { label =
                el
                    [ height fill
                    , Border.widthEach { bottom = 0, top = 0, left = 2, right = 0 }
                    , paddingX 8
                    ]
                    (el
                        [ centerX
                        , centerY
                        , Font.heavy
                        ]
                        (Icons.close 30 Inherit |> Element.html)
                    )
            , onPress = Just onDelete
            }
        ]



-- Helpers


type alias CustomThemes =
    Nonempty Theme


addCustomTheme : CustomThemes -> Theme -> Nonempty Theme
addCustomTheme themes theme =
    if NEList.length themes == 10 then
        themes |> NEList.pop |> NEList.cons theme

    else
        NEList.cons theme themes


customThemesToPort : CustomThemes -> List ( RGB, RGB )
customThemesToPort themes =
    themes
        |> NEList.toList
        |> List.map
            (\( a, b ) ->
                let
                    primary : { red : Float, green : Float, blue : Float, alpha : Float }
                    primary =
                        Element.toRgb a

                    secondary : { red : Float, green : Float, blue : Float, alpha : Float }
                    secondary =
                        Element.toRgb b
                in
                ( ( primary.red, primary.green, primary.blue )
                , ( secondary.red
                  , secondary.green
                  , secondary.blue
                  )
                )
            )


{-| Add element to the tail of the list
-}
unshift : a -> Nonempty a -> Nonempty a
unshift item list =
    NEList.reverse list |> NEList.cons item |> NEList.reverse


moveLeft : Nonempty a -> a -> Nonempty a
moveLeft (Nonempty first rest) needle =
    if first == needle then
        Nonempty first rest

    else
        case rest of
            [] ->
                NEList.singleton first

            [ second ] ->
                if second == needle then
                    Nonempty second [ first ]

                else
                    Nonempty first [ second ]

            second :: restItems ->
                if second == needle then
                    Nonempty second (first :: restItems)

                else
                    Nonempty first (moveLeft (Nonempty second restItems) needle |> NEList.toList)


moveRight : Nonempty a -> a -> Nonempty a
moveRight (Nonempty first rest) needle =
    case rest of
        [] ->
            NEList.singleton first

        [ second ] ->
            if first == needle then
                Nonempty second [ first ]

            else
                Nonempty first [ second ]

        second :: restItems ->
            if first == needle then
                Nonempty second (needle :: restItems)

            else
                Nonempty first (moveRight (Nonempty second restItems) needle |> NEList.toList)
