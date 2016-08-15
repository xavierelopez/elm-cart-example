module Cart.State exposing (init, update, subscriptions)

import Cart.Types exposing (..)
import Cart.Http exposing (..)
import HttpBuilder exposing (..)


availableProducts : List Product
availableProducts =
    [ Product
        "1"
        "Best product ever. Delivers instantly via email."
        Nothing
        "Product 1"
        1
        True
    , Product
        "2"
        "Delivers immediately via email."
        Nothing
        "Product 2"
        0.1
        True
    ]


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model (Cart []) availableProducts ProductList False Nothing Nothing

        command =
            (Cmd.none)
    in
        ( model, command )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


isSameItem : Item -> Item -> Bool
isSameItem item1 item2 =
    (item1.product.id == item2.product.id) && (item1.customization == item2.customization)


increaseItemQuantityForProduct : List Item -> Product -> List Item
increaseItemQuantityForProduct items product =
    let
        increaseQuantity =
            \item ->
                if (item.product.id == product.id) then
                    { item | quantity = item.quantity + 1 }
                else
                    item
    in
        List.map increaseQuantity items


addToCart : Cart -> Product -> Customization -> Cart
addToCart cart product customization =
    let
        quantity =
            Maybe.withDefault 1 customization.quantity

        updatedItems =
            List.append cart.items [ Item product quantity customization ]
    in
        { cart | items = (Debug.log "updated cart items" updatedItems) }


updateItemQuantity : Cart -> Item -> Int -> Cart
updateItemQuantity cart itemToUpdate quantity =
    let
        updatedItems =
            List.filterMap
                (\item ->
                    if (isSameItem item itemToUpdate) then
                        if quantity == 0 then
                            Nothing
                        else
                            Just { item | quantity = quantity }
                    else
                        Just item
                )
                cart.items
    in
        { cart | items = updatedItems }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddToCart product customization ->
            let
                model =
                    { model | cart = (addToCart model.cart product customization), page = Checkout }
            in
                ( model, Cmd.none )

        UpdateItemQuantity item quantity ->
            let
                model =
                    { model | cart = (updateItemQuantity model.cart item quantity) }
            in
                ( model, Cmd.none )

        RemoveItem item ->
            let
                model =
                    { model | cart = (updateItemQuantity model.cart item 0) }
            in
                ( model, Cmd.none )

        StartCheckout ->
            let
                model =
                    { model | page = Checkout }
            in
                ( model, Cmd.none )

        SelectProduct product ->
            let
                model =
                    { model | currentProduct = Just product, page = ProductDetail }
            in
                ( model, Cmd.none )

        GoHome ->
            let
                model =
                    { model | currentProduct = Nothing, currentCustomization = Nothing, page = ProductList }
            in
                ( model, Cmd.none )

        Customize customization ->
            let
                model =
                    { model | currentCustomization = Just customization }
            in
                ( model, Cmd.none )

        StartPurchase cart ->
            let
                model =
                    { model | purchaseInProgress = True }

                purchase =
                    Purchase cart.items

                command =
                    postPurchase purchase
            in
                ( model, command )

        PurchaseSucceed response ->
            let
                model =
                    { model | purchaseInProgress = False, page = CheckoutSuccess, cart = Cart [] }
            in
                ( model, Cmd.none )

        PurchaseFail error ->
            let
                model =
                    { model | purchaseInProgress = False }

                errorMessage =
                    case error of
                        HttpBuilder.BadResponse response ->
                            response.data

                        _ ->
                            "Something went wrong with the payment... Please try again."
            in
                ( model, Cmd.none )
