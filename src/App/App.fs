module App

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Attr
open Sutil.DOM

[<SutilComponent>]
let view () =
    let count = Store.make 1
    let doubled = count |> Observable.map ((*) 2)
    let quadrupled = doubled |> Store.map ((*) 2)

    Html.div
        [
            disposeOnUnmount [ count ]

            class' "container my-5"

            Html.button
                [
                    class' "block"
                    onClick (fun _ -> count |> Store.modify (fun n -> n + 1)) []
                    text $"Count: %i{count.Signal}"
                ]

            Html.p [ class' "block"; text $"{count.Signal} * 2 = {doubled.Signal}" ]

            Html.p [ class' "block"; text $"{doubled.Signal} * 2 = {quadrupled.Signal}" ]
        ]

// Start the app
view () |> Program.mountElement "app-container"