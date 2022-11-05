module App

// Adapted from
// https://svelte.dev/examples

open Sutil
open Sutil.Attr
open Sutil.DOM

[<SutilComponent>]
let viewTemplate () =
    let showCom = Store.make false
    let count = Store.make 1
    let doubled = count |> Observable.map ((*) 2)
    let quadrupled = doubled |> Store.map ((*) 2)

    Sutil.html
        $"""
    <div class="container my-5">
        {disposeOnUnmount [ count ]}
        <button
            class="block"
            onClick={fun _ -> count |> Store.modify (fun n -> n + 1)}>
            Count: %i{count.Signal}
        </button>
        <p class="block">{count.Signal} * 2 = %i{doubled.Signal}</p>
        <p class="block">{doubled.Signal} * 2 = {quadrupled.Signal}</p>

        <button
            class="block"
            onClick={fun _ -> showCom |> Store.modify not}>
            Show external
        </button>
        {if showCom.Signal then
             Sutil.lazyImport (ExternalComponent.SayHello("David", "Alfonso"))
         else
             nothing}
    </div>
    """

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

            Html.p [ class' "block"; text $"{count.Signal} * 2 = %i{doubled.Signal}" ]

            Html.p [ class' "block"; text $"{doubled.Signal} * 2 = {quadrupled.Signal}" ]
        ]

// Start the app
viewTemplate () |> Program.mountElement "app-container"
