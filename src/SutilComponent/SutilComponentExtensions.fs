namespace Sutil

open System
open Sutil.DOM
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions

[<AutoOpen>]
module SutilComponentExtensions =
    type IObservable<'T> with
        member _.Signal: 'T =
            failwith ".Signal calls must be transformed at compile-time with SutilComponent attribute"

    type Sutil =
        [<SutilTemplate>]
        static member html (template: string): SutilElement =
            failwith "HTML templates must be parsed at compile time"

        static member bindPromise (pr: JS.Promise<'T>, view : 'T -> SutilElement, ?onWaiting: SutilElement, ?onError: exn -> SutilElement) =
            let op = ObservablePromise<'T>()
            op.Run(pr)
            Bind.el(op, fun state ->
                match state with
                | Waiting -> match onWaiting with Some el -> el | None -> fragment []
                | State.Error e -> match onError with Some f -> f e | None -> JS.console.error(e); fragment []
                | Result r -> view r
            )

        /// Automatically creates a lazy component with a dynamic import to the referenced value
        static member inline lazyImport(call: SutilElement, ?onWaiting: SutilElement): SutilElement =
            Sutil.bindPromise(importValueDynamic call, id, ?onWaiting=onWaiting)
