namespace Sutil

open System
open Sutil.DOM

[<AutoOpen>]
module SutilComponentExtensions =
    type IObservable<'T> with
        member _.Signal: 'T =
            failwith ".Signal calls must be transformed at compile-time with SutilComponent attribute"

    module Sutil =
        [<SutilTemplate>]
        let html (template: string): SutilElement =
            failwith "HTML templates must be parsed at compile time"