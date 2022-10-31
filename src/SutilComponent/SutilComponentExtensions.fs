namespace Sutil

open System

[<AutoOpen>]
module SutilComponentExtensions =
    type IObservable<'T> with
        member _.Signal: 'T =
            failwith ".Signal calls must be transformed at compile-time with SutilComponent attribute"
