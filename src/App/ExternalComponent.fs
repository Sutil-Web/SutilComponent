module ExternalComponent

open Sutil
open Sutil.Attr
open Sutil.DOM

printfn "Loading external component..."

[<SutilComponent>]
let SayHello (name1: string, name2: string) =
    Sutil.html $"""
    <p class="block">Hello {name1} and {name2}!</p>
    """
