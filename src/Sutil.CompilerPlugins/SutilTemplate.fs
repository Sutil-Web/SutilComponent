namespace Sutil

open System.Text
open Fable
open Fable.AST
open Fable.AST.Fable

open AstUtils

module private SutilTemplateHelpers =
    open System.Text.RegularExpressions

    let OPEN_OR_CLOSE_TAG = Regex(@"</?[a-z\-]+|\$\$\d+")

    let KEY_VALUE_OR_END_TAG = Regex(@"\G\s*(@?[a-zA-Z\-]+\s*=\s*(:?"".*?""|\$\$\d+)|\/?>)")

    type Prop =
        | Prop of string * Expr

    type PropResult =
        | PropSuccess of int * Prop
        | CloseTag of int
        | CloseTagImmediate of int
        | PropFail

    type El =
        | HtmlEl of tag: string * props: Prop list * children: El list
        | TextEl of string
        | FSharpEl of Expr

    type ElResult =
        | ElSuccess of int * El
        | ElClosed of int
        | ElFail

    let softTrimStart (str: string) =
        if Regex.IsMatch(str, @"^ *\S") then str else str.TrimStart()

    let softTrimEnd (str: string) =
        if Regex.IsMatch(str, @"\S *$") then str else str.TrimEnd()

    let tryProp (idx: int) (str: string) (values: Expr list) =
        let m = KEY_VALUE_OR_END_TAG.Match(str, idx)
        if m.Success then
            let idx = m.Index + m.Length
            let kv = m.Groups[1].Value
            match kv with
            | ">" -> CloseTag idx
            | "/>" -> CloseTagImmediate idx
            | kv ->
                let k, v =
                    let i = kv.IndexOf('=')
                    kv.Substring(0, i).TrimEnd(),
                    kv.Substring(i + 1).TrimStart()
                let v =
                    if v.StartsWith("$$") then
                        let v = v.Substring(2) |> int
                        List.item v values
                    else
                        // TODO: Accept holes within a prop string?
                        v.Trim('"') |> makeStrConst
                PropSuccess (idx, Prop(k, v))
        else
            PropFail

    let rec tryEls isFirstChild (prevIdx: int) (str: string) (values: Expr list): ElResult list =
        let m = OPEN_OR_CLOSE_TAG.Match(str, prevIdx)
        if m.Success then [
            let tag = m.Value
            let isCloseTag = tag[1] = '/'
            let mutable idx = m.Index + m.Length

            let prev = str.[prevIdx .. m.Index - 1]
            // Trying to emulate React/HTML behaviour here but not entirely sure how we should do the trimming
            let prev = if isFirstChild then prev.TrimStart() else softTrimStart prev
            let prev = if isCloseTag then prev.TrimEnd() else softTrimEnd prev

            if not(System.String.IsNullOrWhiteSpace(prev)) then
                yield ElSuccess(m.Index, TextEl prev)

            if isCloseTag then
                // TODO: Check the tag corresponds to the currently open tag
                let closeIdx = str.IndexOf('>', idx)
                yield ElClosed((max closeIdx idx) + 1)

            elif tag.StartsWith("$$") then
                let v = tag.Substring(2) |> int
                yield ElSuccess(idx, List.item v values |> FSharpEl)

            else
                let tag = tag.Substring(1)                
                let mutable startChildren = false
                let mutable closeEl = false

                let props = [
                    while not startChildren && not closeEl do
                        match tryProp idx str values with
                        | PropSuccess(i, prop) -> idx <- i; yield prop
                        | CloseTag i -> idx <- i; startChildren <- true
                        | CloseTagImmediate i -> idx <- i; closeEl <- true
                        | PropFail -> startChildren <- true
                ]

                let mutable isFirstChild = true
                let children =
                    if closeEl then []
                    else [
                        while not closeEl do
                            let results = tryEls isFirstChild idx str values
                            isFirstChild <- false
                            for result in results do
                                match result with
                                | ElSuccess(i, el) -> idx <- i; yield el
                                | ElClosed i -> idx <- i; closeEl <- true
                                | ElFail -> closeEl <- true
                    ]

                yield ElSuccess(idx, HtmlEl(tag, props, children))
            ]
        else
            [ElFail]

open SutilTemplateHelpers
open SutilComponentHelpers

type SutilTemplateAttribute() =
    inherit MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "4.0"

    override _.TransformCall(compiler, _memb, expr) =
        match expr with
        | Call(_callee, CallInfoArgs(StringConstOrTemplate(parts, values)::_), _typeInfo, _range) ->
            let r = expr.Range
            let sutilElType = expr.Type
            let template, values =
                match parts with
                | [] -> "", []
                | [part] -> part, []
                | head::rest ->
                    let sb = StringBuilder(head)
                    rest |> List.iteri (fun i part ->
                        sb.Append("$$").Append(i).Append(part) |> ignore)
                    sb.ToString(), values
            match tryEls true 0 template values with
            | [ElSuccess(_, el)] ->
                let buildProp (Prop(key, value)) =
                    let isEvent, key =
                        if key.StartsWith('@') then
                            true, key.Substring(1)
                        elif key.StartsWith("on") then
                            true, key.Substring(2, 1).ToLower() + key.Substring(3)
                        else false, key
                    if isEvent then
                        callSutilFunction compiler r sutilElType "Attr.fs" "on" [
                            makeStrConst key
                            value
                            Value(NewList(None, Any), None)
                        ]
                    else
                        callSutilFunction compiler r sutilElType "DOM.fs" "attr" [
                            makeStrConst key
                            value
                        ]

                let rec buildEl = function
                    | FSharpEl(MaybeCasted el) ->
                        if el.Type = sutilElType then el
                        else callSutilFunction compiler r sutilElType "DOM.fs" "text" [el]
                    | TextEl text ->
                        [ makeStrConst text ]
                        |> callSutilFunction compiler r sutilElType "DOM.fs" "text"
                    | HtmlEl(tag, props, children) ->
                        if tag = "" then
                            [ List.map buildEl children |> makeArray ]
                            |> callSutilFunction compiler r sutilElType "DOM.fs" "fragment"
                        else
                            [
                                makeStrConst tag
                                makeArray <| List.append
                                    (List.map buildProp props)
                                    (List.map buildEl children)

                            ] |> callSutilFunction compiler r sutilElType "DOM.fs" "el"

                buildEl el
            | _ ->
                compiler.LogError("Cannot parse HTML template", ?range = expr.Range)
                expr
        | _ ->
            expr

    override _.Transform(compiler, _file, decl) = decl
