namespace Sutil

open Fable
open Fable.AST
open Fable.AST.Fable
open System.Text.RegularExpressions

open AstUtils

// Tell Fable to scan for plugins in this assembly
[<assembly: ScanForPlugins>]
do ()

module internal SutilComponentHelpers =
    let callSutilFunction (compiler: PluginHelper) r t fileName functionName args =
        let reg = Regex($@"Sutil\.\d.*?\/{fileName}$", RegexOptions.IgnoreCase)
        let importPath =
            compiler.SourceFiles
            |> Seq.tryFind reg.IsMatch
            |> Option.defaultWith (fun () ->
                compiler.LogError("Cannot find Sutil/Bindings source file", ?range = r)
                "../Sutil/" + fileName)
        let callee = makeImport functionName importPath
        makeTypedCall t callee args

open SutilComponentHelpers

type SutilComponentAttribute() =
    inherit MemberDeclarationPluginAttribute()
    override _.FableMinimumVersion = "4.0"

    override _.TransformCall(_compiler, _memb, expr) = expr

    override _.Transform(compiler, _file, decl) =
        let rec checkBindings (expr: Expr) =
            let signals = System.Collections.Generic.Dictionary()

            let rec checkBindingsInner (expr: Expr) =
                // printfn
                //     "Visiting... %A%s"
                //     (let uci, _ = Reflection.FSharpValue.GetUnionFields(expr, typeof<Expr>)
                //      uci.Name)
                //     (expr.Range
                //      |> Option.map (fun r -> $" ({r.start.line}, {r.start.column})")
                //      |> Option.defaultValue "")

                expr
                |> visit (function
                    | Call (_, (CallInfoMemberRef (ent, memb) as info), t, _) as expr when
                        ent = "Sutil.SutilComponentExtensions" && memb = "IObservable`1.get_Signal"
                        ->
                        match info.ThisArg with
                        | Some (MaybeCasted (IdentExpr ident)) ->
                            match signals.TryGetValue(ident) with
                            | true, signalIdent -> IdentExpr signalIdent
                            | false, _ ->
                                let signalIdent = makeUniqueName ident.Name |> makeTypedIdent t
                                signals.Add(ident, signalIdent)
                                IdentExpr signalIdent
                        | _ ->
                            compiler.LogError(".Signal can only be called on identifiers", ?range = expr.Range)
                            expr

                    | ExprType (DeclaredType (ent, _)) as expr when ent.FullName = "Sutil.DOM.SutilElement" ->
                        checkBindings expr

                    | expr -> checkBindingsInner expr)

            let expr = checkBindingsInner expr

            // Add helpers for 3, 4... auto-bindings?
            let maxBindings = 2

            if signals.Count > maxBindings then
                let errorMessage =
                    $"Cannot bind more than {maxBindings} signals in the same expression"

                compiler.LogError(errorMessage, ?range = expr.Range)
                expr

            elif signals.Count > 0 then
                let suffix, bindArgs, bindBody =
                    if signals.Count = 1 then
                        "", Seq.head signals.Values, expr
                    else
                        let mutable idx = -1
                        let bindArgs = makeUniqueName "bind" |> makeIdent

                        string signals.Count,
                        bindArgs,
                        signals.Values
                        |> Seq.fold
                            (fun body ident ->
                                idx <- idx + 1
                                Let(ident, Get(IdentExpr bindArgs, TupleIndex idx, ident.Type, None), body))
                            expr

                callSutilFunction compiler expr.Range expr.Type "Bindings.fs" $"bindElement{suffix}" [
                    yield! signals |> Seq.map (fun kv -> IdentExpr kv.Key)
                    Lambda(bindArgs, bindBody, None)
                ]
            else
                expr

        // TODO: Check decl.Body returns SutilElement
        { decl with
            Body = checkBindings decl.Body
        }
