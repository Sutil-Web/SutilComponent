module Sutil.AstUtils

open Fable
open Fable.AST
open System
open System.Linq
open System.Text.RegularExpressions

let (|ExprType|) (e: Fable.Expr) = e.Type

let (|CallInfoMemberRef|_|) (info: Fable.CallInfo) =
    match info.MemberRef with
    | Some (Fable.MemberRef (ent, memb)) -> Some(ent.FullName, memb.CompiledName)
    // | Some (Fable.GeneratedMemberRef memb) ->
    //     let entFullName =
    //         memb.Info.DeclaringEntity
    //         |> Option.map (fun ent -> ent.FullName)
    //         |> Option.defaultValue ""
    //     Some(entFullName, memb.Info.Name)
    | _ -> None

let (|CallInfoArgs|) (info: Fable.CallInfo) = info.Args

let (|StringConstOrTemplate|_|) (expr: Fable.Expr) =
    match expr with
    | Fable.Value(Fable.StringConstant s, _) -> Some([s], [])
    | Fable.Value(Fable.StringTemplate(_, parts, values), _) -> Some(parts, values)
    | _ -> None

let rec (|MaybeCasted|) = function
    | Fable.TypeCast(MaybeCasted e,_) -> e
    | e -> e

let cleanFullDisplayName str =
    Regex.Replace(str, @"`\d+", "").Replace(".", "_")

let makeTypedIdent t name: Fable.Ident =
    { Name = name
      Type = t
      IsCompilerGenerated = true
      IsThisArgument = false
      IsMutable = false
      Range = None }

let makeIdent name =
    makeTypedIdent Fable.Any name

let makeUniqueName (name: string) =
    let hashToString (i: int) =
        if i < 0
        then "Z" + (abs i).ToString("X")
        else i.ToString("X")
    "$" + name + (Guid.NewGuid().GetHashCode() |> hashToString)

let makeValue r value =
    Fable.Value(value, r)

let makeStrConst (x: string) =
    Fable.StringConstant x
    |> makeValue None

let makeArray (xs: Fable.Expr list) =
    Fable.NewArray(Fable.ArrayValues xs, Fable.Any, Fable.MutableArray)
    |> makeValue None

let nullValue = Fable.Expr.Value(Fable.ValueKind.Null(Fable.Type.Any), None)

let makeCallInfo args: Fable.CallInfo = {
    ThisArg = None
    Args = args
    SignatureArgTypes = []
    GenericArgs = []
    MemberRef = None
    Tags = []
}

let emitJs macro args  =
    let callInfo = makeCallInfo args

    let emitInfo : Fable.AST.Fable.EmitInfo =
        { Macro = macro
          IsStatement = false
          CallInfo = callInfo }

    Fable.Expr.Emit(emitInfo, Fable.Type.Any, None)

let rec flattenList (head: Fable.Expr) (tail: Fable.Expr) =
    [
        yield head;
        match tail with
        | Fable.Expr.Value (value, range) ->
            match value with
            | Fable.ValueKind.NewList(Some(nextHead, nextTail), listType) ->
                yield! flattenList nextHead nextTail
            | Fable.ValueKind.NewList(None, listType) ->
                yield! [ ]
            | _ ->
                yield! [ Fable.Expr.Value (value, range) ]

        | _ ->
            yield! [ ]
    ]

let makeImport (selector: string) (path: string) =
    Fable.Import({ Selector = selector.Trim()
                   Path = path.Trim()
                   Kind = Fable.UserImport(false) }, Fable.Any, None)

let isRecord (compiler: PluginHelper) (fableType: Fable.Type) =
    match fableType with
    | Fable.Type.AnonymousRecordType _ -> true
    | Fable.Type.DeclaredType (entity, genericArgs) -> compiler.GetEntity(entity).IsFSharpRecord
    | _ -> false

let isPropertyList (compiler: PluginHelper) (fableType: Fable.Type) =
    match fableType with
    | Fable.Type.List(genericArg) ->
        match genericArg with
        | Fable.Type.DeclaredType (entity, genericArgs) -> entity.FullName.EndsWith "IReactProperty"
        | _ -> false
    | _ -> false

let isPascalCase (input: string) = not (String.IsNullOrWhiteSpace input) && List.contains input.[0] ['A' .. 'Z']
let isCamelCase (input: string) = not (isPascalCase input)

let isAnonymousRecord (fableType: Fable.Type) =
    match fableType with
    | Fable.Type.AnonymousRecordType  _ -> true
    | _ -> false

let isReactElement (fableType: Fable.Type) =
    match fableType with
    | Fable.Type.DeclaredType (entity, genericArgs) -> entity.FullName.EndsWith "ReactElement"
    | _ -> false

let recordHasField name (compiler: PluginHelper) (fableType: Fable.Type) =
    match fableType with
    | Fable.Type.AnonymousRecordType (fieldNames, genericArgs, _isStruct) ->
        fieldNames
        |> Array.exists (fun field -> field = name)

    | Fable.Type.DeclaredType (entity, genericArgs) ->
        compiler.GetEntity(entity).FSharpFields
        |> List.exists (fun field -> field.Name = name)

    | _ ->
        false

let memberName = function
    | Fable.MemberRef(_,m) -> m.CompiledName
    | Fable.GeneratedMemberRef m -> m.Info.Name

let makeTypedCall t callee args =
    Fable.Call(callee, makeCallInfo args, t, None)

let makeCall callee args =
    makeTypedCall Fable.Any callee args

let createElement reactElementType args =
    let callee = makeImport "createElement" "react"
    Fable.Call(callee, makeCallInfo args, reactElementType, None)

let emptyReactElement reactElementType =
    Fable.Expr.Value(Fable.Null(reactElementType), None)

let makeMemberInfo isInstance typ name: Fable.GeneratedMemberInfo = {
    Name = name
    ParamTypes = []
    ReturnType = typ
    IsInstance = isInstance
    HasSpread = false
    IsMutable = false
    DeclaringEntity = None
}

let objValue (k, v): Fable.ObjectExprMember =
    {
        Name = k
        Args = []
        Body = v
        MemberRef =
            makeMemberInfo true v.Type k
            |> Fable.GeneratedValue
            |> Fable.GeneratedMemberRef
        IsMangled = false
    }

let objExpr kvs = Fable.ObjectExpr(List.map objValue kvs, Fable.Any, None)

let capitalize (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

let camelCase (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToLower() + String.Join("", input.Skip(1))

open Fable.AST.Fable

let visit f e =
    match e with
    | Unresolved _ -> e // Unresolved expressions must be matched explicitly
    | IdentExpr _ -> e
    | TypeCast(e, t) -> TypeCast(f e, t)
    | Import(info, t, r) ->
        Import({ info with Selector = info.Selector
                           Path = info.Path }, t, r)
    | Extended(kind, r) ->
        match kind with
        | Curry(e, arity) -> Extended(Curry(f e, arity), r)
        | Throw(e, t) -> Extended(Throw(Option.map f e, t), r)
        | Debugger -> e
    | Value(kind, r) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> e
        | StringTemplate(tag, parts, exprs) -> StringTemplate(tag, parts, List.map f exprs) |> makeValue r
        | NewOption(e, t, isStruct) -> NewOption(Option.map f e, t, isStruct) |> makeValue r
        | NewTuple(exprs, isStruct) -> NewTuple(List.map f exprs, isStruct) |> makeValue r
        | NewArray(ArrayValues exprs, t, i) -> NewArray(List.map f exprs |> ArrayValues, t, i) |> makeValue r
        | NewArray(ArrayFrom expr, t, i) -> NewArray(f expr |> ArrayFrom, t, i) |> makeValue r
        | NewArray(ArrayAlloc expr, t, i) -> NewArray(f expr |> ArrayAlloc, t, i) |> makeValue r
        | NewList(ht, t) ->
            let ht = ht |> Option.map (fun (h,t) -> f h, f t)
            NewList(ht, t) |> makeValue r
        | NewRecord(exprs, ent, genArgs) ->
            NewRecord(List.map f exprs, ent, genArgs) |> makeValue r
        | NewAnonymousRecord(exprs, ent, genArgs, isStruct) ->
            NewAnonymousRecord(List.map f exprs, ent, genArgs, isStruct) |> makeValue r
        | NewUnion(exprs, uci, ent, genArgs) ->
            NewUnion(List.map f exprs, uci, ent, genArgs) |> makeValue r
    | Test(e, kind, r) -> Test(f e, kind, r)
    | Lambda(arg, body, name) -> Lambda(arg, f body, name)
    | Delegate(args, body, name, tag) -> Delegate(args, f body, name, tag)
    | ObjectExpr(members, t, baseCall) ->
        let baseCall = Option.map f baseCall
        let members = members |> List.map (fun m -> { m with Body = f m.Body })
        ObjectExpr(members, t, baseCall)
    | CurriedApply(callee, args, t, r) ->
        CurriedApply(f callee, List.map f args, t, r)
    | Call(callee, info, t, r) ->
        let info = { info with ThisArg = Option.map f info.ThisArg
                               Args = List.map f info.Args }
        Call(f callee, info, t, r)
    | Emit(info, t, r) ->
        let callInfo =
            { info.CallInfo with ThisArg = Option.map f info.CallInfo.ThisArg
                                 Args = List.map f info.CallInfo.Args }
        Emit({ info with CallInfo = callInfo }, t, r)
    | Operation(kind, tags, t, r) ->
        match kind with
        | Unary(operator, operand) ->
            Operation(Unary(operator, f operand), tags, t, r)
        | Binary(op, left, right) ->
            Operation(Binary(op, f left, f right), tags, t, r)
        | Logical(op, left, right) ->
            Operation(Logical(op, f left, f right), tags, t, r)
    | Get(e, kind, t, r) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleIndex _ | UnionTag
        | UnionField _ | FieldGet _ -> Get(f e, kind, t, r)
        | ExprGet e2 -> Get(f e, ExprGet(f e2), t, r)
    | Sequential exprs -> Sequential(List.map f exprs)
    | Let(ident, value, body) -> Let(ident, f value, f body)
    | LetRec(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, f e)
        LetRec(bs, f body)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(f cond, f thenExpr, f elseExpr, r)
    | Set(e, kind, t, v, r) ->
        match kind with
        | ExprSet e2 -> Set(f e, ExprSet(f e2), t, f v, r)
        | FieldSet _ | ValueSet -> Set(f e, kind, t, f v, r)
    | WhileLoop(e1, e2, r) -> WhileLoop(f e1, f e2, r)
    | ForLoop(i, e1, e2, e3, up, r) -> ForLoop(i, f e1, f e2, f e3, up, r)
    | TryCatch(body, catch, finalizer, r) ->
        TryCatch(f body,
                    Option.map (fun (i, e) -> i, f e) catch,
                    Option.map f finalizer, r)
    | DecisionTree(expr, targets) ->
        let targets = targets |> List.map (fun (idents, v) -> idents, f v)
        DecisionTree(f expr, targets)
    | DecisionTreeSuccess(idx, boundValues, t) ->
        DecisionTreeSuccess(idx, List.map f boundValues, t)

let rec visitFromInsideOut f e =
    visit (visitFromInsideOut f) e |> f

let rec visitFromOutsideIn (f: Expr->Expr option) e =
    match f e with
    | Some e -> e
    | None -> visit (visitFromOutsideIn f) e
