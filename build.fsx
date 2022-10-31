#r "nuget: Fable.PublishUtils, 2.4.0"

open PublishUtils

let args =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> List.ofArray

match args with
| IgnoreCase "publish"::_ ->
    pushNuget "src/Sutil.CompilerPlugins/Sutil.CompilerPlugins.fsproj" [] doNothing
    pushNuget "src/SutilComponent/SutilComponent.fsproj" [] doNothing
| _ -> ()
