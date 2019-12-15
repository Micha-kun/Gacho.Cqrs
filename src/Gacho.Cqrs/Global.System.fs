namespace global

open System

[<RequireQualifiedAccess>]
module Enum =
    let isDefined<'a, 'b when 'a: enum<'b>> (value: 'a) =
        let (!<) =
            box
            >> unbox
            >> uint64

        let typ = typeof<'a>
        if typ.IsDefined(typeof<FlagsAttribute>, false) then
            ((!<value, Enum.GetValues(typ) |> unbox)
             ||> Array.fold (fun n v -> n &&& ~~~(!<v)) = 0UL)
        else Enum.IsDefined(typ, value)

[<RequireQualifiedAccess>]
module Option =
    let inline ofNullRecord r =
        match Object.ReferenceEquals(r, null) with
        | false -> Some r
        | true -> None

[<RequireQualifiedAccess>]
module Record =
    let inline isNull r = Object.ReferenceEquals(r, null)

    let inline toOption r =
        match isNull r with
        | true -> None
        | false -> Some r

    let inline toRecord errorData r =
        match isNull r with
        | false -> Ok r
        | true -> Error errorData

    let inline ofOption opt = opt |> Option.defaultValue Unchecked.defaultof<_>

[<RequireQualifiedAccess>]
module String =
    let trim (str: string) = str.Trim()
