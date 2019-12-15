namespace global

[<RequireQualifiedAccess>]
module Result =
    let bimap onSuccess onFailure xr =
        match xr with
        | Ok v -> onSuccess v
        | Error e -> onFailure e
    let map =
        Result.map
    let mapError =
        Result.mapError
    let bind =
        Result.bind
    let iter (f: _ -> unit) xr =
        map f xr |> ignore
        xr
    let iterError (f: _ -> unit) xr =
        mapError f xr |> ignore
        xr

    let apply fr xr =
        match fr, xr with
        | Ok f, Ok x -> Ok(f x)
        | Error e, _
        | Ok _, Error e -> Error e

    let sequence resultList =
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
        List.foldBack consR resultList initialValue

    let map2 f xr yr =
        let (<!>) = map
        let (<*>) = apply
        f <!> xr <*> yr

    let map3 f xr yr zr =
        let (<!>) = map
        let (<*>) = apply
        f <!> xr <*> yr <*> zr

    let map4 f xr yr zr ar =
        let (<!>) = map
        let (<*>) = apply
        f <!> xr <*> yr <*> zr <*> ar

    let bind2 f xr yr =
        map2 f xr yr |> bind id
    let bind3 f xr yr zr =
        map3 f xr yr zr |> bind id
    let bind4 f xr yr zr ar =
        map4 f xr yr zr ar |> bind id

    let isOk xr =
        match xr with
        | Ok _ -> true
        | Error _ -> false

    let isError xr =
        isOk xr |> not

    let filter pred =
        function
        | Ok x -> pred x
        | Error _ -> true

    let bindOption f xOpt =
        match xOpt with
        | Some x ->
            f x |> map Some
        | None ->
            Ok None

    let mapOption f xOpt =
        bindOption (f >> Ok) xOpt

    let ofOption errorValue xOpt =
        match xOpt with
        | Some x -> Ok x
        | None -> Error errorValue

    let toOption xr =
        match xr with
        | Ok x -> Some x
        | Error _ -> None

    let toErrorOption xr =
        match xr with
        | Ok _ -> None
        | Error e -> Some e

    let defaultValue value xr =
        xr
        |> toOption
        |> Option.defaultValue value

    let defaultWith valueFun xr =
        xr
        |> toOption
        |> Option.defaultWith valueFun

    let defaultError value xr =
        xr
        |> toErrorOption
        |> Option.defaultValue value

    let defaultErrorWith valueFun xr =
        xr
        |> toErrorOption
        |> Option.defaultWith valueFun

[<AutoOpen>]
module ResultComputationException =
    type ResultBuilder() =
        member __.Return(x) =
            Ok x
        member __.Bind(x, f) =
            Result.bind f x
        member __.ReturnFrom(x) =
            x
        member this.Zero() =
            this.Return()
        member __.Delay(f) =
            f
        member __.Run(f) =
            f()
        member this.While(guard, body) =
            if not (guard()) then
                this.Zero()
            else
                this.Bind(body(), fun () -> this.While(guard, body))
        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body())
            with e ->
                handler e
        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body())
            finally
                compensation()

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally (body',
                            fun () ->
                                match disposable with
                                | null -> ()
                                | disp -> disp.Dispose())

        member this.For(sequence: seq<_>, body) =
            this.Using
                (sequence.GetEnumerator(),
                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
        member this.Combine(a, b) =
            this.Bind(a, fun () -> b())

    let result =
        ResultBuilder()

type Validation<'Success, 'Failure> =
    Result<'Success, 'Failure list>

[<RequireQualifiedAccess>]
module Validation =
    let apply (fV: Validation<_, _>) (xV: Validation<_, _>): Validation<_, _> =
        match fV, xV with
        | Ok f, Ok x -> f x |> Ok
        | Error e, Ok _
        | Ok _, Error e -> Error e
        | Error e1, Error e2 -> Error(e1 @ e2)
    let sequence (validationList: Validation<_, _> list) =
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
        List.foldBack consR validationList initialValue
    let ofResult r: Validation<_, _> =
        r |> Result.mapError List.singleton
    let toResult (v: Validation<_, _>): Result<_, _> =
        v

[<RequireQualifiedAccess>]
module Async =
    let map f (xA: Async<_>) =
        async {
            let! x = xA
            return f x
        }
    let retn x =
        async.Return x
    let apply fA (xA: Async<_>) =
        async {
            let! fChild = fA |> Async.StartChild
            let! x = xA
            let! f = fChild
            return f x
        }
    let bind f xA =
        async.Bind(xA, f)

type AsyncResult<'Success, 'Failure> =
    Async<Result<'Success, 'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =
    let map f (xAR: AsyncResult<_, _>): AsyncResult<_, _> =
        xAR |> Async.map (Result.map f)
    let mapError f (xAR: AsyncResult<_, _>): AsyncResult<_, _> =
        xAR |> Async.map (Result.mapError f)
    let ignore x =
        x |> map ignore
    let retn x: AsyncResult<_, _> =
        x
        |> Ok
        |> Async.retn
    let catch f (x: AsyncResult<_, _>): AsyncResult<_, _> =
        x
        |> Async.Catch
        |> Async.map (function
               | Choice1Of2(Ok x) -> Ok x
               | Choice1Of2(Error e) -> Error e
               | Choice2Of2(e) -> f e |> Error)

    let applyM (fAsyncResult: AsyncResult<_, _>) (xAsyncResult: AsyncResult<_, _>): AsyncResult<_, _> =
        async {
            let! fResultChild = fAsyncResult |> Async.StartChild
            let! xResult = xAsyncResult
            let! fResult = fResultChild
            return Result.apply fResult xResult
        }
    let applyA (fAsyncResult: AsyncResult<_, _>) (xAsyncResult: AsyncResult<_, _>): AsyncResult<_, _> =
        async {
            let! fResultChild = fAsyncResult |> Async.StartChild
            let! xResult = xAsyncResult
            let! fResult = fResultChild
            return Validation.apply fResult xResult
        }
    let bind (f: 'a -> AsyncResult<'b, 'c>) (xAsyncResult: AsyncResult<_, _>): AsyncResult<_, _> =
        async {
            let! xResult = xAsyncResult
            return! match xResult with
                    | Ok x -> f x
                    | Error e ->
                        e
                        |> Error
                        |> Async.retn
        }
    let sequenceM resultList =
        let (<*>) = applyM
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn []
        List.foldBack consR resultList initialValue
    let sequenceA resultList =
        let (<*>) = applyA
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn []
        List.foldBack consR resultList initialValue
    let ofSuccess x: AsyncResult<_, _> =
        retn x
    let ofError e: AsyncResult<_, _> =
        e
        |> Error
        |> Async.retn
    let ofResult r: AsyncResult<_, _> =
        r |> Async.retn
    let ofAsync x: AsyncResult<_, _> =
        x |> Async.map (Ok)
    let sleep ms =
        Async.Sleep ms |> ofAsync
    let defaultValue value (xr: AsyncResult<_, _>) =
        xr |> Async.map (Result.defaultValue value)
    let defaultWith valueFun (xr: AsyncResult<_, _>) =
        xr |> Async.map (Result.defaultWith valueFun)
    let defaultError value (xr: AsyncResult<_, _>) =
        xr |> Async.map (Result.defaultError value)
    let defaultErrorWith valueFun (xr: AsyncResult<_, _>) =
        xr |> Async.map (Result.defaultErrorWith valueFun)

[<AutoOpen>]
module AsyncResultComputationExpression =
    type AsyncResultBuilder() =
        member __.Return(x) =
            x |> AsyncResult.retn
        member __.Bind(x, f) =
            AsyncResult.bind f x
        member __.ReturnFrom(x) =
            x
        member this.Zero() =
            this.Return()
        member __.Delay(f) =
            f
        member __.Run(f) =
            f()
        member this.While(guard, body) =
            if not (guard()) then
                this.Zero()
            else
                this.Bind(body(), fun () -> this.While(guard, body))
        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body())
            with e ->
                handler e
        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body())
            finally
                compensation()
        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally (body',
                            fun () ->
                                match disposable with
                                | null -> ()
                                | disp -> disp.Dispose())
        member this.For(sequence: seq<_>, body) =
            this.Using
                (sequence.GetEnumerator(),
                 fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
        member this.Combine(a, b) =
            this.Bind(a, fun () -> b())

    let asyncResult =
        AsyncResultBuilder()