namespace Gacho.Cqrs

open System

type IIdentityId = 
    abstract member Value : Guid with get 

[<AbstractClass>]
type IdentityId<'Id when 'Id :> IdentityId<'Id>>(value) =
    member __.Value = value

    static member New() : 'Id =
        let id = Guid.NewGuid()
        Activator.CreateInstance(typeof<'Id>, [| id |]) :?> _

    interface IIdentityId with
        member self.Value = self.Value

    override self.GetHashCode() =
        hash self.Value

    override self.Equals(other) =
        match other with
        | :? 'Id as o -> (self.Value) = (o.Value)
        | _ -> false