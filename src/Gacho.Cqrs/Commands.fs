namespace Gacho.Cqrs

open System

type ICommand =
    abstract AggregateId: Guid