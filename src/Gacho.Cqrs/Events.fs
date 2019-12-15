namespace Gacho.Cqrs

open System

type IEvent =
    abstract AggregateId: Guid