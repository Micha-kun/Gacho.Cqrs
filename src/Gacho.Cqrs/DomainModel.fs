namespace Gacho.Cqrs

type AggregateRoot<'state, 'command, 'event when 'command :> ICommand and 'event :> IEvent> =
    { zero: 'state
      apply: 'state -> 'event -> 'state
      execute: 'state -> 'command -> Validation<'event list, string> }