namespace Gacho.Cqrs

type AggregateRoot<'state, 'command, 'event when 'command :> ICommand and 'event :> IEvent> =
    { zero: 'state
      apply: 'state -> 'event -> 'state
      exec: 'state -> 'command -> Result<'event list, string> }