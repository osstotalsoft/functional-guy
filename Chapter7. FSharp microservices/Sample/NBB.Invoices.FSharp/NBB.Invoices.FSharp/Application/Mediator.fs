namespace NBB.Invoices.FSharp.Application

open NBB.Core.Effects.FSharp
open NBB.Application.Mediator.FSharp

module Mediator = 
    let dispatchEvent (ev:#IEvent) = 
        effect{
            return ()
        }

    let dispatchEvents evs = evs |> List.traverse_ dispatchEvent
       