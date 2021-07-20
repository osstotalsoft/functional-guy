module NBB.Invoices.FSharp.Api.HttpHandlers

open System
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks
open Giraffe
open NBB.Core.Effects
open NBB.Application.Mediator.FSharp
open NBB.Invoices.FSharp.Invoice


let asyncCommandResponse : HttpHandler =
    let response =
        {| CommandId = Guid.NewGuid()
           CorrelationId = Guid.NewGuid() |}

    json response

let sendCommand (cmd: 'a :> ICommand) (next: HttpFunc) (ctx: HttpContext) =
    task {
        let effect = Mediator.sendCommand cmd
        let interpreter = ctx.GetService<IInterpreter>()
        do! interpreter.Interpret(effect)

        return! next ctx
    }

let cmd<'a when 'a :> ICommand> =
    bindJson<'a> sendCommand >=> asyncCommandResponse


let invoiceHandler : HttpHandler =
    POST
    >=> choose [ route "/create" >=> cmd<CreateInvoice.Command>
                 route "/pay" >=> cmd<MarkInvoiceAsPayed.Command> ]
