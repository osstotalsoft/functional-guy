namespace NBB.Invoices.FSharp.Application
open NBB.Core.Effects
open NBB.Core.Effects.FSharp
open NBB.Application.Mediator.FSharp
open System
open Microsoft.Extensions.DependencyInjection
open NBB.Messaging.Effects

[<AutoOpen>]
module Middlewares =
    let logRequest =
        fun next req ->
            effect {
                Console.WriteLine "before"

                let! result = next req
                Console.WriteLine "after"
                return result
            }

    let handleExceptions =
        fun next req ->
            effect {
                Console.WriteLine "try"

                let! result = next req
                Console.WriteLine "finally"
                return result
            }

    let publishMessage =
        fun _ req ->
            effect {
                do! MessageBus.Publish req |> Effect.ignore
                return Some()
            }

[<AutoOpen>]
module PipelineUtils =
    let terminateRequest<'a> : (Effect<'a option> -> Effect<'a>) =
        Effect.map
            (function
            | Some value -> value
            | None -> failwith "No handler found")

    let terminateEvent : (Effect<unit option> -> Effect<unit>) = Effect.map ignore


module WriteApplication =
    open RequestMiddleware
    open CommandMiddleware
    open CommandHandler

    let private commandPipeline : CommandMiddleware =
        handleExceptions
        << logRequest
        << handlers [ CreateInvoice.handle |> upCast
                      MarkInvoiceAsPayed.handle |> upCast ]

    let private queryPipeline : QueryMiddleware =
        handleExceptions << logRequest << handlers []

    open EventMiddleware

    let private eventPipeline : EventMiddleware =
        handleExceptions << logRequest << handlers []

    let addServices (services: IServiceCollection) =
        let sendCommand (cmd: 'TCommand) =
            CommandMiddleware.run commandPipeline cmd
            |> terminateRequest

        let publishEvent (ev: 'TEvent) =
            EventMiddleware.run eventPipeline ev
            |> terminateEvent

        let sendQuery (q: IQuery) =
            RequestHandler.empty q |> terminateRequest

        let mediator =
            { SendCommand = sendCommand
              SendQuery = sendQuery
              DispatchEvent = publishEvent }


        services.AddEffects() |> ignore
        services.AddMessagingEffects() |> ignore

        services.AddSideEffectHandler(Mediator.handleGetMediator mediator)
