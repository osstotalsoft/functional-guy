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
                printfn $"Before: {req.GetType().FullName}"
                let! result = next req
                printfn $"After: {req.GetType().FullName}"
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

[<AutoOpen>]
module MediatorUtils =
    let addMediator commandPipeline queryPipeline eventPipeline (services: IServiceCollection) =
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


        services.AddSideEffectHandler(Mediator.handleGetMediator mediator)

module WriteApplication =
    open RequestMiddleware
    open CommandHandler
    open RequestHandler

    let private commandPipeline : CommandMiddleware =
        logRequest
        << handlers [ CreateInvoice.validate >=> CreateInvoice.handle |> upCast
                      MarkInvoiceAsPayed.handle |> upCast ]

    let private queryPipeline : QueryMiddleware = logRequest << handlers []

    open EventMiddleware

    let private eventPipeline : EventMiddleware = logRequest << handlers []

    let addServices (services: IServiceCollection) =

        services.AddEffects() |> ignore
        services.AddMessagingEffects() |> ignore
        addMediator commandPipeline queryPipeline eventPipeline services

module ReadApplication =
    open RequestMiddleware

    let private commandPipeline : CommandMiddleware = logRequest << publishMessage

    let private queryPipeline : QueryMiddleware = logRequest << handlers []

    open EventMiddleware

    let private eventPipeline : EventMiddleware = logRequest << handlers []

    let addServices (services: IServiceCollection) =

        services.AddEffects() |> ignore
        services.AddMessagingEffects() |> ignore
        addMediator commandPipeline queryPipeline eventPipeline services
