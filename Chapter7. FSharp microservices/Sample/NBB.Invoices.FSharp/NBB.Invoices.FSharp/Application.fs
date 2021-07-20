namespace NBB.Invoices.FSharp

open NBB.Core.Effects
open NBB.Core.Effects.FSharp
open NBB.Application.Mediator.FSharp
open Microsoft.Extensions.DependencyInjection
open NBB.Messaging.Effects
open NBB.Invoices.FSharp.Invoice

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
        services.AddMediator(commandPipeline, queryPipeline, eventPipeline)

module ReadApplication =
    open RequestMiddleware

    let private commandPipeline : CommandMiddleware = logRequest << publishMessage

    let private queryPipeline : QueryMiddleware = logRequest << handlers []

    open EventMiddleware

    let private eventPipeline : EventMiddleware = logRequest << handlers []

    let addServices (services: IServiceCollection) =
        services.AddEffects() |> ignore
        services.AddMessagingEffects() |> ignore
        services.AddMediator(commandPipeline, queryPipeline, eventPipeline)
