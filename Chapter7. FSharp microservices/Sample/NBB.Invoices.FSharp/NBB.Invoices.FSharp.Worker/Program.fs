namespace NBB.Invoices.FSharp.Worker

open System
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open NBB.Messaging.Abstractions
open NBB.Messaging.Nats
open NBB.Messaging.Host
open NBB.Messaging.Host.MessagingPipeline
open NBB.Core.Effects
open NBB.Application.Mediator.FSharp
open NBB.Invoices.FSharp
open NBB.Invoices.FSharp.Invoice

module Program =

    // App configuration

    let configureServices (context: HostBuilderContext) services = 
        let connectionString = context.Configuration.GetConnectionString("DefaultConnection")
        WriteApplication.addServices services |> ignore
        DataAccess.addServices connectionString services |> ignore

        services
            .AddMessageBus()
            .AddNatsTransport(context.Configuration)
        |> ignore

        services.AddMessagingHost
            (fun hostBuilder ->
                hostBuilder.Configure
                    (fun configBuilder ->
                        configBuilder
                            .AddSubscriberServices(fun config ->
                                config.AddTypes(
                                    typeof<CreateInvoice.Command>,
                                    typeof<MarkInvoiceAsPayed.Command>
                                )
                                |> ignore)
                            .WithDefaultOptions()
                            .UsePipeline(fun pipelineBuilder ->
                                pipelineBuilder
                                    .UseCorrelationMiddleware()
                                    .UseExceptionHandlingMiddleware()
                                    .UseDefaultResiliencyMiddleware()
                                    .UseEffectMiddleware(fun m ->
                                        m
                                        |> Mediator.sendMessage
                                        |> EffectExtensions.ToUnit)
                                |> ignore)
                        |> ignore)
                |> ignore)
        |> ignore

    let loggingConfig (context: HostBuilderContext) (loggingBuilder: ILoggingBuilder) =
        loggingBuilder.AddConsole().AddDebug() |> ignore

    let createHostBuilder args =
        Host
            .CreateDefaultBuilder(args)
            .ConfigureServices(configureServices)
            .ConfigureLogging(Action<HostBuilderContext, ILoggingBuilder> loggingConfig)


    [<EntryPoint>]
    let main args =
        createHostBuilder(args).Build().Run()

        0 // exit code
