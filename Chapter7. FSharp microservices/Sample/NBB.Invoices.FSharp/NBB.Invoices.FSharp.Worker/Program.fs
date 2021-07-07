namespace NBB.Invoices.FSharp.Worker

open System
open System.Collections.Generic
open System.Linq
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open NBB.Invoices.FSharp.Application
open NBB.Invoices.FSharp.Data
open NBB.Messaging.Abstractions
open NBB.Messaging.Nats
open NBB.Messaging.Host
open NBB.Invoices.FSharp.Application
open NBB.Messaging.Host.MessagingPipeline
open NBB.Core.Effects
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open System.Reflection
open System.IO

module Program =

    // App configuration
    let appConfig (argv: string array) (context: HostBuilderContext) (configApp: IConfigurationBuilder) =
        configApp
            .SetBasePath(Directory.GetCurrentDirectory())
            .AddJsonFile("appsettings.json", optional = true)
            .AddJsonFile(sprintf "appsettings.%s.json" context.HostingEnvironment.EnvironmentName, optional = true)
            .AddUserSecrets(Assembly.GetExecutingAssembly())
            .AddEnvironmentVariables()
            .AddCommandLine(argv)
        |> ignore

    let configureServices (context: HostBuilderContext) services = 
        WriteApplication.addServices services |> ignore
        DataAccess.addServices services |> ignore

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
            .ConfigureAppConfiguration(Action<HostBuilderContext, IConfigurationBuilder> (appConfig args))
            .ConfigureLogging(Action<HostBuilderContext, ILoggingBuilder> loggingConfig)
            .UseDefaultServiceProvider( fun options -> options.ValidateScopes <- false)


    [<EntryPoint>]
    let main args =
        createHostBuilder(args).Build().Run()

        0 // exit code
