module NBB.Invoices.FSharp.Data.DataAccess

open NBB.Invoices.FSharp.Domain
open Microsoft.Extensions.DependencyInjection
open NBB.Core.Effects

let addServices (services: IServiceCollection) = 
    services
        //.AddSideEffectHandler(InvoiceRepositoryImpl.handle<InvoiceAggregate.Invoice>)
        .AddSideEffectHandler(InvoiceRepositoryImpl.handle<unit>)