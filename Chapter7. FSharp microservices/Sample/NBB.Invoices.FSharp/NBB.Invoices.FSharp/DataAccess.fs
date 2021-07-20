module NBB.Invoices.FSharp.DataAccess

open NBB.Invoices.FSharp.Invoice
open Microsoft.Extensions.DependencyInjection
open NBB.Core.Effects

let addServices (services: IServiceCollection) = 
    services
        .AddSideEffectHandler(InvoiceRepositoryImpl.handle<InvoiceAggregate.Invoice>)
        .AddSideEffectHandler(InvoiceRepositoryImpl.handle<unit>)