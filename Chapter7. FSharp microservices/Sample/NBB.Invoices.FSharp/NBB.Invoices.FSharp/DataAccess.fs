module NBB.Invoices.FSharp.DataAccess

open NBB.Invoices.FSharp.Invoice
open Microsoft.Extensions.DependencyInjection
open NBB.Core.Effects

let addServices connectionString (services: IServiceCollection) = 
    services
        .AddSideEffectHandler(InvoiceRepositoryImpl.handle<InvoiceAggregate.Invoice option> connectionString)
        .AddSideEffectHandler(InvoiceRepositoryImpl.handle<unit> connectionString)