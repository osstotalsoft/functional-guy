
module NBB.Invoices.FSharp.Data.DataAccess
open NBB.Core.Effects
open Microsoft.Extensions.DependencyInjection
open NBB.Invoices.FSharp.Domain


let addServices (connectionString:string) (services:IServiceCollection) = 
    services
        .AddSideEffectHandler(InvoiceRepoImpl.handle<InvoiceAggregate.Invoice> connectionString)
        .AddSideEffectHandler(InvoiceRepoImpl.handle<unit> connectionString)