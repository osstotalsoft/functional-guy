namespace NBB.Invoices.FSharp.Data

open NBB.Invoices.FSharp.Domain
open InvoiceRepository
open InvoiceAggregate
open System

module InvoiceRepositoryImpl =
    let handle<'a> (sideEffect: InvoiceRepository.SideEffect<'a>) : 'a =
        match sideEffect with
        | GetById (invoiceId, cont) ->
            let invoice =
                { Id = Guid.NewGuid()
                  ClientId = Guid.NewGuid()
                  ContractId = Guid.NewGuid() |> Some
                  Amount = 100m
                  PaymentId = None }

            printfn $"InvoiceRepositoryImpl.GetById {invoiceId} => {invoice}"

            invoice |> cont
        | Save (invoice, cont) ->
            printfn $"InvoiceRepositoryImpl.Save {invoice}"
            cont ()
