namespace NBB.Invoices.FSharp.Invoice

open InvoiceAggregate
open System

module InvoiceRepositoryImpl =
    let handle<'a> (sideEffect: InvoiceRepository.SideEffect<'a>) : 'a =
        match sideEffect with
        | InvoiceRepository.GetById (invoiceId, cont) ->
            let invoice =
                { Id = Guid.NewGuid()
                  ClientId = Guid.NewGuid()
                  ContractId = Guid.NewGuid() |> Some
                  Amount = 100m
                  PaymentId = None }

            printfn $"InvoiceRepositoryImpl.GetById {invoiceId} => {invoice}"

            invoice |> cont
        | InvoiceRepository.Save (invoice, cont) ->
            printfn $"InvoiceRepositoryImpl.Save {invoice}"
            cont ()
