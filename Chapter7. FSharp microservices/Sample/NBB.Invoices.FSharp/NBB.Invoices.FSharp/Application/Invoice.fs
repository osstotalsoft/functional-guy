namespace NBB.Invoices.FSharp.Application

open System
open NBB.Core.Effects.FSharp
open NBB.Invoices.FSharp.Domain.InvoiceAggregate
open NBB.Core.Evented.FSharp
open NBB.Invoices.FSharp.Domain

module CreateInvoice =
    type Command =
        { clientId: Guid
          contractId: Guid
          amount: decimal }

    //let validate' (command: Command1) = 
    let handle (command: Command) : Effect<unit option> =
        effect {
            let invoice =
                create command.clientId (Some command.contractId) command.amount

            do!
                invoice
                |> Evented.run
                |> fst
                |> InvoiceRepository.save

            do!
                invoice
                |> Evented.exec
                |> Mediator.dispatchEvents

            return Some()
        }


module MarkInvoiceAsPayed = 
    type Command = { 
        invoiceId : Guid
        paymentId : Guid
    }
    
    let handle cmd = 
        effect {
            let! invoice = InvoiceRepository.getById cmd.invoiceId
            let invoice = markAsPayed cmd.paymentId invoice

            do!
                invoice
                |> Evented.run
                |> fst
                |> InvoiceRepository.save
            
            do!
                invoice
                |> Evented.exec
                |> List.traverse_ Mediator.dispatchEvent

            return Some()
        }