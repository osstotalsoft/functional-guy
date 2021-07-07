namespace NBB.Invoices.FSharp.Application

open System
open NBB.Core.Effects.FSharp
open NBB.Invoices.FSharp.Domain.InvoiceAggregate
open NBB.Core.Evented.FSharp
open NBB.Invoices.FSharp.Domain
open NBB.Application.Mediator.FSharp

module CreateInvoice =
    type Command =
        { clientId: Guid
          contractId: Guid
          amount: decimal }
        interface ICommand

    let validate (command: Command) =
        effect {
            if command.amount = 0m then
                failwith "Empty amount" |> ignore
                return None
            else
                return Some command
        }

    let handle (command: Command) : Effect<unit option> =
        effect {
            let invoice =
                create command.clientId (Some command.contractId) command.amount

            do!
                invoice
                |> Evented.run
                |> fst
                |> InvoiceRepository.save

            do! invoice |> Evented.exec |> Mediator.dispatchEvents

            return Some()
        }


module MarkInvoiceAsPayed =
    type Command =
        { invoiceId: Guid
          paymentId: Guid }
        interface ICommand

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
