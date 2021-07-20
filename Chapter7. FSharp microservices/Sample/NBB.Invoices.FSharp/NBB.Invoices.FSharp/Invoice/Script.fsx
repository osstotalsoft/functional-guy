#r "nuget: NBB.Application.Mediator.FSharp"
#r "nuget: NBB.Core.Effects.FSharp"
#r "nuget: NBB.Core.Evented.FSharp"

#load "Domain.fs"
#load "Application.fs"

open NBB.Invoices.FSharp.Invoice
open System

open CreateInvoice
let createInvoiceCmd = {
    clientId = Guid.NewGuid()
    contractId = Guid.NewGuid()
    amount = 100m
}

let createInvoiceEffect = handle createInvoiceCmd
printfn "%A" createInvoiceEffect

open MarkInvoiceAsPayed
let markInvoiceAsPayedCmd = {
    invoiceId = Guid.NewGuid()
    paymentId = Guid.NewGuid()
}

let markInvoiceAsPayedEff = handle markInvoiceAsPayedCmd
printfn "%A" markInvoiceAsPayedEff