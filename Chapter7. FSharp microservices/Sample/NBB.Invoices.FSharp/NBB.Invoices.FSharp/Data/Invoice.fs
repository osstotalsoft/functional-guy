module NBB.Invoices.FSharp.Data.InvoiceRepoImpl

open System
open NBB.Invoices.FSharp.Domain
open InvoiceAggregate
open FSharp.Data

type SelectInvoicesCommand = SqlCommandProvider<"SELECT * FROM Invoices where InvoiceId = @invoiceId" , "name=InvoicesDb">
type InvoicesDb = SqlProgrammabilityProvider<"name=InvoicesDb">

let handle<'a> (connectionString:string) (sideEffect:InvoiceRepository.SideEffect<'a>) : 'a =
    match sideEffect with
    | InvoiceRepository.GetById (invoiceId, continuation) ->
        use cmd = new SqlCommandProvider<"
            SELECT * FROM Invoices Where InvoiceId = @invoiceId
            " , "name=InvoicesDb">(connectionString)
        
        cmd.Execute(invoiceId) 
        |> Seq.map
            (fun item ->
                { 
                    Id = item.InvoiceId
                    ClientId = item.ClientId
                    ContractId = item.ContractId
                    Amount = item.Amount
                    PaymentId = item.PaymentId
                }
            )
        |> Seq.head
        |> continuation

    | InvoiceRepository.Save (invoice, continuation) -> continuation ()


//let handle<'a> : InvoiceRepository.SideEffect<'a> -> 'a =
//    function
//    | InvoiceRepository.GetById (invoiceId, continuation) ->
//        { Id = invoiceId
//          ClientId = Guid.NewGuid()
//          ContractId = Guid.NewGuid() |> Some
//          Amount = 100m
//          PaymentId = None }
//        |> continuation
//    | InvoiceRepository.Save (invoice, continuation) -> continuation ()
