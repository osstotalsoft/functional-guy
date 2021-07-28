namespace NBB.Invoices.FSharp.Invoice

open InvoiceAggregate
open System
open System.Data.SqlClient
open FSharp.Control.Tasks
open System.Threading.Tasks

module InvoiceRepositoryImpl =
    let unboxOption<'a> (o: obj) : 'a option =
        if (isNull o) || DBNull.Value.Equals o then
            None
        else
            Some(unbox o)

    let boxOption<'a> (x: 'a option) =
        match x with
        | Some value -> box value
        | None -> box DBNull.Value

    let getById connectionString invoiceId cancellationToken =
        task {
            use conn = new SqlConnection(connectionString)
            do! conn.OpenAsync(cancellationToken)

            let query =
                "SELECT InvoiceId, ClientId, ContractId, Amount, PaymentId FROM FS_Invoices WHERE InvoiceId = @InvoiceId"

            use cmd = new SqlCommand(query, conn)

            cmd.Parameters.AddWithValue("@InvoiceId", invoiceId)
            |> ignore

            let! r = cmd.ExecuteReaderAsync(cancellationToken)

            let results =
                [ while r.Read() do
                      yield
                          { Id = unbox r.[0]
                            ClientId = unbox r.[1]
                            ContractId = unboxOption r.[2]
                            Amount = unbox r.[3]
                            PaymentId = unboxOption r.[4] } ]

            return
                if results.IsEmpty then
                    None
                else
                    Some results.Head
        }


    let save connectionString (invoice: Invoice) cancellationToken =
        task {
            use conn = new SqlConnection(connectionString)
            do! conn.OpenAsync(cancellationToken)

            let query =
                @"IF NOT EXISTS(SELECT 1 FROM FS_Invoices WHERE InvoiceId = @InvoiceId)
    BEGIN
    	INSERT INTO FS_Invoices(InvoiceId, ClientId, ContractId, Amount, PaymentId)
    	SELECT @InvoiceId, @ClientId, @ContractId, @Amount, @PaymentId
    END
    ELSE
    BEGIN
    	UPDATE FS_Invoices
    	SET ClientId = @ClientId, ContractId = @ContractId, Amount = @Amount, PaymentId = @PaymentId
    	WHERE InvoiceId = @InvoiceId
    END"

            use cmd = new SqlCommand(query, conn)

            cmd.Parameters.AddWithValue("@InvoiceId", invoice.Id)
            |> ignore

            cmd.Parameters.AddWithValue("@ClientId", invoice.ClientId)
            |> ignore

            cmd.Parameters.AddWithValue("@ContractId", invoice.ContractId |> boxOption)
            |> ignore

            cmd.Parameters.AddWithValue("@Amount", invoice.Amount)
            |> ignore

            cmd.Parameters.AddWithValue("@PaymentId", invoice.PaymentId |> boxOption)
            |> ignore

            let! _ = cmd.ExecuteNonQueryAsync(cancellationToken)
            return ()
        }


    let handle<'a> connectionString (sideEffect: InvoiceRepository.SideEffect<'a>) cancellationToken : Task<'a> =
        match sideEffect with
        | InvoiceRepository.GetById (invoiceId, cont) ->
            task {
                let! invoice = getById connectionString invoiceId cancellationToken
                printfn $"InvoiceRepositoryImpl.GetById {invoiceId} => {invoice}"
                return invoice |> cont
            }

        | InvoiceRepository.Save (invoice, cont) ->
            task {
                printfn $"InvoiceRepositoryImpl.Save {invoice}"
                do! save connectionString invoice cancellationToken
                return cont ()
            }
