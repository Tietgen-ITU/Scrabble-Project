module internal Mailbox

open MoveUtil

type MessageRequest<'a> =
    | SetValue of 'a
    | RequestValue of AsyncReplyChannel<'a>

let createMoveMailbox st =
    MailboxProcessor.Start (fun inbox ->
        let rec loop word =
            async {
                let! msg = inbox.Receive()

                match msg with
                | SetValue newWord -> return! loop (getBestMove st word newWord)
                | RequestValue replyCh ->
                    replyCh.Reply(snd word)
                    return! loop word
            }

        loop (0, []))
