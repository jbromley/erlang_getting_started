%%% mess_interface.hrl

%%% Message interface between client, server, and client shell for the
%%% messenger program.

%%% Messages from client to server received in server/1 function.
-record(logon, {client_pid, username}).
-record(message, {client_pid, to_name, message}).
%%% {'EXIT', Client_Pid, Reason} Client terminated or unreachable.

%%% Messages from server to client received in await_result/0 function.
-record(abort_client, {message}).
%%% Messages are: user_exists_at_other_node,
%%%               you_are_not_logged_on
-record(server_reply, {message}).
%%% Messages are: logged_on
%%%               receiver_not_found
%%%               sent (Message has been sent, no guaratee)
%%% Messages from server to client received in client/1 function.
-record(message_from, {from_name, message}).

%%% Messages from shell to client received in client/1 function.
%%% spawn(mess_client, client, [server_node(), Name])
-record(message_to, {to_name, message}).
%%% logoff
