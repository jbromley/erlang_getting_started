%%% mess_client.erl

%%% The client process which runs on each user node

-module(mess_client).
-export([client/2]).
-include("mess_interface.hrl").

client(ServerNode, Name) ->
    {messenger, ServerNode} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(ServerNode).

client(ServerNode) ->
    receive
	logoff ->
	    exit(normal);
	#message_to{to_name=ToName, message=Message} ->
	    {messenger, ServerNode} !
		#message{client_pid=self(), to_name=ToName, message=Message},
	    await_result();
	{message_from, FromName, Message} ->
	    io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(ServerNode).

%%% Wait for a response from the server.
await_result() ->
    receive
	#abort_client{message=Why} ->
	    io:format("~p~n", [Why]),
	    exit(normal);
	#server_reply{message=What} ->
	    io:format("~p~n", [What])
    after 5000 ->
	    io:format("No response from server.~n", []),
	    exit(timeout)
    end.
