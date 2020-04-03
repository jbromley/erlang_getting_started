%%% Message passing utility.
%%% User interface:
%%% login(Name)
%%%     One user at a time can log in from each Erlang node in the
%%%     system messenger: and choose a suitable Name. If the Name
%%%     is already logged in at another node or if someone else is
%%%     already logged in at the same node, login will be rejected
%%%     with a suitable error message.
%%% logoff()
%%%     Logs off anybody at that node
%%% message(ToName, Message)
%%%     sends Message to ToName. Error messages if the user of this
%%%     function is not logged on or if ToName is not logged on at
%%%     any node.
%%%
%%% One node in the network of Erlang nodes runs a server which maintains
%%% data about the logged on users. The server is registered as "messenger"
%%% Each node where there is a user logged on runs a client process registered
%%% as "mess_client"
%%%
%%% Protocol between the client processes and the server
%%% ----------------------------------------------------
%%%
%%% To server: {ClientPid, logon, UserName}
%%% Reply {messenger, stop, user_exists_at_other_node} stops the client
%%% Reply {messenger, logged_on} logon was successful
%%%
%%% When the client terminates for some reason
%%% To server: {'EXIT', ClientPid, Reason}
%%%
%%% To server: {ClientPid, message_to, ToName, Message} send a message
%%% Reply: {messenger, stop, you_are_not_logged_on} stops the client
%%% Reply: {messenger, receiver_not_found} no user with this name logged on
%%% Reply: {messenger, sent} Message has been sent (but no guarantee)
%%%
%%% To client: {message_from, Name, Message},
%%%
%%% Protocol between the "commands" and the client
%%% ----------------------------------------------
%%%
%%% Started: messenger:client(Server_Node, Name)
%%% To client: logoff
%%% To client: {message_to, ToName, Message}
%%%
%%% Configuration: change the server_node() function to return the
%%% name of the node where the messenger server runs

-module(messenger).
-export([start_server/0, server/0, logon/1, logoff/0, message/2, client/2]).

%%% Messenger server node name.
server_node() ->
    messenger@dromedary.

%%% Initial server function.
%%% Traps the exit signal and calls the main server loop function.
server() ->
    process_flag(trap_exit, true),
    server([]).

%%% Main messenger server message loop.
server(User_List) ->
    receive
	{From, logon, Name} ->
	    New_User_List = server_logon(From, Name, User_List),
	    server(New_User_List);
	{'EXIT', From, _} ->
	    New_User_List = server_logoff(From, User_List),
	    server(New_User_List);
	{From, message_to, To, Message} ->
	    server_transfer(From, To, Message, User_List),
	    io:format("list is now: ~p~n", [User_List]),
	    server(User_List);
    end.

%%%  Start the server
start_server() ->
    register(messenger, spawn(messenger, server, [])).

%%% Server adds a new user to the user list.
server_logon(From, Name, User_List) ->
    %% Check if logged on anywhere else.
    case lists:keymember(Name, 2, User_List) of
	true ->
	    From ! {messenger, stop, user_exists_at_other_node},
	    User_List;
	false ->
	    From ! {messenger, logged_on},
	    link(From),
	    [{From, Name} | User_List]
    end.

%%% Server deletes a user from the user list.
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Transfer a message between users.
server_transfer(From, To, Message, User_List) ->
    %% Check that the user is logged on an who he is.
    case lists:keysearch(From , 1, User_List) of
	false ->
	    From ! {messenger, stop, you_are_not_logged_in};
	{value, {_, Name}} ->
	    server_transfer(From, Name, To, Message, User_List)
    end.

%%% If the user exists, the transfer the message.
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message.
    case lists:keysearch(To, 2, User_List) of
	false ->
	    From ! {messenger, receiver_not_found};
	{value, {To_Pid, To}} ->
	    To_Pid ! {message_from, Name, Message},
	    From ! {messenger, sent}
    end.


%%% User commands
logon(Name) ->
    case whereis(mess_client) of
	undefined ->
	    register(mess_client, spawn(messenger, client, [server_node(), Name]));
	_ ->
	    already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(To_Name, Message) ->
    case whereis(mess_client) of
	undefined ->
	    not_logged_on;
	_ ->
	    mess_client ! {message_to, To_Name, Message},
	    ok
    end.

%%% Client process which runs on each user node.
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
	logoff ->
	    exit(normal);
	{message_to, To_Name, Message} ->
	    {messenger, Server_Node} ! {self(), message_to, To_Name, Message},
	    await_result();
	{message_from, From_Name, Message} ->
	    io:format("Message from ~p: ~p~n", [From_Name, Message])
    end,
    client(Server_Node).

%%% Wait for a response from the server.
await_result() ->
    receive
	{messenger, stop, Why} ->
	    io:format("~p~n", [Why]),
	    exit(normal);
	{messenger, What} ->
	    io:format("~p~n", [What])
    after 5000 ->
	    io:format("No response from server~n", []),
	    exit(timeout)
    end.
