-module(client).
-export([start/0, sendOutput/2, receiveInput/1, parseInput/3]).

start() ->
	sendOutput(none,none).

receiveInput(Pids) ->
	receive
		{getPids,From} ->
			From ! Pids,
			receiveInput(Pids);
		{kill,User} ->
			unregister(User),
			exit(normal);
		{isOnline,Server,User,Ret} ->
			Server ! {isOnline,{User,node()}},
			receive
				Val ->
					Ret ! Val
			end,
			receiveInput(Pids);
		{pids,NewPids} ->
			receiveInput(NewPids);
		{removePid,User} ->
			receiveInput(lists:delete(User,Pids));
		{list, Msg} ->
			io:format("~p~n",[Msg]),
			receiveInput(Pids);
		{User, Msg} ->
			io:format("~s: ~s~n",[User,Msg]),
			receiveInput(Pids);
		Msg ->
			io:format("Error received: ~p~n",[Msg]),
			receiveInput(Pids)
	end.
sendOutput(User,Server) ->
	Message = io:get_line("client> "),
	{NUser,NServer} = parseInput(Message,User,Server),
	sendOutput(NUser,NServer).

parseInput("quit\n",User,Server)->
	parseInput(quitOffline,User,Server),
	exit(normal);
parseInput("goOnline(" ++ Message,User,Server)->
	NotRegistered = (User == none) and (whereis(User) == undefined),
	case NotRegistered of
		true -> 
				Args=parseArgs(Message),
		 		NewServer = {list_to_atom(lists:nth(1,Args)),list_to_atom(lists:nth(2,Args))},
				NewUser = list_to_atom(lists:nth(3,Args)),
				case rpc:call(element(2,NewServer), erlang, whereis,[element(1,NewServer)]) of
					undefined ->
						io:format("client:Server ~p not found~n",[NewServer]),
						{User,Server};
					_ ->
						PID = spawn(client, receiveInput, [[]]),
						register(NewUser, PID),
						NewUser ! {isOnline,NewServer,NewUser,self()},
						receive
							true ->
								io:format("client: User with that name is already online~n",[]),
								NewUser ! {kill,NewUser},
								{none,none};
							false ->
								Msg = {onlineRequest,NewUser,{NewUser,node()}},
								NewServer ! Msg,
								{NewUser,NewServer}
						end
				end;
		false ->
			io:format("client: User already registered~n",[]),
			{User,Server}
	end;
parseInput(quitOffline,_,none)->
        {none,none};
parseInput(quitOffline,User,Server)->
	parseInput("chatQuit()\n",User,Server),
        Server ! {offlineRequest,User},
        User ! {kill,User},
        {none,none};
parseInput(_,none,none)->
	io:format("client: You are not connected to a server~n",[]),
	{none,none};
parseInput("goOffline()\n",User,Server)->
	parseInput(quitOffline,User,Server);
parseInput("ping(" ++ Message,User,Server)->
	Args = parseArgs(Message),
	Server ! {ping,User,lists:nth(1,Args)},
	{User,Server};
parseInput("chatRequest(" ++ Message,User,Server)->
	Args = parseArgs(Message),
	Server ! {chatRequest,list_to_atom(lists:nth(1,Args)),list_to_atom(lists:nth(2,Args))},
	{User,Server};
parseInput("chatAccept(" ++ Message,User,Server)->
        Args = parseArgs(Message),
        Server ! {chatAccept,list_to_atom(lists:nth(1,Args)),list_to_atom(lists:nth(2,Args))},
        {User,Server};
parseInput("chatReject(" ++ Message,User,Server)->
        Args = parseArgs(Message),
        Server ! {chatReject,list_to_atom(lists:nth(1,Args)),list_to_atom(lists:nth(2,Args))},
        {User,Server};
parseInput("sendMsg(" ++ Message,User,Server)->
	parseInput("sendMessage(" ++ Message,User,Server),
        {User,Server};
parseInput("sendMessage(" ++ Message,User,Server)->
        Args = parseArgs(Message),
	User ! {getPids,self()},
	receive
	     Pids ->
 		sendMessage(User,Pids,lists:nth(1,Args))
	end,
        {User,Server};
parseInput("users\n",User,Server)->
	User ! {getPids,self()},
        	receive
            		 Pids ->
				User ! {list,Pids}
        	end,
        {User,Server};
parseInput("online\n",User,Server)->
        Server ! {online,User},
        {User,Server};
parseInput("chatQuit()\n",User,Server)->
	User ! {getPids,self()},
        receive
             Pids ->
                removePid(User,Pids)
        end,
	User ! {pids,[]},
	Server ! {chatQuit,User},
	{User,Server};
parseInput(Message,User,Server)->
	io:format("client: Error unrecognized command:~p~n",[Message]),
	{User,Server}.

parseArgs(Args)->
	RawArgs = string:substr(Args,1,string:len(Args)-2),
	NewArgs = string:tokens(RawArgs , ","),
	NewArgs.

sendMessage(_,[],_) -> true;
sendMessage(User,Pids,Msg) ->
	[H|T] = Pids,
	case element(1,H) == User of
		true ->
			ok;
		false ->
			H ! {User,Msg}
	end,
	sendMessage(User,T,Msg).

removePid(_,[]) -> true;
removePid(User,Pids) ->
        [H|T] = Pids,
        H ! {removePid,{User,node()}},
        removePid(User,T).
