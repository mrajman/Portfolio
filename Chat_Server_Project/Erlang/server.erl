-module(server).
-export([start/1,serverRun/2]).

start(Id) ->
    PID = spawn(server,serverRun,[#{}, []]),
    register(Id, PID),
    io:format("Server started at address: {~p:~p}~n" ,[Id,node()]).
    
serverRun(Map, RoomList) ->
    receive
	{isOnline,From} ->
		From ! isOnline(element(1,From),Map),
		serverRun(Map,RoomList);
        {onlineRequest,User,ID} ->
            case getNode(User,Map) == error of
                true ->
                    ID ! {"Server", lists:concat(["Logged in as ", User, "."])},
                    serverRun(maps:put(User,ID,Map),RoomList);
                false ->
                    ID ! {"Server", "User is already logged in."},
            serverRun(Map,RoomList)
            end;
        {offlineRequest,User} ->
            ID = getNode(User,Map),
            case ID == error of   
                false ->
                    ID ! {"Server", lists:concat(["User ", User, " has been logged out."])},
                    serverRun(removeUserFromMap(User,Map),removeUserFromRooms(User,RoomList,[]));
                true ->
                    serverRun(Map,RoomList)
            end;
        {online,From} ->
            getNode(From,Map) ! {list, maps:keys(Map)},
            serverRun(Map, RoomList);
        {users,From} ->
            case isOnline(From, Map) of
                false -> false;
                true ->  getNode(From,Map) ! {list, getUserRoom(From, RoomList)}
            end,
            serverRun(Map, RoomList);
        {chatRequest,From,To} ->
            case (isOnline(To, Map) and (not isInChat(To,RoomList))) of
                false -> getNode(From,Map) ! {"Server", "User is unavailable to chat."};
                true ->  getNode(To,Map) ! {"Server", lists:concat(["Accept user ", From, "?"])}
            end,
            serverRun(Map, RoomList);
        {chatAccept,From,To} ->
            case isOnline(To, Map) of
                false -> getNode(From,Map) ! {"Server", lists:concat(["User ", To, " is offline."])};
                true -> 
                    getNode(To,Map) ! {"Server", lists:concat(["Your chat request to ", From, " was accepted."])}
            end,
            
            FromRoom = getUserRoom(From, RoomList),
            ToRoom   = getUserRoom(To, RoomList),
            NewRoom = combineUserRoom(FromRoom,ToRoom),
            ListOfPids = getPidsInRoom(NewRoom, Map, []),
            
            sendPidsToRoom(ListOfPids, ListOfPids),
            serverRun(Map, combineUserRoom(
                lists:delete(ToRoom, lists:delete(FromRoom, RoomList)),[NewRoom])
            );
        {chatReject,From,To} ->
            case isOnline(To, Map) of
                false -> getNode(From,Map) ! {"Server", lists:concat(["User ", To, " is offline."])};
                true -> 
                    getNode(To,Map) ! {"Server", lists:concat(["Your chat request to ", From, " was rejected."])}
            end,
            serverRun(Map, RoomList);
        {ping,User,Msg} ->
            maps:get(User,Map) ! {node(),Msg},
            serverRun(Map,RoomList);
        {sendMsg,From,Msg} ->
            case isInChat(From, RoomList) of
                true ->
                    sendToUsersInRoom(From, Msg, getUserRoom(From,RoomList), Map);
                false ->
                    getNode(From, Map) ! {"Server", "You are not in a chat."}
            end,
            serverRun(Map,RoomList);
        {chatQuit,From} ->
	    case isInChat(From,RoomList) of
		true ->
			io:format("User: ~p has left a chat.~n",[From]),
	    		serverRun(Map,removeUserFromRooms(From,RoomList,[]));
		false ->
		        io:format("User: ~p was not in a chat~n",[From]),
	                serverRun(Map,RoomList)
	    end;
	{Message} ->
            io:format("message is :~p~n",[Message]);
        _ ->
            false,
            serverRun(Map,RoomList)
    end.

% Checks if a user is online
isOnline(User, UserMap) ->
    maps:is_key(User,UserMap).
 
% Returns the user's address.
getNode(User, UserMap) ->
    case maps:is_key(User, UserMap) of 
        true -> maps:get(User, UserMap);
        false -> error
    end.

% Sends a msg to all user's in a room.
sendToUsersInRoom(From, Message, Room, UserMap) ->
    lists:foldl(fun(User, Msg) ->
        case User =/= From of
            true ->
                Stringified = lists:concat([Msg]),
                getNode(User, UserMap) ! {From, 
string:strip(Stringified, both, $")},
                Msg;
            false -> Msg
        end 
    end,
    Message, Room).

% Returns a list of pids in a specific room
getPidsInRoom([], _, ListOfPids) -> ListOfPids;  
getPidsInRoom(Room, UserMap, ListOfPids) ->
    [Head|Tail] = Room, 
    getPidsInRoom(Tail, UserMap, lists:append(ListOfPids, [getNode(Head, 
UserMap)])).

% Sends pids to all user's in a room.
sendPidsToRoom([], _) -> ok;
sendPidsToRoom(AddressList, ListOfPids) ->
    [Head|Tail] = AddressList,
    Head ! {pids, ListOfPids},
    sendPidsToRoom(Tail, ListOfPids).
    

% Combines two lists
combineUserRoom(RoomOne,RoomTwo) ->  lists:append(RoomOne, RoomTwo).

% Returns map with specific user missing.
removeUserFromMap(_,[])-> [];
removeUserFromMap(User,UserMap)-> 
    maps:remove(User,UserMap).

% Returns room list with specific user missing.
removeUserFromRooms(_,[],ModifiedList) -> ModifiedList;
removeUserFromRooms(User,RoomList, ModifiedList) ->
    [Head|Tail] = RoomList,
    
removeUserFromRooms(User,Tail,lists:append(ModifiedList,[lists:delete(User,Head)])).

% Returns a list of all users in a room.
getUserRoom(User,[])-> [User];
getUserRoom(User,RoomList)->
    [Head|Tail] = RoomList,
    case lists:member(User, Head) of 
        true -> Head;
        false -> getUserRoom(User, Tail)
    end.

% Checks if a user is currently in chat
isInChat(_,[])-> false;
isInChat(User,RoomList)->
    [Head|Tail] = RoomList,
    case lists:member(User, Head) of 
        true -> (lists:flatlength(Head) > 1);
        false -> isInChat(User, Tail)
    end.
