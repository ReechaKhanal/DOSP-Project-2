-module(actor).
-import(math, []).
-export[start/0, startActors/1, startActorsPushSum/2, startGossip/2, sendGossip/5].

start() ->

    {ok, [Nodes]} = io:fread("\nEnter the number of nodes: ", "~d\n"),
    {ok, [Topology]} = io:fread("\nEnter Topology: ", "~s\n"),
    {ok, [Algorithm]} = io:fread("\nEnter the Algorithm (Gossip or PushSum): ", "~s\n"),

    if
        Topology == "2D" ->
            NumberOfNodes = getNextSquare(Nodes);
        Topology == "imp2D" ->
            NumberOfNodes = getNextSquare(Nodes);
        true ->
            NumberOfNodes = Nodes
    end,

    io:format("Number of Nodes: ~p\n", [NumberOfNodes]), % Where numNodes is the number of actors involved
    io:format("Topology: ~p\n", [Topology]), % Topology Options: Full Network, 2D Grid, Line, Imperfect 3D Grid
    io:format("Algorithm: ~p\n", [Algorithm]), % Algorithm Options: Gossip, Push-Sum

    case Algorithm of
        "gossip" -> startGossip(NumberOfNodes, Topology);
        "pushsum" -> startPushSum(NumberOfNodes, Topology)
    end.

startGossip(NumberOfNodes, Topology) ->
    io:format('Starting the Gossip Algorithm \n'),
    Actors = createActors(NumberOfNodes),
    {ChosenActor, ChosenActor_PID} = lists:nth(rand:uniform(length(Actors)), Actors),
    io:format("\nThe Actor Chosen by the Main Process is : ~p \n\n", [ChosenActor]),

    ChosenActor_PID ! {self(), {Topology, Actors, NumberOfNodes}}.

getAliveActors(Actors) ->
    Alive_Actors = [{A, A_PID} || {A, A_PID} <- Actors, is_process_alive(A_PID) == true],
    Alive_Actors.

startPushSum(NumberOfNodes, Topology) ->
    
    io:format('Starting the Push Sum Algorithm \n'),
    
    W = 1,
    Actors = createActorsPushSum(NumberOfNodes, W),
    {ChosenActor, ChosenActor_PID} = lists:nth(rand:uniform(length(Actors)), Actors),
    Neighbors = buildTopology(Topology, Actors, NumberOfNodes, ChosenActor),
    io:format("\nThe neighbors of the chosen node ~p for the topology ~p are ~p\n",[ChosenActor,Topology, Neighbors]),

    io:format("\nThe chosen actor is : ~p \n", [ChosenActor]),
    io:format("\nThe chosen actor process ID is : ~p \n", [ChosenActor_PID]),
    
    ChosenActor_PID ! {self(), {0, 0, Topology, Actors, NumberOfNodes}}.

buildTopology(Topology, Actors, NumberOfNodes, Id) ->
    Actors_Map = maps:from_list(Actors),
    case Topology of
        "full" -> findFullNetworkNeighbors(Id, NumberOfNodes, Actors_Map);
        "2D" -> find2DGridNeighbors(Id, NumberOfNodes, Actors_Map);
        "line" -> findLineGridNeighbors(Id, NumberOfNodes, Actors_Map);
        "imp2D" -> find2DIperfectGridNeighbors(Id, NumberOfNodes, Actors_Map)
    end.

findFullNetworkNeighbors(Id, N, Actors_Map) ->
    % considers everyone except itself for neighbors
    Neighbors = lists:subtract(lists:seq(1, N), [Id]),
    Detailed_Neighbors = [
        {N, maps:get(N, Actors_Map)}
        || N <- Neighbors, maps:is_key(N, Actors_Map)
    ],
    Detailed_Neighbors.

find2DGridNeighbors(Id, N, Actors_Map) ->

    % assumption provided in the question is that the 2D grid is always a perfect square.
    io:format("\nEntering into 2D\n"),
    Rows = erlang:trunc(math:sqrt(N)),
    ModVal = Id rem Rows,

    if
        ModVal == 1 ->
            Neighbors = [Id+1];
        ModVal == 0 ->
            Neighbors = [Id-1];
        true ->
            Neighbors = lists:append([[Id-1], [Id+1]])
    end,

    if
        Id+Rows > N ->
            Neighbors2 = Neighbors;
        true ->
            Neighbors2 = lists:append([Neighbors, [Id+Rows]])
    end,
    if
        Id-Rows < 1 ->
            Neighbors3 = Neighbors2;
        true ->
            Neighbors3 = lists:append([Neighbors2, [Id-Rows]])
    end,
    
    Detailed_Neighbors = [
        {N, maps:get(N, Actors_Map)}
        || N <- Neighbors3
    ],
    Detailed_Neighbors.

findLineGridNeighbors(Id, N, Actors_Map) ->

    io:format("\nEntering into Line\n"),

    if
        Id > N -> 
            Neighbors = [];
        Id < 1 ->
            Neighbors = [];
        Id + 1 > N ->
            if
                Id - 1 < 1 ->
                    Neighbors = [];
                true ->
                    Neighbors = [Id-1],
                    io:fwrite("~w",[Id-1])
            end;           
        true ->
            if
                Id - 1 < 1 ->
                    Neighbors = [Id+1];
                true ->
                    Neighbors = [Id-1, Id+1]
            end
    end,
    Detailed_Neighbors = [
        {N, maps:get(N, Actors_Map)}
        || N <- Neighbors
    ],
    Detailed_Neighbors.

find2DIperfectGridNeighbors(Id, N, Actors_Map) ->

    io:format("\nEntering into Imperfect 2D\n"),
    
    %ImmediateNeighbors = find2DGridNeighbors(Id, N, Actors_Map),

    Rows = erlang:trunc(math:sqrt(N)),
    ModVal = Id rem Rows,

    if
        ModVal == 1 ->
            Neighbors = [Id+1];
        ModVal == 0 ->
            Neighbors = [Id-1];
        true ->
            Neighbors = lists:append([[Id-1], [Id+1]])
    end,

    if
        Id+Rows > N ->
            Neighbors2 = Neighbors;
        true ->
            Neighbors2 = lists:append([Neighbors, [Id+Rows]])
    end,
    if
        Id-Rows < 1 ->
            ImmediateNeighbors = Neighbors2;
        true ->
            ImmediateNeighbors = lists:append([Neighbors2, [Id-Rows]])
    end,

    NeighborsToBeIgnored = lists:append([ImmediateNeighbors, [Id]]),
    RemainingNeighbors = lists:subtract(lists:seq(1, N), NeighborsToBeIgnored),

    RandomRemaningNeighbor = lists:nth(rand:uniform(length(RemainingNeighbors)), RemainingNeighbors),
    RandomImmediateNeighbor = lists:nth(rand:uniform(length(ImmediateNeighbors)), ImmediateNeighbors),

    FinalNeighbors = lists:append([[RandomRemaningNeighbor], [RandomImmediateNeighbor]]),
    Detailed_Neighbors = [
        {N, maps:get(N, Actors_Map)}
        || N <- FinalNeighbors
    ],
    Detailed_Neighbors.

startActors(Id) ->
    io:fwrite("START I am an actor with Id : ~w\n", [Id]),
    awaitResponseGossip(Id, 0),
    io:fwrite("END ending Process : ~w\n", [Id]).

awaitResponseGossip(Id, Count) ->
    receive
        {From, {Topology, Actors, NumberOfNodes}} ->
            if
                Count == 10 ->
                    io:format("YAAASSS WE GOT IT IN:Process: ~p || Count: ~p\n", [Id, Count]);
                true ->
                    spawn(actor, sendGossip, [self(), Topology, Actors, NumberOfNodes, Id]),
                    awaitResponseGossip(Id, Count+1)
            end
    end.

sendGossip(Current, Topology, Actors, NumberOfNodes, Id) ->
    Status = is_process_alive(Current),
    if
        Status == true ->
            Alive_Actors = getAliveActors(Actors),

            Neighbors = buildTopology(Topology, Alive_Actors, NumberOfNodes, Id),
            if
                Neighbors == [] ->
                    exit(0);
                true ->
                    {_, ChosenNeighbor_PID} = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
                    ChosenNeighbor_PID ! {Current, {Topology, Actors, NumberOfNodes}},
                    sendGossip(Current, Topology, Actors, NumberOfNodes, Id)
            end;
        true ->
            exit(0)
    end.  
    

createActors(N) ->

    Actors = [  % { {Pid, Ref}, Id }
        {Id, spawn(actor, startActors, [Id])}
        || Id <- lists:seq(1, N)
    ],

    Actors.

createActorsPushSum(N, W) ->
    Actors = [  % { {Pid, Ref}, Id }
        {Id, spawn(actor, startActorsPushSum, [Id, W])}
        || Id <- lists:seq(1, N)
    ],
    Actors.

startActorsPushSum(Id, W) ->
    io:fwrite("I am an actor with Id : ~w\n", [Id]),
    awaitResponsePushSum(Id, Id, W, 0, 0).

awaitResponsePushSum(Id, S, W, Prev_ratio, Count) ->
    receive
        {From, {S1, W1, Topology, Actors, NumberOfNodes}} ->

            if
                Count == 3 ->
                    io:fwrite("YAAASSS WE GOT IT");
                true ->
                    % Upon receiving this the actor should add the received pair to its own corresponding values
                    S2 = S + S1,
                    W2 = W + W1,
                    
                    % Upon receiving, each actor selects a random neighbor and sends it a message.
                    Neighbors = buildTopology(Topology, Actors, NumberOfNodes, Id),
                    {_, ChosenNeighbor_PID} = lists:nth(rand:uniform(length(Neighbors)), Neighbors),

                    % SEND: When sending a message to another actor, half of s and w is kept by the sending actor and half is placed in the message                        
                    S3 = S2 / 2,
                    W3 = W2 / 2,

                    ChosenNeighbor_PID ! {self(), {S3, W3, Topology, Actors, NumberOfNodes}},

                    Curr_ratio = S / W,
                    Difference = math:pow(10, -10),
                    if
                        abs(Curr_ratio - Prev_ratio) < Difference ->
                            io:format("\nPrevious Ratio: ~p & Current Ratio ~p & Difference is ~p\n",[Prev_ratio, Curr_ratio, abs(Curr_ratio - Prev_ratio)]),
                            awaitResponsePushSum(Id, S3, W3, Curr_ratio, Count + 1);
                        true ->
                            awaitResponsePushSum(Id, S3, W3, Curr_ratio, 0)
                    end

                    % awaitResponsePushSum(Id, S3, W3, Curr_ratio, New_Count)
                    % SUM ESTIMATE: At any given moment of time, the sum estimate is s/w where s and w are teh current values of an actor
                    % TERMINATION: If an actor's ratio s/w did not change more than 10^-10 in 3 consecutive rounds the actor terminates.
            end
    end.

getNextSquare(NumberOfNodes) ->
    SquaredNumber =  round(math:pow(math:ceil(math:sqrt(NumberOfNodes)),2)),
    SquaredNumber.


