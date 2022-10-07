-module(actor).
-import(math, []).
-export[start/0, startActors/2, startActorsPushSum/3, startGossip/2].

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

    %Actors = createActors(NumberOfNodes).

    case Algorithm of
        "gossip" -> startGossip(NumberOfNodes, Topology);
        "pushsum" -> startPushSum(NumberOfNodes, Topology)
    end.

    %GridNeighbors,
    %LineNeighbors,
    %ImperfectGridNeighbors,
    %FullNetworkNeighbors.

startGossip(NumberOfNodes, Topology) ->
    io:format('Starting the Gossip Algorithm \n'),
    io:format('Choosing a random actor from the given number of actors. \n'),

    Actors = createActors(NumberOfNodes),
    {ChosenActor, Reference} = lists:nth(rand:uniform(length(Actors)), Actors),
    io:format("\nThe chosen actor is : ~p \n", [ChosenActor]),
    Neighbors = buildTopology(Topology, Actors, NumberOfNodes, ChosenActor),
    io:format("\nThe neighbors of the chosen node ~p for the topology ~p are ~p\n",[ChosenActor,Topology,Neighbors]).


%%    recurseGossip(chosenActor).

    % The count for the chosen actor has to be updated now.
    % A function for that has to be written

% START: Pick the first actor and tell the actor a gossip.
% STEP: Each actor selects a random neighbor and tells it the rumor.
% Termination: Each actor keeps track of rumors and how many times has he heard the rumor.

% Gossip type algorithms can be used both for group communication and for aggregate computation.
% Gossip Algorithm for information propagation | The Gossip algorithm involves the following:
% 1. Starting: A participant(actor) told/sent a rumor (fact) by the main process
% 2. Step: Each actor selects a random neighbor and tells it the rumor.
% 3. Termination: Each actor keeps track of rumors and how many times he has heard the rumor.
%    It stops transmitting once it has heard the rumor 10 times (10 is arbitrary, you can select other values).


%%recurseGossip(chosenActor) ->
%%    io:format("Recursive Gossip").

startPushSum(NumberOfNodes, Topology) ->
    io:format('Starting the Push Sum Algorithm \n'),
    io:format('Choosing a random actor from the given number of actors. \n'),

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
    io:format("\nEntering into Full\n"),
    Neighbors = lists:subtract(lists:seq(1, N), [Id]),
    Detailed_Neighbors = [
        {N, maps:get(N, Actors_Map)}
        || N <- Neighbors
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

startActors(Id, N) ->
    io:fwrite("I am an actor with Id : ~w\n", [Id]).


createActors(N) ->

    Actors = [  % { {Pid, Ref}, Id }
        {Id, spawn(actor, startActors, [Id, N])}
        || Id <- lists:seq(1, N)
    ],

    Actors.

createActorsPushSum(N, W) ->
    io:fwrite("Reached the Create Actors Push Sum method\n"),
    Actors = [  % { {Pid, Ref}, Id }
        {Id, spawn(actor, startActorsPushSum, [Id, W, N])}
        || Id <- lists:seq(1, N)
    ],
    Actors.

startActorsPushSum(Id, W, N) ->
    io:fwrite("I am an actor with Id : ~w\n", [Id]),
    awaitResponsePushSum(Id, Id, W).

awaitResponsePushSum(Id, S, W) ->
    receive
        {From, {S1, W1, Topology, Actors, NumberOfNodes}} ->
            io:format("P2 received message \n"),
            io:format("\n Actor ~p received pair ~p, ~p from process ~p\n", [Id, S1, W1, From]),

            % Upon receiving this the actor should add the received pair to its own corresponding values
            S = S + S1,
            W = W + W1,
            
            % Upon receiving, each actor selects a random neighbor and sends it a message.
            Neighbors = buildTopology(Topology, Actors, NumberOfNodes, Id),
            {ChosenNeighbor, ChosenNeighbor_PID} = lists:nth(rand:uniform(length(Neighbors)), Neighbors),

            % SEND: When sending a message to another actor, half of s and w is kept by the sending actor and half is placed in the message
            S = S/2,
            W = W/2,

            ChosenNeighbor_PID ! {self(), {S, W, Topology, Actors, NumberOfNodes}}
            % SUM ESTIMATE: At any given moment of time, the sum estimate is s/w where s and w are teh current values of an actor
            % TERMINATION: If an actor's ratio s/w did not change more than 10^-10 in 3 consecutive rounds the actor terminates.
    end.

getNextSquare(NumberOfNodes) ->
    SquaredNumber =  round(math:pow(math:ceil(math:sqrt(NumberOfNodes)),2)),
    SquaredNumber.


