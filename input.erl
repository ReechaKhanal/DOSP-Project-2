-module(input).
-import(string,[equal/2, lower/1]). 
-export[start/0, startGossip/1].

start() ->

    {ok, [NumberOfNodes]} = io:fread("\nEnter the number of nodes: ", "~d\n"),
    {ok, [Topology]} = io:fread("\nEnter Topology: ", "~s\n"),
    {ok, [Algorithm]} = io:fread("\nEnter the Algorithm (Gossip or PushSum): ", "~s\n"),

    io:format("Number of Nodes: ~p\n", [NumberOfNodes]), % Where numNodes is the number of actors involved
    io:format("Topology: ~p\n", [Topology]), % Topology Options: Full Network, 2D Grid, Line, Imperfect 3D Grid
    io:format("Algorithm: ~p\n", [Algorithm]), % Algorithm Options: Gossip, Push-Sum

    if
        Topology == "2D" or Topology == "imp2D" ->
            NumberOfNodes = getNextSquare(NumberOfNodes)
    end,
    Actors = createActors(NumberOfNodes),
    startGossip(Actors).


startGossip(AllNodes) ->
    io:format('Starting the Gossip Algorithm \n'),
    io:format('Choosing a random actor from the given number of actors.'),

    ChosenActor = lists:nth(rand:uniform(length(AllNodes)), AllNodes).

    % START: Pick the first actor and tell the actor a gossip.
    % STEP: Each actor selects a random neighbor and tells it the rumor.
    % Termination: Each actor keeps track of rumors and how many times has he heard the rumor.
    
    % Gossip type algorithms can be used both for group communication and for aggregate computation.
    % Gossip Algorithm for information propagation | The Gossip algorithm involves the following:
    % 1. Starting: A participant(actor) told/sent a rumor (fact) by the main process
    % 2. Step: Each actor selects a random neighbor and tells it the rumor.
    % 3. Termination: Each actor keeps track of rumors and how many times he has heard the rumor. 
    %    It stops transmitting once it has heard the rumor 10 times (10 is arbitrary, you can select other values).

startPushSum(allNodes, startTime) ->
    io:format("All nodes received").

createActors(N) ->

    Actors = [  % { {Pid, Ref}, Id }
        {spawn(actor, start, [Id, N]), Id }
        || Id <- lists:seq(1, N)
    ],

    Actors.

