-module(input).
-import(string,[equal/2, lower/1]). 
-export[start/3, startGossip/1].

start(numNodes, topology, algorithm) ->

    io:format("Number of Nodes: ~p\n", [numNodes]), % Where numNodes is the number of actors involved 
    io:format("Topology: ~p\n", [topology]), % Topology Options: Full Network, 2D Grid, Line, Imperfect 3D Grid
    io:format("Algorithm: ~p\n", [algorithm]), % Algorithm Options: Gossip, Push-Sum

    startGossip(numNodes).

startGossip(N) ->
    io:format('Starting the Gossip Algorithm'),
    
    Actors = createActors(N),
    Actors.

    % START: Pick the first actor and tell the actor a gossip.
    % STEP: Each actor selects a random neighbor and tells it the rumor.
    % Termination: Each actor keeps track of rumors and how many times has he heard the rumor.
    
    % Gossip type algorithms can be used both for group communication and for aggregate computation.
    % Gossip Algorithm for information propagation | The Gossip algorithm involves the following:
    % 1. Starting: A participant(actor) told/sent a rumor (fact) by the main process
    % 2. Step: Each actor selects a random neighbor and tells it the rumor.
    % 3. Termination: Each actor keeps track of rumors and how many times he has heard the rumor. 
    %    It stops transmitting once it has heard the rumor 10 times (10 is arbitrary, you can select other values).

createActors(N) ->

    Actors = [  % { {Pid, Ref}, Id }
        {spawn(actor, start, [Id, N]), Id }
        || Id <- lists:seq(1, N)
    ],

    Actors.

