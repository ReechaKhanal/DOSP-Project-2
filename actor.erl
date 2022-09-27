-module(actor).

-export[start/2].

start(Id, N) ->
    io:fwrite("I am an actor\n"),
    FullNetworkNeighbors = findFullNetworkNeighbors(Id, N),
    GridNeighbors = find2DGridNeighbors(Id, N),
    LineNeighbors = findLineGridNeighbors(Id, N),
    ImperfectGridNeighbors = find2DIperfectGridNeighbors(Id, N),
    GridNeighbors,
    LineNeighbors,
    ImperfectGridNeighbors,
    FullNetworkNeighbors.

findFullNetworkNeighbors(Id, N) ->
    % considers everyone except itself for neighbors
    Neighbors = lists:subtract(lists:seq(1, N), [Id]),
    Neighbors.

find2DGridNeighbors(Id, N) ->

    % assumption provided in the question is that the 2D grid is always a perfect square.
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
    Neighbors3.

findLineGridNeighbors(Id, N) ->

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
    Neighbors.

find2DIperfectGridNeighbors(Id, N) ->
    
    ImmediateNeighbors = find2DGridNeighbors(Id, N),

    NeighborsToBeIgnored = lists:append([ImmediateNeighbors, [Id]]),
    RemainingNeighbors = lists:subtract(lists:seq(1, N), NeighborsToBeIgnored),

    RandomRemaningNeighbor = lists:nth(rand:uniform(length(RemainingNeighbors)), RemainingNeighbors),
    RandomImmediateNeighbor = lists:nth(rand:uniform(length(ImmediateNeighbors)), ImmediateNeighbors),

    FinalNeighbors = lists:append([[RandomRemaningNeighbor], [RandomImmediateNeighbor]]),
    FinalNeighbors.