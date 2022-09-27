-module(actor).

-export[start/2].

start(Id, N) ->
    io:fwrite("I am an actor"),
    neighbors = find2D_GridNeighbors(Id, N),
    
    neighbors.

find2D_GridNeighbors(Id, N) ->

    Rows = erlang:trunc(math:sqrt(N)),
    Cols = erlang:trunc(math:sqrt(N)),

    io:fwrite(" Row: ~w  \n", [Rows]),

    Neighbors = lists:append([[Id+Rows], [Id-Cols]]),
    ModVal = Id rem Rows,

    case ModVal > 1 of
        false ->
            if
                ModVal == 0 ->
                    Neighbors = lists:append(Neighbors, [Id-1]);    
                true ->
                    Neighbors = lists:append(Neighbors, [Id+1])
            end;
        true ->  
            Neighbors = lists:append(Neighbors, [Id+1])
    end,
    io:fwrite("I am an actor"),
    Neighbors.

