-module(factory).
-export([
    factory/0,
    conveyor_belt/2,
    truck/4,
    spawn_actors/3,
    send_packages/3,
    stop_convs/1
]).


factory() -> 
    N_belts = 10,
    Truck_cap = 50,
    N_packages = 51,

    Convs = spawn_actors(N_belts,Truck_cap, []),
    
    %%Send packages to conveyors
    send_packages(N_packages, Convs, Convs)

.

%%After all actors are spawned, return conveyor process ids for messaging
spawn_actors(0,_,Convs) -> Convs;

%%Spawns all actors (conveyor belts and trucks)
spawn_actors(N_belts, Truck_cap, Convs) -> 
    Truck_id = "truck_conv" ++ integer_to_list(N_belts) ++ "v_",
    
    %% State = {Truck_id, capacity, load, iteration}
    Truck_process_id = spawn(?MODULE, truck, [Truck_id, Truck_cap, 0, 1]),

    Conv_id= "conv_" ++ integer_to_list(N_belts),

    %% State = {conv_id, truck_process_id}
    Conv_process_id = spawn(?MODULE, conveyor_belt, [Conv_id, Truck_process_id]),


    New_convs = Convs ++ [Conv_process_id],

    io:format("Started conv number: ~p~n", [N_belts]),
    spawn_actors(N_belts-1, Truck_cap, New_convs).

%%All packages sent
send_packages(0, Convs, _) -> stop_convs(Convs);

%%Reset conveyor belt iteration
send_packages(N_belts,Convs,[]) -> send_packages(N_belts,Convs,Convs);

%%Send a package to the next conveyor
send_packages(N_packages, Convs, [H|T])->
    Size = 1,
    H ! {Size},
    send_packages(N_packages-1, Convs, T).

stop_convs([]) -> true;

%%Stop all convs
stop_convs([H|T]) ->
    H ! stop,
    stop_convs(T).


conveyor_belt(Id, Truck) -> 
    
    receive
        {Size} -> Truck ! {Id, Size}, 
                  %%io:format("~p sent a package of size ~p to its truck~n", [Id, Size]),
                  conveyor_belt(Id,Truck);

        stop -> Truck ! stop

    end.

truck(Truck_id, Capacity, Load, Iteration) -> 

    receive
        {Id, Size} when Size+Load =< Capacity ->
                    
                    %% Load package into truck
                    Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
                    io:format("~p (~p/~p).~n", [Curr_truck_id,Load+1,Capacity]),
                    truck(Truck_id, Capacity, Load + Size, Iteration);
                
        {Id, Size} when Size + Load > Capacity ->

                        %% Substitute truck and load package into the new truck
                        Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
                        New_truck_id = Truck_id ++ integer_to_list(Iteration + 1),

                        %% Log truck substitution
                        io:format("~p is full, substituting with new iteration ~p~n", [Curr_truck_id, New_truck_id]),
                        io:format("~p (~p/~p).~n", [Curr_truck_id, Load, Capacity]),

                        %% Load package into the new truck
                        truck(Truck_id, Capacity, Size, Iteration + 1);

        stop ->
            Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
            io:format("~s stopping. Final load: (~p/~p) ~n", [Curr_truck_id, Load, Capacity]),
            true
            
    end.
    