-module(factory).
-export([
    factory/0,
    conveyor_belt/1,
    truck/6,
    spawn_actors/4,
    send_packages/4,
    stop_convs/1
]).

factory() ->
    N_belts = 10,
    Truck_cap = 50,
    N_packages = 1000,
    Package_max_size = 10,
    Truck_timeout = 1000,

    Convs = spawn_actors(N_belts, Truck_cap, [], Truck_timeout),

    %%Send packages to conveyors
    send_packages(N_packages, Convs, Convs, Package_max_size).

%%After all actors are spawned, return conveyor process ids for messaging
spawn_actors(0, _, Convs, _) ->
    Convs;

%%Spawns all actors (conveyor belts and trucks)
spawn_actors(N_belts, Truck_cap, Convs, Truck_timeout) ->
    Truck_id = "truck_conv" ++ integer_to_list(N_belts) ++ "v_",

    %% State = {Truck_id, capacity, load, iteration, total_packages}
    Truck_process_id = spawn(?MODULE, truck, [Truck_id, Truck_cap, 0, 1, 0, Truck_timeout]),

    %% State = {conv_id, truck_process_id}
    Conv_process_id = spawn(?MODULE, conveyor_belt, [Truck_process_id]),

    New_convs = Convs ++ [Conv_process_id],

    io:format("Started conv number: ~p~n", [N_belts]),
    spawn_actors(N_belts - 1, Truck_cap, New_convs, Truck_timeout).

%%All packages sent
send_packages(0, Convs, _, _) ->
    stop_convs(Convs);

%%Reset conveyor belt iteration
send_packages(N_packages, Convs, [], Package_max_size) ->
    send_packages(N_packages, Convs, Convs, Package_max_size);

%%Send a package to the next conveyor
send_packages(N_packages, Convs, [H | T], Package_max_size) ->
    Size = rand:uniform(Package_max_size),
    H ! {Size},
    send_packages(N_packages - 1, Convs, T, Package_max_size).

stop_convs([]) ->
    true;
%%Stop all convs
stop_convs([H | T]) ->
    H ! stop,
    stop_convs(T).

conveyor_belt(Truck) ->
    receive
        {Size} ->
            Truck ! {Size},
            %%io:format("~p sent a package of size ~p to its truck~n", [Id, Size]),
            conveyor_belt(Truck);
        stop ->
            Truck ! stop
    end.

truck(Truck_id, Capacity, Load, Iteration, Total_packages, Truck_timeout) ->
    receive
        {Size} when Size + Load =< Capacity ->
            %% Load package into truck
            Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
            io:format("~p (~p/~p).~n", [Curr_truck_id, Load + 1, Capacity]),
            truck(Truck_id, Capacity, Load + Size, Iteration, Total_packages + 1, Truck_timeout);
        {Size} when Size + Load > Capacity ->
            %% Substitute truck
            Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
            New_truck_id = Truck_id ++ integer_to_list(Iteration + 1),

            %%Timeout
            timer:sleep(Truck_timeout),
            io:format("~p is done waiting for the substituiton timeout. ~n", [Curr_truck_id]),

            %% Log truck substitution
            io:format("~p (~p/~p).~n", [Curr_truck_id, Load, Capacity]),
            io:format("~p is full, substituting with new iteration ~p~n", [Curr_truck_id, New_truck_id]),

            %% Load package into the new truck
            truck(Truck_id, Capacity, Size, Iteration + 1, Total_packages + 1, Truck_timeout);
        stop ->
            Curr_truck_id = Truck_id ++ integer_to_list(Iteration),
            io:format(
                "~s stopping. Final load: (~p/~p). The total amount of packages received by these trucks was: ~p ~n ",
                [Curr_truck_id, Load, Capacity, Total_packages]
            ),
            true
    end.
