-module(factory).
-export([factory/0, conveyor_belt/0, truck/0, conveyor_spawn_loop/1, truck_spawn_loop/1]).

factory() -> 
    N_BELTS = 10,
    CONV_CAP = 10,
    TRUCK_CAP = 50,
    N_PACKAGES = 1000,

    conveyor_spawn_loop(N_BELTS),
    truck_spawn_loop(N_BELTS).


conveyor_spawn_loop(0) -> io:format("Finished spawning coveyor belts.~n");
conveyor_spawn_loop(N) when N > 0 ->spawn(conveyor_belt).

truck_spawn_loop(0) -> io:format("Finished spawning trucks.~n");
truck_spawn_loop(N) when N > 0 ->spawn(truck).

conveyor_belt() -> a.

truck() -> b.
    