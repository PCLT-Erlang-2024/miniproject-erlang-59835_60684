The model for our system is implemented through an actor model, where each actor (factory, conveyor and truck) is represented through an independent process. 

The factory(serves as main) function calls(spawn_actors/4) to spawn all the processes representing the conveyer belts and the respective trucks.

It then calls (send_packages/3) an aux function that sends messages with the package to each of the conveyors alternately until there none left to send in which case it calls a function (stop_convs/1) that sends all the conveyors a message signaling the end of packages. 

Conveyor belts (conveyor_belt/1) receive messages from the factory and act accordingly to its content. If it's a 'stop' message, it will send a message to the corresponding truck signaling the end of packages and terminates, otherwise, it sends a message with the package.

Trucks (truck/5) receive messages from their correspondent conveyor belt and act accordingly. If it's a 'stop' message, it terminates, otherwise, it looks at the content of the package received. If the truck is too full to take it, it gets replaced and then the new truck gets loaded with it, so that there are no missing packages.

The number of belts, the capacity of each truck, and the number of conveyor belts are defined in the beginning of the program. They can be exchanged. The proposed example is 10 conveyors, for 1000 packages with trucks of 50 capacity. This allows for an easy check if the output correct, as if it is, each conveyor belt should deliver 100 packages to the correspondent trucks and its truck should be substituted once. All the trucks should finish with 50 packages in each. 

The console will print some relevant information during the execution of the program.

To test the code, the following commands should be executed in the terminal:
- erl
- c(factory).
- factory:factory().

Deadlocks and data races are avoided through the use of asynchronous communication with messages. Process starvation is avoided, since the packages are divided equally by the conveyor belts/trucks, so the corresponding processes get equal amount of work.
