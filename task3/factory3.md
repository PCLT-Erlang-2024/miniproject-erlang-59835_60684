To simulate trucks taking time to be substituted a sleep was added before the truck is replaced, using the module 'timer', with a duration defined in the beginning of the program in milliseconds.

The number of belts, the capacity of each truck, the number of conveyor belts, the maximum package size and the duration of the timeout for the truck to be replaced are defined in the beginning of the program with the values 10, 50, 1000, 10 and 1000, respectively. This allows to check if the output correct, if it is, each conveyor belt should deliver 100 packages to the correspondent trucks (the number of substitutions of trucks will vary, given that the size of the packages is randomized).

The console will print some relevant information during the execution of the program.

To test the code, the following commands should be executed in the terminal:
- erl
- c(factory).
- factory:factory().

Deadlocks and data races are avoided through the use of asynchronous communication with messages. Process starvation is avoided, since the packages are divided equally by the conveyor belts/trucks, so the corresponding processes get equal amount of work.

