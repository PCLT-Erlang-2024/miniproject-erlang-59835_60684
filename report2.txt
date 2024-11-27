In order to accommodate packages with different sizes, we randomize a number between 1 and the maximum size of the packages, defined in the beginning of the program.
The randomizing is done using the module 'rand'.

Like in task 1, if the next package to be loaded in a truck exceeds its capacity, then the truck will be replaced and the package will be added to the replacement truck.

The number of belts, the capacity of each truck, the number of conveyor belts and the maximum package size are defined in the beginning of the program with the values 10, 50, 1000 and 10, respectively. This allows to check if the output correct, if it is, each conveyor belt should deliver 100 packages to the correspondent trucks (the number of substitutions of trucks will vary, given that the size of the packages is randomized).

The console will print some relevant information during the execution of the program.

To test the code, the following commands should be executed in the terminal:
- erl
- c(factory).
- factory:factory().

Deadlocks and data races are avoided through the use of asynchronous communication with messages. Process starvation is avoided, since the packages are divided equally by the conveyor belts/trucks, so the corresponding processes get equal amount of work.
