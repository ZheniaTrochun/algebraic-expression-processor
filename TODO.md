1. ~~Open parenthesis - 3th~~
2. ~~Commutativity - 4th~~
3. DataFlow systems - 5th

DataFlow executor:
1. submit takes action and complexity
2. inner pool which works in a loop and on each iteration it does 1 point of work (so division will occupy executor for 4 loops)
3. it has inner queue before pool, pool on every loop takes new tasks from queue if it (executor) has free space
4. when we run flow, it submits recursively all inner flows to executor
5. total work time - number of performed loops in executor