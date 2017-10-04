# Benchmarks for the different iteration schemes

See https://github.com/sgraf812/journal/blob/master/datafix.md.html.

Currently, this implements an algorithm that can do the third scheme (e.g. `recompute` all unstable nodes not yet present in the call stack). 
The results of benchmarking this can be seen in an older version of this folder from [`b9f1b69`](https://github.com/sgraf812/datafix/tree/b9f1b69b5f7edbed7eb63a9d958f0ec32a83c0a3/schemes).

This has implications on performance: An approach that relinquishes the third scheme can be implemented more efficiently, as outlined in the above document.