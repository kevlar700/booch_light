# Booch components for constrained systems

Grady Booch from Rational Software released Ada 83 source code along with a book SOFTWARE COMPONENTS WITH Ada in 1987. His original code still compiles with the latest Gnat compilers just by changing the extension of his files to e.g. .ads and serves as a testament to Adas stability.

Grady Booch has kindly agreed for me to release his code under a more permissive MIT license.

His re-usable components are fully documented in the above book but include Lists, Maps, Graphs, Strings of items, Queues and even Calendar utilities and sorting algorithms etc..

Whilst Ada containers are not compatible with light runtimes. Grady Booch components have been adapted to work without exception propagation. Bounded components have been kept as well as some unbounded. The unbounded components will never release storage and re-use when able. Some code is a work in progress as the need arises such as re-enabling code that required the secondary stack or recursion such as for e.g. array quick sort. Whilst these features are technically compatible with any runtime they are also not ideal for constrained environments and should be replaceable without much difficulty.

This repository has SPARK mode enabled but proving can only be done upon instantiation of generic packages and so SPARK proving perhaps to silver level will hopefully progress with time.

There are Booch 95 Components available online and that may have prompted Adas own Container packages but they utilise some later Ada features such as protected objects whilst this repositories code is targetted at constrained systems like the light runtimes themselves are.
