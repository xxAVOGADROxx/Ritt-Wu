# Implementation of the Ritt-Wu's algorithm.

This project implements the Characteristic set method developed by Ritt and Wu. We use Haskell programming language, massiv libary to be more precise, to compute the elements of an ascending chaing with parallel computing.

### Prerequisites

The project is under constant development using GHC 8.6.4. Compilers under this version doesn't work correctly. We recommend use versions higher than 8.6.x. Also, the proyect execution need the cabal (recommended) or stack CLT.
## Running the tests

The test are evaluated using cabal new-bench, for testing purposes the default test are 10 samples.
## Built With

* [Massiv](http://hackage.haskell.org/package/massiv-0.4.4.0/docs/Data-Massiv-Array.html) - Haskell library to manage parallesm using OS threads.

## Authors

* **Jose Seraquive** - *Initial work*

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

