aces4
=====

Parallel Computational Chemistry Application

Built in C, C++ & Fortran & Java.

Successor to ACESIII [http://www.qtp.ufl.edu/]. 

The ACESIII system is described in the paper titled "A Block-Oriented Language and Runtime System for Tensor Algebra with Very Large Arrays" [http://dx.doi.org/10.1109/SC.2010.3]

See the Wiki (https://github.com/UFParLab/aces4/wiki) for more information

Executables Built
-----------------

```aces4``` - computational chemistry application
```
Options : 
-d : .dat file, generated using acesinit. Contains initialization data
-s : directory containing .siox files referred to in the .dat file.
-m : approx. upper bound for memory usage. Additional usage in overhead & bookkeeping.
```

```print_siptables``` - dumps SipTables to stdout. SipTables store all static data contained in a compiled SIAL program and the .dat file.
```
Options :
-d : .dat file, generated using acesinit. Contains initialization data
-s : directory containing .siox files referred to in the .dat file.
```


Prerequisites
-------------
```
java
GNU (gcc > v4.8, g++, gfortran, libblas-dev, liblapack-dev) or
Intel compilers & Math Libraries
```

If checking out from git:
```
automake
autoconf
libtool
```

### or
```
cmake
```

You will need ```doxygen ``` to make documentation.

Google tests v1.7 is required to run tests. 
Get it from https://github.com/google/googletest/archive/release-1.7.0.zip
Unzip it into the ```test/``` directory.


To Compile & Test
-----------------

### Using CMake
In the root of the project, say:
```BASH
git clone git@egit.ensco.com:IMPALA/sia.git
mkdir BUILD
cd BUILD
cmake ..
make
```

MPI Version
-----------
There are 2 versions of ACES4 - ```a single node``` and a ```MPI multinode version```.
By default, if an MPI compiler can be detected, the mpi version will be compiled. 

### With CMake
To disable MPI, use the ```-DHAVE_MPI=NO``` argument to cmake.

Developer Mode (More logging)
-----------------------------
Use the ```--enable-development``` switch to configure to get extra logging.


Tools
-----
Please explore the ```sia/tool/``` directory.
