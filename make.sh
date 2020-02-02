#!/bin/bash

# Make dirs
mkdir -p bin
mkdir -p mod

# Build
mpif90 -g \
-fbounds-check \
-mtune=native \
-fopenmp \
-O3 \
-ffast-math \
src/kinds/kinds.f90 \
src/types/data_types.f90 \
src/mpi_udgb/mpi_udgb.f90 \
src/arr_fill/arr_fill.f90 \
src/main/main.f90 \
-J mod \
-o bin/test.x

cd mod 
rm *.mod

# -Wno-unused-function -fcheck=all  -Wall -O3
