#!/bin/bash

modules=( "nag" "intel" "pgi" "gnu" )
compilers=( "nagfor" "ifort" "pgf90" "gfortran" )
for i in `seq 1 ${#modules[@]}`
do
  rm -f a.out
  echo ${compilers[i-1]}
  echo "----"
  module purge
  module load compiler/${modules[i-1]}
  ${compilers[i-1]} namelist.F90
  ./a.out
  echo ""
done
