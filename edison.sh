#!/bin/bash

modules=( "intel" "cray" "gnu" )
for i in `seq 1 ${#modules[@]}`
do
  rm -f a.out
  echo ${modules[i-1]}
  echo "----"
  if [ $i -gt 1 ]; then
    module swap PrgEnv-${modules[i-2]} PrgEnv-${modules[i-1]}
  fi
  ftn namelist.F90
  ./a.out
  echo ""
done
