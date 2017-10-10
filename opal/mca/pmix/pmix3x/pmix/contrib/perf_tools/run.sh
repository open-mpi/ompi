#!/bin/bash

OMPI_BASE=<path_to_ompi>/install/
PMIX_LIB=<path_to_pmix>/lib/
#LIBEVENT=/hpc/mtr_scrap/users/artemp/PMIx_paper/libevent-2.0.22-stable/install/lib/
#HWLOC=/hpc/mtr_scrap/users/artemp/PMIx_paper/hwloc-1.11.3/install/lib/

export PATH="$OMPI_BASE/bin:$PATH"
export LD_LIBRARY_PATH="$OMPI_BASE/lib:$PMIX_LIB:$LD_LIBRARY_PATH"

np=$1
shift
mpirun -np $np `pwd`/pmix_intra_perf $@
