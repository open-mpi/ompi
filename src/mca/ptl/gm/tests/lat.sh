#!/bin/bash 
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004 The Ohio State University.
#                    All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

prog=./lat
i=$((0))
while [ $i -le $((1<<20)) ] ; 
do
    echo $i >&2
    #prun -N 2 -B0 $prog 1000 $i
    ./run.sh $prog $i 500
    #Allow the prun to clean up 
    sleep 4
    if [ $i -eq 0 ] ; then 
	i=$((1))
    else
	i=$(($i*2))
    fi
done
