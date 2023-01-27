#!/bin/bash

loops=20000
#loops=2

TIMEOUT="timeout --preserve-status -k 32 30"
TIMEOUT=""

#timeout --preserve-status -k 32 30 ./opal_fifo
#MCA_PARAMS="-host rhc001:24 -mca coll ^hcoll -mca pml ob1 -mca btl tcp,self -mca oob tcp --tag-output"

VALGRIND_OPTS=""
#VALGRIND_OPTS="valgrind --track-origins=yes"

cmd="${TIMEOUT} ./simptest -n 20"

#export PMIX_DEBUG=2
#export PMIX_MCA_ptl=tcp

i=1
while [ $i -le $loops ]; do
    echo "--------------------------------------------"
    echo "-------------------- Iteration $i of $loops"
    starttime=`date +%s`
    $cmd
    rc=$?
    endtime=`date +%s`

    echo "Loop $i of $loops. rc=$rc r=$ranks s=$starttime e=$endtime d=$((endtime - starttime))"
    if [ $rc != 0 ] ; then
        if [ $rc == 124 ] ; then
            echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX TIMEOUT - skip"
        else
            echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX FAILURE"
        fi
        exit -1
    fi

    i=$((i + 1))
done

exit 0
