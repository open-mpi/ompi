#!/bin/bash

for i in {1..100} ; do
    rm -rf ~/tmp/prte*
    if [ $(expr $i % 2) == "0" ]; then
        echo "================================"
        echo "================================ Iteration $i"
        echo "================================"
    fi
    gtimeout -k 27 25 prterun -n 1 ./qspawn
    #timeout -k 133 130 mpiexec -n 1 python test/runtests.py -v -i test_spawn
    RTN=$?
    if [[ $RTN != 0 ]] ; then
        echo "=-=-=-=->> Error: Failed with $RTN"
        exit 1
    fi
    if [ $(expr $i % 2) == "0" ]; then
        echo ""
    fi
done

exit 0
