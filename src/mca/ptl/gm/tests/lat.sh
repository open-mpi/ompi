#!/bin/bash 
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
