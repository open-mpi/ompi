#!/bin/bash
i=0
for (( ; ; ))
do
	prun -n 1 hostname > /dev/null
	((i++))
	if [ $i -eq 1000 ]
	then
		echo Executed $i times
		i=0;
	fi
done
