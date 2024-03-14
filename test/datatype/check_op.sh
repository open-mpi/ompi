#!/bin/bash

set -u

echo "ompi version with AVX512 -- Usage: arg1: count of elements, args2: 'i'|'u'|'f'|'d' : datatype: signed, unsigned, float, double. args3 size of type. args4 operation"
mpirun="mpirun --mca pml ob1 --mca btl vader,self"
# For SVE-architecture
# echo "$mpirun -mca op_sve_hardware_available 0 -mca op_avx_hardware_available 0 -n 1 Reduce_local_float 1048576  i 8 max"

# For X86_64 architectures
# echo "$mpirun -mca op_avx_support 0 -n 1 Reduce_local_float 1048576 i 8 max"

Orange="\033[0;33m"
Blue="\033[0;34m"
Purple="\033[0;35m"
Yellow="\e[1;33m"

NC="\e[m"

verbose=0

echo "=========Signed Integer type all operations & all sizes========"
echo ""
for op in max min sum prod band bor bxor; do
    echo -e "\n===Operation  $op test==="
    for type_size in 8 16 32 64; do
        for size in 0 1 7 15 31 63 127 130; do
            foo=$((1024 * 1024 + $size))
            echo -e "Test $Yellow __mm512 instruction for loop $NC Total_num_bits = $foo * $type_size "
            cmd="$mpirun -n 1 reduce_local -l $foo -u $foo -t i -s $type_size -o $op"
            if test $verbose -eq 1 ; then echo $cmd; fi
            eval $cmd
        done
        echo -e "\n\n"
    done
    echo -e "\n\n"
done
echo "=========Signed Integer type all operations & all sizes========"
echo -e "\n\n"

echo "=========Unsigned Integer type all operations & all sizes========"
echo ""
for op in max min sum prod band bor bxor; do
    echo -e "\n===Operation  $op test==="
    for type_size in 8 16 32 64; do
        for size in 0 1 7 15 31 63 127 130; do
            foo=$((1024 * 1024 + $size))
            echo -e "Test $Yellow __mm512 instruction for loop $NC Total_num_bits = $foo * $type_size"
            cmd="$mpirun -n 1 reduce_local -l $foo -u $foo -t u -s $type_size -o $op"
            if test $verbose -eq 1 ; then echo $cmd; fi
            eval $cmd
        done
    done
done
echo "=========Unsigned Integer type all operations & all sizes========"
echo -e "\n\n"

echo "=======Float type all operations========"
echo ""
for op in max min sum prod; do
    for size in 1024 127 130; do
        foo=$((1024 * 1024 + $size))
        echo -e "Test $Yellow __mm512 instruction for loop $NC Total_num_bits = $foo * 32"
        cmd="$mpirun -n 1 reduce_local -l $foo -u $foo -t f -s 32 -o $op"
        if test $verbose -eq 1 ; then echo $cmd; fi
        eval $cmd
    done
done

echo "========Double type all operations========="
echo ""
for op in max min sum prod; do
    for size in 1024 127 130; do
        foo=$((1024 * 1024 + $size))
        echo -e "Test $Yellow __mm512 instruction for loop $NC Total_num_bits = $foo * 64"
        cmd="$mpirun -n 1 reduce_local -l $foo -u $foo -t d -s 64 -o $op"
        if test $verbose -eq 1 ; then echo $cmd; fi
        eval $cmd
    done
done

