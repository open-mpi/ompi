#! /usr/bin/env bash 
#

if (( $# < 2 )) ; then
    echo "usage ./contrib/platform/embedded/gen_embedded.sh prefix debug|optimized"
    exit 1
fi

prefix=$1
shift 1
platform=$1

./autogen.pl -no-ompi
./configure --prefix="${prefix}" --with-platform=contrib/platform/embedded/"${platform}"
make clean > /dev/null
make -j2 all > /dev/null
make -j2 install > /dev/null

# All done
exit 0
