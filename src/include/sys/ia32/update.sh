#!/bin/sh

CFILE=/tmp/ompi_atomic_$$.c

trap "/bin/rm -f $CFILE; exit 0" 0 1 2 15

echo Updating atomic.s from atomic.h using gcc

cat > $CFILE<<EOF
#include <stdlib.h>
#include <inttypes.h>
#define static
#define inline
#include "atomic.h"
EOF

gcc -I. -S $CFILE -o atomic.s
