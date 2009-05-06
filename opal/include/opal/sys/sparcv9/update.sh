#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
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

CFILE=/tmp/opal_atomic_$$.c

trap "/bin/rm -f $CFILE; exit 0" 0 1 2 15

echo Updating atomic.s from atomic.h using gcc

cat > $CFILE<<EOF
#include "../architecture.h"
#include <stdlib.h>
#include <inttypes.h>
#define static
#define inline
#define OMPI_GCC_INLINE_ASSEMBLY 1
#define OPAL_WANT_SMP_LOCKS 1
#include "atomic.h"
EOF

gcc -m64 -O3 -DOPAL_ASSEMBLY_ARCH=OMPI_SPARCV9_64 -I. -S $CFILE -o atomic64.s
gcc -mv8plus -DOPAL_ASSEMBLY_ARCH=OMPI_SPARCV9_32 -O3 -I. -S $CFILE -o atomic32.s
