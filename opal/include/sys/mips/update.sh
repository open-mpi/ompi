#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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
#include <stdlib.h>
#include <inttypes.h>
#define static
#define inline
#define OMPI_GCC_INLINE_ASSEMBLY 1
#define OMPI_WANT_SMP_LOCKS 1
#include "../architecture.h"
#include "atomic.h"
EOF

gcc -O1 -I. -S $CFILE -o atomic.s
