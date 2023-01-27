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

CFILE=/tmp/pmix_asm_$$.c

trap "/bin/rm -f $CFILE; exit 0" 0 1 2 15

echo Updating asm.s from atomic.h and timer.h using gcc

cat > $CFILE<<EOF
#include <stdlib.h>
#include <inttypes.h>
#define static
#define inline
#define PMIX_GCC_INLINE_ASSEMBLY 1
#include "../architecture.h"
#include "atomic.h"
#include "timer.h"
EOF

gcc -O1 -mpowerpc64 -mcpu=970 -DPMIX_ASSEMBLY_ARCH=POWERPC32 -DPMIX_ASM_SUPPORT_64BIT=1 -I. -S $CFILE -o asm-32-64.s
gcc -O1 -DPMIX_ASSEMBLY_ARCH=PMIX_POWERPC32 -DPMIX_ASM_SUPPORT_64BIT=0 -I. -S $CFILE -o asm-32.s
gcc -m64 -O1 -finline-functions -DPMIX_ASSEMBLY_ARCH=PMIX_POWERPC64 -DPMIX_ASM_SUPPORT64BIT=1 -I. -S $CFILE -o asm-64.s
