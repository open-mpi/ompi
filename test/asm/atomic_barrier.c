/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#define OMPI_BUILDING 0
#include "ompi_config.h"

#include "include/sys/atomic.h"

int
main(int argc, char *argv[])
{
#if OMPI_HAVE_ATOMIC_MEM_BARRIER

    /* there really isn't a great way to test that the barriers
       actually barrier, but at least make sure they don't kill the
       machine.*/

    ompi_atomic_mb();
    ompi_atomic_rmb();
    ompi_atomic_wmb();

    return 0;
#else
    return 77;
#endif
}

