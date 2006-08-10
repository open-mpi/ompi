/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

pid$target:libmpi:MPI_*:entry
{
    printf("Entered %s...", probefunc);
}

pid$target:libmpi:MPI_*:return
{
    printf("exiting, return value = %d\n", arg1);
}
