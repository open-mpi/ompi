/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

pid$target:libmpi:MPI_Send:entry,
pid$target:libmpi:MPI_*send:entry,
pid$target:libmpi:MPI_Recv:entry,
pid$target:libmpi:MPI_*recv:entry
{
    printf("%s(0x%x, %d, 0x%x, %d, %d, 0x%x)", probefunc, arg0, arg1, arg2, arg3, arg4, arg5);
}

pid$target:libmpi:MPI_Send:return,
pid$target:libmpi:MPI_*send:return,
pid$target:libmpi:MPI_Recv:return,
pid$target:libmpi:MPI_*recv:return
{
    printf("\t\t = %d\n", arg1);
}
