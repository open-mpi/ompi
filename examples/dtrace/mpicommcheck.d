/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

BEGIN
{
    allocations = 0;
    deallocations = 0;
    prcnt = 0;
}

pid$target:libmpi:MPI_Comm_create:entry,
pid$target:libmpi:MPI_Comm_dup:entry,
pid$target:libmpi:MPI_Comm_split:entry
{
    ++allocations;
    @counts[probefunc] = count();
    @stacks[ustack()] = count();
}

pid$target:libmpi:MPI_Comm_free:entry
{
    ++deallocations;
    @counts[probefunc] = count();
    @stacks[ustack()] = count();
}

profile:::tick-1sec
/++prcnt > 10/
{
    printf("=====================================================================");
    printa(@counts);
    printf("Communicator Allocations = %d \n", allocations);
    printf("Communicator Deallocations = %d\n", deallocations);
    prcnt = 0;
}

END
{
    printf("Communicator Allocations = %d, Communicator Deallocations = %d\n",
	allocations, deallocations);
}    


