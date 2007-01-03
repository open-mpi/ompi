/*
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* 
 * This script will count the number of times MPI APIs are called.  It
 * will print out its results every 10 seconds and then a final count
 * at the end.  This script can be used to see that an MPI application
 * is progressing as the number of MPI API calls should be increasing
 * over time.  In addition, it is a good way to see a summary of which
 * MPI APIs are used an application.
 */

dtrace:::BEGIN
{
    i = 2;
    printf("\n\nNumber of times MPI APIs are called in 10 second intervals\n");
}

pid$target:libmpi:MPI_*:entry
{
    @api[probefunc] = count();
}

profile:::tick-1sec
/i > 0/
{
    i--;
}

profile:::tick-1sec
/i == 0/
{
    i = 10;
    printa(@api);
}

/*
 * Print out the final results.
 */
dtrace:::END
{
    printa(@api);
}