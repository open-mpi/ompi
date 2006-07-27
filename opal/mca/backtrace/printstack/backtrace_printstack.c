/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


void
opal_backtrace_print(FILE *file)
{
    printstack(fileno(file));
}


int
opal_backtrace_buffer(char ***message_out, int *len_out);
{
    *messages_out = NULL;
    *len_out = 0;

    /* BWB - I think we can implement this in a similar way that
       printstack is implemented.  I just don't have time right
       now. */

    return OMPI_ERR_NOT_IMPLEMENTED
}
