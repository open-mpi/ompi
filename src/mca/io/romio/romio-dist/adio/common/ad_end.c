/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_end.c,v 1.6 2002/10/24 17:01:11 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

void ADIO_End(int *error_code)
{
    ADIOI_Flatlist_node *curr, *next;
    ADIOI_Malloc_async *tmp;
    ADIOI_Malloc_req *tmp1;
    
/*    FPRINTF(stderr, "reached end\n"); */

/* delete the flattened datatype list */
    curr = ADIOI_Flatlist;
    while (curr) {
	if (curr->blocklens) ADIOI_Free(curr->blocklens);
	if (curr->indices) ADIOI_Free(curr->indices);
	next = curr->next;
	ADIOI_Free(curr);
	curr = next;
    }
    ADIOI_Flatlist = NULL;

    if (ADIOI_Async_list_head) {
	FPRINTF(stderr, "ADIO_End: Error! There are outstanding nonblocking I/O operations!\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

/* free list of available ADIOI_Async_nodes. */
    while (ADIOI_Malloc_async_head) {
	ADIOI_Free(ADIOI_Malloc_async_head->ptr);
	tmp = ADIOI_Malloc_async_head;
	ADIOI_Malloc_async_head = ADIOI_Malloc_async_head->next;
	ADIOI_Free(tmp);
    }
    ADIOI_Async_avail_head = ADIOI_Async_avail_tail = NULL;
    ADIOI_Malloc_async_head = ADIOI_Malloc_async_tail = NULL;

/* free all available request objects */
    while (ADIOI_Malloc_req_head) {
	ADIOI_Free(ADIOI_Malloc_req_head->ptr);
	tmp1 = ADIOI_Malloc_req_head;
	ADIOI_Malloc_req_head = ADIOI_Malloc_req_head->next;
	ADIOI_Free(tmp1);
    }
    ADIOI_Malloc_req_head = ADIOI_Malloc_req_tail = NULL;

/* free file, request, and info tables used for Fortran interface */
    if (ADIOI_Ftable) ADIOI_Free(ADIOI_Ftable);
    if (ADIOI_Reqtable) ADIOI_Free(ADIOI_Reqtable);
#ifndef HAVE_MPI_INFO
    if (MPIR_Infotable) ADIOI_Free(MPIR_Infotable);
#endif

    *error_code = MPI_SUCCESS;
}



/* This is the delete callback function associated with
   ADIO_Init_keyval when MPI_COMM_WORLD is freed */

int ADIOI_End_call(MPI_Comm comm, int keyval, void *attribute_val, void
		  *extra_state)
{
    int error_code;

    ADIO_End(&error_code);
    return error_code;
}
