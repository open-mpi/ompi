/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_init.c,v 1.7 2002/10/24 17:01:12 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

ADIOI_Flatlist_node *ADIOI_Flatlist;
ADIOI_Async_node *ADIOI_Async_list_head, *ADIOI_Async_list_tail;
    /* list of outstanding asynchronous requests */
ADIOI_Async_node *ADIOI_Async_avail_head, *ADIOI_Async_avail_tail;
    /* list of available (already malloced) nodes for above async list */
ADIOI_Malloc_async *ADIOI_Malloc_async_head, *ADIOI_Malloc_async_tail;
  /* list of malloced areas for async_list, which must be freed in ADIO_End */

ADIOI_Req_node *ADIOI_Req_avail_head, *ADIOI_Req_avail_tail;
    /* list of available (already malloced) request objects */
ADIOI_Malloc_req *ADIOI_Malloc_req_head, *ADIOI_Malloc_req_tail;
    /* list of malloced areas for requests, which must be freed in ADIO_End */

/* for f2c and c2f conversion */
ADIO_File *ADIOI_Ftable;
int ADIOI_Ftable_ptr, ADIOI_Ftable_max;
ADIO_Request *ADIOI_Reqtable;
int ADIOI_Reqtable_ptr, ADIOI_Reqtable_max;
#ifndef HAVE_MPI_INFO
MPI_Info *MPIR_Infotable;
int MPIR_Infotable_ptr, MPIR_Infotable_max;
#endif

#ifdef XFS
int ADIOI_Direct_read, ADIOI_Direct_write;
#endif

int ADIO_Init_keyval=MPI_KEYVAL_INVALID;

MPI_Errhandler ADIOI_DFLT_ERR_HANDLER = MPI_ERRORS_RETURN;

void ADIO_Init(int *argc, char ***argv, int *error_code)
{
#ifdef XFS
    char *c;
#endif

/* initialize the linked list containing flattened datatypes */
    ADIOI_Flatlist = (ADIOI_Flatlist_node *) ADIOI_Malloc(sizeof(ADIOI_Flatlist_node));
    ADIOI_Flatlist->type = (MPI_Datatype) NULL;
    ADIOI_Flatlist->next = NULL;
    ADIOI_Flatlist->blocklens = NULL;
    ADIOI_Flatlist->indices = NULL;

    ADIOI_Async_list_head = ADIOI_Async_list_tail = NULL;
    ADIOI_Async_avail_head = ADIOI_Async_avail_tail = NULL;
    ADIOI_Malloc_async_head = ADIOI_Malloc_async_tail = NULL;

    ADIOI_Req_avail_head = ADIOI_Req_avail_tail = NULL;
    ADIOI_Malloc_req_head = ADIOI_Malloc_req_tail = NULL;

    ADIOI_Ftable = NULL;
    ADIOI_Ftable_ptr = ADIOI_Ftable_max = 0;

    ADIOI_Reqtable = NULL;
    ADIOI_Reqtable_ptr = ADIOI_Reqtable_max = 0;

#ifndef HAVE_MPI_INFO
    MPIR_Infotable = NULL;
    MPIR_Infotable_ptr = MPIR_Infotable_max = 0;
#endif

#ifdef XFS
    c = getenv("MPIO_DIRECT_READ");
    if (c && (!strcmp(c, "true") || !strcmp(c, "TRUE"))) 
	ADIOI_Direct_read = 1;
    else ADIOI_Direct_read = 0;
    c = getenv("MPIO_DIRECT_WRITE");
    if (c && (!strcmp(c, "true") || !strcmp(c, "TRUE"))) 
	ADIOI_Direct_write = 1;
    else ADIOI_Direct_write = 0;
#endif

    *error_code = MPI_SUCCESS;
}
