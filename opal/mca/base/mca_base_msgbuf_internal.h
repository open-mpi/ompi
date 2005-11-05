/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

/**
 * @file
 *
 * Run-time MCA message buffer interface for packing/unpacking messages
 * these routines are used by both the OOB and the registry.
 */

#ifndef OMPI_MCA_BASE_MSGBUF_INTERNAL_H
#define OMPI_MCA_BASE_MSGBUF_INTERNAL_H


/*
 * This contains the internal definitions of the MCA message buffer 
 * data structures
 */

/* this struct is changing to be a opal_object with a opal_list inside! */
/* and a few locks */
struct mca_base_msgbuffer_s {
    int     msg_buff_id;    /* internal ID of this buffer */
    int     contiguous;     /* if this is a single message buffer */
    int     in_use;         /* zero if free, else 1 */
    int     resizable;      /* whether we are allowed to remalloc if needed */

    void*   base_ptr;       /* start of my memory */
    void*   data_ptr;       /* location of where next data will go */
    void*   from_ptr;       /* location of where to get the next data from */

    /* counters */
    size_t    size;         /* total size of this buffer */
    size_t    len;          /* total amount already packed */
    size_t    space;        /* how much space we have left */
                            /* yep, size=len+space */

    size_t    toend;        /* how many bytes till the end when unpacking :) */

#ifdef MCA_BASE_MSGBUF_PROFILING
    /* specialised counters */
    long    times_sent;     /* inc each time we send with it, not used? */
    long    times_freed;    /* how many times has this buffer been freed */
    long    times_resized;  /* how many times has this buffer been resized */
#endif /* used for debugging */

};


#endif /* OMPI_MCA_BASE_MSGBUF_INTERNAL_H */
