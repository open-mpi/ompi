/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_COLL_LIBNBC_EXPORT_H
#define MCA_COLL_LIBNBC_EXPORT_H

#include "ompi/mca/coll/coll.h"
#include "ompi/request/request.h"

BEGIN_C_DECLS

/* Globally exported variables */
OMPI_MODULE_DECLSPEC extern const mca_coll_base_component_2_0_0_t mca_coll_libnbc_component;

int ompi_coll_libnbc_ibarrier(struct ompi_communicator_t *comm, ompi_request_t ** request,
                              struct mca_coll_base_module_2_0_0_t *module);


struct ompi_coll_libnbc_module_t {
    mca_coll_base_module_t super;
};
typedef struct ompi_coll_libnbc_module_t ompi_coll_libnbc_module_t;
OBJ_CLASS_DECLARATION(ompi_coll_libnbc_module_t);


/* Function return codes  */
#define NBC_OK 0 /* everything went fine */
#define NBC_SUCCESS 0 /* everything went fine (MPI compliant :) */
#define NBC_OOR 1 /* out of resources */
#define NBC_BAD_SCHED 2 /* bad schedule */
#define NBC_CONTINUE 3 /* progress not done */
#define NBC_DATATYPE_NOT_SUPPORTED 4 /* datatype not supported or not valid */
#define NBC_OP_NOT_SUPPORTED 5 /* operation not supported or not valid */
#define NBC_NOT_IMPLEMENTED 6
#define NBC_INVALID_PARAM 7 /* invalid parameters */
#define NBC_INVALID_TOPOLOGY_COMM 8 /* invalid topology attached to communicator */

/* number of implemented collective functions */
#define NBC_NUM_COLL 19

/* a schedule is basically a pointer to some memory location where the
 * schedule array resides */ 
typedef void* NBC_Schedule;

/* used to hang off a communicator */
typedef struct {
  MPI_Comm mycomm; /* save the shadow communicator here */
  int tag;
#ifdef NBC_CACHE_SCHEDULE
  void *NBC_Dict[NBC_NUM_COLL]; /* this should point to a struct
                                      hb_tree, but since this is a
                                      public header-file, this would be
                                      an include mess :-(. So let's void
                                      it ...*/
  int NBC_Dict_size[NBC_NUM_COLL];
#endif
} NBC_Comminfo;

struct ompi_coll_libnbc_request_t {
    ompi_request_t super;
    MPI_Comm comm;
    MPI_Comm mycomm;
    long row_offset;
    int tag;
    volatile int req_count;
    /*ompi_request_t **req_array;*/
    MPI_Request *req_array;
    NBC_Comminfo *comminfo;
    volatile NBC_Schedule *schedule;
    void *tmpbuf; /* temporary buffer e.g. used for Reduce */
    /* TODO: we should make a handle pointer to a state later (that the user
     * can move request handles) */
};
typedef struct ompi_coll_libnbc_request_t ompi_coll_libnbc_request_t;
OBJ_CLASS_DECLARATION(ompi_coll_libnbc_request_t);

typedef ompi_coll_libnbc_request_t NBC_Handle;

END_C_DECLS

#endif /* MCA_COLL_LIBNBC_EXPORT_H */
