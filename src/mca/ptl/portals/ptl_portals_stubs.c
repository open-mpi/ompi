/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include "ptl_portals.h"

/* BWB - README - BWB - README - BWB - README - BWB - README - BWB
 *
 * These are stub functions that return error so that the
 * initialization code can be developed and the whole thing will
 * link.  This file will disappear once all functions are
 * implemented.  Do not implement any functions in this file.
 *
 * BWB - README - BWB - README - BWB - README - BWB - README - BWB */


int
mca_ptl_portals_module_init()
{
    return OMPI_ERROR;
}

int
mca_ptl_portals_finalize(struct mca_ptl_base_module_t *ptl)
{
    return OMPI_ERROR;
}

int
mca_ptl_portals_add_procs(struct mca_ptl_base_module_t *ptl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_ptl_base_peer_t **peers,
			  ompi_bitmap_t * reachable)
{
    return OMPI_ERROR;
}


int
mca_ptl_portals_del_procs(struct mca_ptl_base_module_t *ptl,
			  size_t nprocs,
			  struct ompi_proc_t **procs,
			  struct mca_ptl_base_peer_t **peers)
{
    return OMPI_ERROR;
}


int
mca_ptl_portals_request_init(struct mca_ptl_base_module_t *ptl,
			     struct mca_pml_base_send_request_t *req)
{
    return OMPI_ERROR;
}


void
mca_ptl_portals_request_fini(struct mca_ptl_base_module_t *ptl,
			     struct mca_pml_base_send_request_t *req)
{
    return;
}

void
mca_ptl_portals_matched(struct mca_ptl_base_module_t *ptl,
			struct mca_ptl_base_recv_frag_t *frag)
{
    return;
}

int
mca_ptl_portals_send(struct mca_ptl_base_module_t *ptl,
		     struct mca_ptl_base_peer_t *ptl_peer,
		     struct mca_pml_base_send_request_t *req,
		     size_t offset, size_t size, int flags)
{
    return OMPI_ERROR;
}

int
mca_ptl_portals_send_continue(struct mca_ptl_base_module_t *ptl,
			      struct mca_ptl_base_peer_t *ptl_peer,
			      struct mca_pml_base_send_request_t *req,
			      size_t offset, size_t size, int flags)
{
    return OMPI_ERROR;
}
