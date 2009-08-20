/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
 * Hoke CRCP component
 *
 */

#ifndef MCA_CRCP_HOKE_BTL_EXPORT_H
#define MCA_CRCP_HOKE_BTL_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/bkmrk/crcp_bkmrk.h"

BEGIN_C_DECLS

    /*
     * BTL Coordination functions
     */
    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_add_procs
    ( struct mca_btl_base_module_t* btl,
      size_t nprocs,
      struct ompi_proc_t** procs,
      struct mca_btl_base_endpoint_t** endpoints,
      struct opal_bitmap_t* reachable,
      ompi_crcp_base_btl_state_t* );

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_del_procs
    ( struct mca_btl_base_module_t* btl,
      size_t nprocs,
      struct ompi_proc_t** procs,
      struct mca_btl_base_endpoint_t**,
      ompi_crcp_base_btl_state_t*);
    
    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_register
    ( struct mca_btl_base_module_t* btl,
      mca_btl_base_tag_t tag,
      mca_btl_base_module_recv_cb_fn_t cbfunc,
      void* cbdata,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_finalize
    ( struct mca_btl_base_module_t* btl,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_alloc
    ( struct mca_btl_base_module_t* btl,
      size_t size,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_free
    ( struct mca_btl_base_module_t* btl,
      mca_btl_base_descriptor_t* descriptor,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_prepare_src
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      mca_mpool_base_registration_t* registration,
      struct opal_convertor_t* convertor,
      size_t reserve,
      size_t* size,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_prepare_dst
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      mca_mpool_base_registration_t* registration,
      struct opal_convertor_t* convertor,
      size_t reserve,
      size_t* size,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_send
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      struct mca_btl_base_descriptor_t* descriptor,
      mca_btl_base_tag_t tag,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_put
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      struct mca_btl_base_descriptor_t* descriptor,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_get
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      struct mca_btl_base_descriptor_t* descriptor,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_dump
    ( struct mca_btl_base_module_t* btl,
      struct mca_btl_base_endpoint_t* endpoint,
      int verbose,
      ompi_crcp_base_btl_state_t*);

    ompi_crcp_base_btl_state_t* ompi_crcp_base_coord_btl_ft_event
    (int state,
     ompi_crcp_base_btl_state_t*);

END_C_DECLS

#endif /* MCA_CRCP_HOKE_BTL_EXPORT_H */
