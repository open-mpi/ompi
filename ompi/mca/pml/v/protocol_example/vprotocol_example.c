/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_example.h"

mca_vprotocol_example_module_t mca_vprotocol_example = 
{
  { 
    /* mca_pml_base_module_add_procs_fn_t     */ mca_vprotocol_example_add_procs,
    /* mca_pml_base_module_del_procs_fn_t     */ mca_vprotocol_example_del_procs,
    /* mca_pml_base_module_enable_fn_t        */ mca_vprotocol_example_enable,
    /* mca_pml_base_module_progress_fn_t      */ mca_vprotocol_example_progress,

    /* mca_pml_base_module_add_comm_fn_t      */ mca_vprotocol_example_add_comm,       
    /* mca_pml_base_module_del_comm_fn_t      */ mca_vprotocol_example_del_comm,
    /* mca_pml_base_module_irecv_init_fn_t    */ mca_vprotocol_example_irecv_init,
    /* mca_pml_base_module_irecv_fn_t         */ mca_vprotocol_example_irecv,
    /* mca_pml_base_module_recv_fn_t          */ mca_vprotocol_example_recv,
    /* mca_pml_base_module_isend_init_fn_t    */ mca_vprotocol_example_isend_init,
    /* mca_pml_base_module_isend_fn_t         */ mca_vprotocol_example_isend,
    /* mca_pml_base_module_send_fn_t          */ mca_vprotocol_example_send,
    /* mca_pml_base_module_iprobe_fn_t        */ mca_vprotocol_example_iprobe,
    /* mca_pml_base_module_probe_fn_t         */ mca_vprotocol_example_probe,
    /* mca_pml_base_module_start_fn_t         */ mca_vprotocol_example_start, 

    /* mca_pml_base_module_dump_fn_t          */ mca_vprotocol_example_dump,
    
    /* opal_class_t *                         */ NULL,
  },
/**
  * Insert here your own protocol structures
  */
};

OMPI_DECLSPEC int mca_vprotocol_example_dump(struct ompi_communicator_t* comm, int verbose)
{
  V_OUTPUT("vprotocol_example dump for comm %d", comm->c_contextid);
  return mca_pml_v.host_pml.pml_dump(comm, verbose);
}

