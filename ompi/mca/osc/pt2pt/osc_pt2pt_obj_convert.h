/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
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

/*
 * utility functions for dealing with remote datatype and op structures
 */

#include "mpi.h"

#include "ompi/datatype/datatype.h"

static inline
struct ompi_datatype_t*
ompi_osc_pt2pt_datatype_create(ompi_proc_t *remote_proc,  void **payload)
{
    struct ompi_datatype_t *datatype =
        ompi_ddt_create_from_packed_description(payload, remote_proc);
    if (ompi_ddt_is_predefined(datatype)) OBJ_RETAIN(datatype);
    return datatype;
}

static inline
ompi_op_t *
ompi_osc_pt2pt_op_create(int op_id)
{
    ompi_op_t *op = MPI_Op_f2c(op_id);
    OBJ_RETAIN(op);
    return op;
}


int ompi_osc_pt2pt_process_op(ompi_osc_pt2pt_module_t *module,
                              ompi_osc_pt2pt_send_header_t *header,
                              struct ompi_datatype_t *datatype,
                              ompi_op_t *op,
                              void *inbuf,
                              size_t inbuflen);

/**
 * Convert a window index number into a module instance.
 */
static inline ompi_osc_pt2pt_module_t*
ompi_osc_pt2pt_windx_to_module(uint32_t windx)
{
    int ret;
    ompi_osc_pt2pt_module_t *module;

    /* find the right module and dispatch */
    ret = opal_hash_table_get_value_uint32(&mca_osc_pt2pt_component.p2p_c_modules,
                                           windx,
                                           (void**) (&module));
    if (OMPI_SUCCESS != ret) {
        opal_output(0, "Could not translate windx %d to a local MPI_Win instance",
                    windx);
        return NULL;
    }

    return module;
}
