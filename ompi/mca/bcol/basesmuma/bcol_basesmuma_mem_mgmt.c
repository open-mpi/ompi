/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "bcol_basesmuma.h"


/* Shared memory registration function: Calls into the "shared memory
   connection manager" (aka - smcm) and registers a chunk of memory by
   opening and mmaping a file.

   @input:

   void *reg_data  - shared memory specific data needed by the registration
   function.

   void *base      - pointer to memory address.

   size_t size     - size of memory chunk to be registered with sm.

   mca_mpool_base_registration_t *reg  - registration data is cached here.

   @output:

   returns OMPI_SUCCESS on successful registration.

   returns OMPI_ERROR on failure.

*/

int mca_bcol_basesmuma_register_sm(void *context_data, void *base, size_t size,
                                   void **reg_desc)
{

    /* local variables */
    int ret = OMPI_SUCCESS;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    bcol_basesmuma_registration_data_t *sm_reg =
        (bcol_basesmuma_registration_data_t*) context_data;

    /* cache some info on sm_reg aka "context_data", you'll need it later */
    sm_reg->base_addr = base;
    sm_reg->size = size;

    /* call into the shared memory registration function in smcm
     * we need to be sure that the memory is page aligned in order
     * to "map_fixed"
     */
    sm_reg->sm_mmap = bcol_basesmuma_smcm_mem_reg(base, size,
                                                  sm_reg->data_seg_alignment,
                                                  sm_reg->file_name);
    if(NULL == sm_reg->sm_mmap) {
        opal_output (ompi_bcol_base_framework.framework_output, "Bcol_basesmuma memory registration error");
        return OMPI_ERROR;
    }

    /* don't let other communicators re-register me! */
    cs->mpool_inited = true;
    /* alias back to component */
    cs->sm_payload_structs = sm_reg->sm_mmap;

    return ret;
}

/* Shared memory deregistration function - deregisters memory by munmapping it and removing the
   shared memory file.

   Basic steps (please let me know if this is incompatible with your notion of deregistration
   or if it causes problems on cleanup):

   1. munmap the shared memory file.
   2. set the base pointer to the mmaped memory to NULL.
   3. permanently remove the shared memory file from the directory.

*/

int mca_bcol_basesmuma_deregister_sm(void *context_data, void *reg)
{
    /* local variables */
    bcol_basesmuma_registration_data_t *sm_reg =
        (bcol_basesmuma_registration_data_t*) context_data;

    if (sm_reg->sm_mmap) {
        OBJ_RELEASE(sm_reg->sm_mmap);
    }

    /* set the pointer to NULL */
    sm_reg->base_addr = NULL;

    return OMPI_SUCCESS;
}
