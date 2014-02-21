/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ML_ALLOC_H
#define MCA_ML_ALLOC_H

#include "ompi_config.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/bcol/base/base.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/mpool/base/base.h"
#include "coll_ml_lmngr.h"

/*
  Returns a block of memory from mpool

  ARGS:
  IN ml_component: component descriptor
  OUT ml_memblock: block_addr - Starting address of the memory block
                   size       - Size of the block
                   register_info - Register information passed from the mpool

  Return
  On Sucess : Returns size of memory block
  On Failure: Returns -1

 */

struct mca_coll_ml_component_t;
struct mca_coll_ml_module_t;

mca_bcol_base_memory_block_desc_t *mca_coll_ml_allocate_block(
                struct mca_coll_ml_component_t  *ml_component,
                struct mca_bcol_base_memory_block_desc_t *ml_memblock
                );
    /* Allocate the memory from mpool */
    /* Register the memory block with bcols */

void mca_coll_ml_free_block(
                 mca_bcol_base_memory_block_desc_t *ml_memblock
                );




/*
   Initialize the memory block and map into buffers and memory banks, and
   also buffer descriptors are initialized.

   IN ml_memblock: Memory block descriptor
   IN num_buffers: number of buffers
   IN num_banks: number of banks
   Return
   On Sucess: OMPI_SUCCESS
   On Failure: OMPI_ERROR
 */
int mca_coll_ml_initialize_block(
        mca_bcol_base_memory_block_desc_t *ml_memblock,
        uint32_t num_buffers,
        uint32_t num_banks,
        uint32_t buffer_size,
        int32_t data_offset,
        opal_list_t *bcols_in_use
        );
    /* Map blocks into buffers and banks */
    /* Initialize the descriptors */



/*
   Allocate a memory buffer from the block
    IN ml_memblock: Memory block descriptor
    OUT ml_membuffer: Buffer allocated for data from the block

   Return
   On Sucess: OMPI_SUCCESS
   On Failure: OMPI_ERROR
 */
mca_bcol_base_payload_buffer_desc_t *mca_coll_ml_alloc_buffer(
            struct mca_coll_ml_module_t *module);

int mca_coll_ml_free_buffer(
        mca_bcol_base_memory_block_desc_t *ml_memblock,
        struct mca_bcol_base_payload_buffer_desc_t *ml_membuffer
        );

/*
   Register the memory block with bcol component

   IN ml_memblock: Memory block descriptor
   OUT registerations (ml_memblock)

   Return
   On Sucess: OMPI_SUCCESS
   On Failure: OMPI_ERROR

  */
int mca_coll_ml_register_block_bcol(
                mca_bcol_base_memory_block_desc_t *ml_memblock
                );

#endif /* MCA_ML_ALLOC_H */
