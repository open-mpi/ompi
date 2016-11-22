/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/addproc/sharedfp_addproc.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
 /* IMPORTANT: Update here when adding sharedfp component interface functions*/
static mca_sharedfp_base_module_1_0_0_t  addproc =  {
    mca_sharedfp_addproc_module_init, /* initalise after being selected */
    mca_sharedfp_addproc_module_finalize, /* close a module on a communicator */
    mca_sharedfp_addproc_seek,
    mca_sharedfp_addproc_get_position,
    mca_sharedfp_addproc_read,
    mca_sharedfp_addproc_read_ordered,
    mca_sharedfp_addproc_read_ordered_begin,
    mca_sharedfp_addproc_read_ordered_end,
    mca_sharedfp_addproc_iread,
    mca_sharedfp_addproc_write,
    mca_sharedfp_addproc_write_ordered,
    mca_sharedfp_addproc_write_ordered_begin,
    mca_sharedfp_addproc_write_ordered_end,
    mca_sharedfp_addproc_iwrite,
    mca_sharedfp_addproc_file_open,
    mca_sharedfp_addproc_file_close
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_sharedfp_addproc_component_init_query(bool enable_progress_threads,
                                            bool enable_mpi_threads)
{
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_sharedfp_base_module_1_0_0_t *
        mca_sharedfp_addproc_component_file_query
        (mca_io_ompio_file_t *fh, int *priority) {
    *priority = mca_sharedfp_addproc_priority;

    /*test, and update priority*/

    return &addproc;
}

int mca_sharedfp_addproc_component_file_unquery (mca_io_ompio_file_t *file)
{
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_sharedfp_addproc_module_init (mca_io_ompio_file_t *file)
{
    return OMPI_SUCCESS;
}


int mca_sharedfp_addproc_module_finalize (mca_io_ompio_file_t *file)
{
    return OMPI_SUCCESS;
}
