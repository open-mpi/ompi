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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_SHAREDFP_DUMMY_H
#define MCA_SHAREDFP_DUMMY_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/io/ompio/io_ompio.h"


BEGIN_C_DECLS

int mca_sharedfp_dummy_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_sharedfp_base_module_1_0_0_t *
mca_sharedfp_dummy_component_file_query (int *priority);
int mca_sharedfp_dummy_component_file_unquery (mca_io_ompio_file_t *file);

int mca_sharedfp_dummy_module_init (mca_io_ompio_file_t *file);
int mca_sharedfp_dummy_module_finalize (mca_io_ompio_file_t *file);

OMPI_MODULE_DECLSPEC extern mca_sharedfp_base_component_2_0_0_t mca_sharedfp_dummy_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */ 

int mca_sharedfp_dummy_update (mca_io_ompio_file_t *fh, 
                               int num_bytes,
                               OMPI_MPI_OFFSET_TYPE current_position);
int mca_sharedfp_dummy_seek (mca_io_ompio_file_t *fh, 
                             OMPI_MPI_OFFSET_TYPE position);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */ 
                                     
END_C_DECLS

#endif /* MCA_SHAREDFP_DUMMY_H */
