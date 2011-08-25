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

#ifndef MCA_FBTL_POSIX_H
#define MCA_FBTL_POSIX_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/io/ompio/io_ompio.h"

extern int mca_fbtl_posix_priority;

BEGIN_C_DECLS

int mca_fbtl_posix_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_posix_component_file_query (mca_io_ompio_file_t *file, int *priority);
int mca_fbtl_posix_component_file_unquery (mca_io_ompio_file_t *file);

int mca_fbtl_posix_module_init (mca_io_ompio_file_t *file);
int mca_fbtl_posix_module_finalize (mca_io_ompio_file_t *file);

OMPI_MODULE_DECLSPEC extern mca_fbtl_base_component_2_0_0_t mca_fbtl_posix_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */ 

size_t mca_fbtl_posix_preadv (mca_io_ompio_file_t *file,
                              int *sorted);
size_t mca_fbtl_posix_pwritev (mca_io_ompio_file_t *file,
                               int *sorted);
size_t mca_fbtl_posix_ipreadv (mca_io_ompio_file_t *file,
                               int *sorted,
                               ompi_request_t **request);
size_t mca_fbtl_posix_ipwritev (mca_io_ompio_file_t *file,
                                int *sorted,
                                ompi_request_t **request);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */ 
                                     
END_C_DECLS

#endif /* MCA_FBTL_POSIX_H */
