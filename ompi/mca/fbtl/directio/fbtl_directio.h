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
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FBTL_DIRECTIO_H
#define MCA_FBTL_DIRECTIO_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/ompio/io_ompio_request.h"

extern int mca_fbtl_directio_priority;
#define FBTL_DIRECTIO_BLOCK_SIZE 4096
#define FBTL_DIRECTIO_MEMALIGN_SIZE 4096 

BEGIN_C_DECLS

int mca_fbtl_directio_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_directio_component_file_query (mca_io_ompio_file_t *file, int *priority);
int mca_fbtl_directio_component_file_unquery (mca_io_ompio_file_t *file);

int mca_fbtl_directio_module_init (mca_io_ompio_file_t *file);
int mca_fbtl_directio_module_finalize (mca_io_ompio_file_t *file);

OMPI_MODULE_DECLSPEC extern mca_fbtl_base_component_2_0_0_t mca_fbtl_directio_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

ssize_t mca_fbtl_directio_preadv (mca_io_ompio_file_t *file );
ssize_t mca_fbtl_directio_pwritev (mca_io_ompio_file_t *file );
ssize_t mca_fbtl_directio_ipreadv (mca_io_ompio_file_t *file,
                               ompi_request_t *request);
ssize_t mca_fbtl_directio_ipwritev (mca_io_ompio_file_t *file,
                                ompi_request_t *request);

bool mca_fbtl_directio_progress     ( mca_ompio_request_t *req);
void mca_fbtl_directio_request_free ( mca_ompio_request_t *req);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FBTL_DIRECTIO_H */
