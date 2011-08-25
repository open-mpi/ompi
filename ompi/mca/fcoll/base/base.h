/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** 
 * @file
 *
 * MCA fcoll base framework public interface functions.
 */

#ifndef MCA_FCOLL_BASE_H
#define MCA_FCOLL_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "opal/mca/mca.h"


BEGIN_C_DECLS

OMPI_DECLSPEC int mca_fcoll_base_open(void);

OMPI_DECLSPEC int mca_fcoll_base_close(void);

OMPI_DECLSPEC int mca_fcoll_base_file_select(struct mca_io_ompio_file_t *file,
                                             mca_base_component_t *preferred);
OMPI_DECLSPEC int mca_fcoll_base_query_table (struct mca_io_ompio_file_t *file, 
                                              char *name);
OMPI_DECLSPEC int mca_fcoll_base_file_unselect(struct mca_io_ompio_file_t *file);

OMPI_DECLSPEC int mca_fcoll_base_find_available(bool enable_progress_threads,
                                                bool enable_mpi_threads);

OMPI_DECLSPEC int mca_fcoll_base_init_file (struct mca_io_ompio_file_t *file);

OMPI_DECLSPEC int mca_fcoll_base_get_param (struct mca_io_ompio_file_t *file, int keyval);
/*
 * Globals
 */

OMPI_DECLSPEC extern int mca_fcoll_base_param;
OMPI_DECLSPEC extern int mca_fcoll_base_output;

OMPI_DECLSPEC extern bool mca_fcoll_base_components_opened_valid;
OMPI_DECLSPEC extern bool mca_fcoll_base_components_available_valid;

OMPI_DECLSPEC extern opal_list_t mca_fcoll_base_components_opened;
OMPI_DECLSPEC extern opal_list_t mca_fcoll_base_components_available;

END_C_DECLS

#endif /* MCA_BASE_FCOLL_H */
