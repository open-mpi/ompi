/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2015 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2012 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_FS_GPFS_H
#define MCA_FS_GPFS_H

#include "ompi_config.h"
#include "opal/mca/mca.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/io/ompio/io_ompio.h"

#include <gpfs.h>

extern int mca_fs_gpfs_priority;

BEGIN_C_DECLS

int mca_fs_gpfs_component_init_query(bool enable_progress_threads,
		bool enable_mpi_threads);
struct mca_fs_base_module_1_0_0_t *
mca_fs_gpfs_component_file_query(ompio_file_t *fh, int *priority);
int mca_fs_gpfs_component_file_unquery(ompio_file_t *file);

int mca_fs_gpfs_module_init(ompio_file_t *file);
int mca_fs_gpfs_module_finalize(ompio_file_t *file);
OMPI_DECLSPEC extern mca_fs_base_component_2_0_0_t mca_fs_gpfs_component;

/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

int mca_fs_gpfs_file_open(struct ompi_communicator_t *comm, const char *filename,
		int amode, struct opal_info_t *info, struct ompio_file_t *fh);
int mca_fs_gpfs_file_set_info(struct ompio_file_t *fh,
		struct ompi_info_t *info);
int mca_fs_gpfs_file_get_info(struct ompio_file_t *fh,
		struct ompi_info_t **info_used);
int mca_fs_gpfs_io_selection(ompio_file_t *fh,
		struct ompi_info_t *info, struct ompi_info_t *info_selected);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FS_GPFS_H */
