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
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FS_PVFS2_H
#define MCA_FS_PVFS2_H
/*
#ifdef HAVE_PVFS2_H
#include "pvfs2.h"
#endif

#ifdef PVFS2_VERSION_MAJOR
#include "pvfs2-compat.h"
#endif
*/
#include "pvfs2.h"
#include "pvfs2-compat.h"

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/common/ompio/common_ompio.h"

extern int mca_fs_pvfs2_priority;
extern int mca_fs_pvfs2_stripe_size;
extern int mca_fs_pvfs2_stripe_width;
extern int mca_fs_pvfs2_IS_INITIALIZED;

BEGIN_C_DECLS

#define OMPIO_PVFS2_MAX_NAME 100

struct mca_fs_pvfs2_s {
    PVFS_credentials credentials;
    PVFS_object_ref object_ref;
};
typedef struct mca_fs_pvfs2_s mca_fs_pvfs2;

int mca_fs_pvfs2_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fs_base_module_1_0_0_t *
mca_fs_pvfs2_component_file_query (ompio_file_t *fh, int *priority);
int mca_fs_pvfs2_component_file_unquery (ompio_file_t *file);

int mca_fs_pvfs2_module_init (ompio_file_t *file);
int mca_fs_pvfs2_module_finalize (ompio_file_t *file);

OMPI_MODULE_DECLSPEC extern mca_fs_base_component_2_0_0_t mca_fs_pvfs2_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

int mca_fs_pvfs2_file_open (struct ompi_communicator_t *comm,
                            const char *filename,
                            int amode,
                            struct opal_info_t *info,
                            ompio_file_t *fh);

int mca_fs_pvfs2_file_close (ompio_file_t *fh);

int mca_fs_pvfs2_file_delete (char *filename,
                              struct opal_info_t *info);

int mca_fs_pvfs2_file_set_size (ompio_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE size);

int mca_fs_pvfs2_file_get_size (ompio_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE *size);

int mca_fs_pvfs2_file_sync (ompio_file_t *fh);

int mca_fs_pvfs2_file_seek (ompio_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE offset,
                            int whence);
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FS_PVFS2_H */
