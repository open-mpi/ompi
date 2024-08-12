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
 * Copyright (c) 2008-2017 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserverd.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FS_LUSTRE_H
#define MCA_FS_LUSTRE_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/common/ompio/common_ompio.h"

extern int mca_fs_lustre_priority;
extern int mca_fs_lustre_stripe_size;
extern int mca_fs_lustre_stripe_width;
extern int mca_fs_lustre_lock_algorithm;

#define FS_LUSTRE_LOCK_AUTO        0
#define FS_LUSTRE_LOCK_NEVER       1
#define FS_LUSTRE_LOCK_ENTIRE_FILE 2
#define FS_LUSTRE_LOCK_RANGES      3

BEGIN_C_DECLS

#include <lustre/lustreapi.h>
#include <lustre/lustre_user.h>

#ifndef LOV_MAX_STRIPE_COUNT
#define LOV_MAX_STRIPE_COUNT 160
#endif


int mca_fs_lustre_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fs_base_module_1_0_0_t *
mca_fs_lustre_component_file_query (ompio_file_t *fh, int *priority);
int mca_fs_lustre_component_file_unquery (ompio_file_t *file);

int mca_fs_lustre_module_init (ompio_file_t *file);
int mca_fs_lustre_module_finalize (ompio_file_t *file);

OMPI_DECLSPEC extern mca_fs_base_component_2_0_0_t mca_fs_lustre_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

int mca_fs_lustre_file_open (struct ompi_communicator_t *comm,
                             const char *filename,
                             int amode,
                             struct opal_info_t *info,
                             ompio_file_t *fh);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FS_LUSTRE_H */
