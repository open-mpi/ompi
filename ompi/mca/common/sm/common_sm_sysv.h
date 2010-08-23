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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_SM_SYSV_H_
#define _COMMON_SM_SYSV_H_

#include "ompi_config.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/proc/proc.h"
#include "ompi/group/group.h"
#include "ompi/mca/common/sm/common_sm.h"

BEGIN_C_DECLS

struct mca_mpool_base_module_t;

typedef struct mca_common_sm_module_sysv_t
{
    mca_common_sm_module_t super;
} mca_common_sm_module_sysv_t;

OBJ_CLASS_DECLARATION(mca_common_sm_module_sysv_t);

/**
 * This routine is used to set up a System V shared memory segment.
 *
 * @param sorted_procs - array of (ompi_proc_t *)'s to create this shared memory
 *                       segment for. this routine, unlike the top-level
 *                       mca_common_sm_init routine, assumes that sorted_procs
 *                       is in the following state: all the local procs at the
 *                       beginning; sorted_procs[0] is the lowest named process.
 *                       (IN)
 *
 * @param num_local_procs - number of local procs contained within
 *                          sorted_procs (IN)
 *
 * @param size - size of the shared memory segment, in bytes (IN)
 *
 * @param file_name - strictly used for RML message identification/queueing (IN)
 *
 * @param size_ctl_structure  size of the control structure at
 *                            the head of the file. the control structure
 *                            is assumed to have mca_common_sm_seg_header_t
 *                            as its first segment (IN)
 *
 * @param data_set_alignment  alignment of the data segment.  This
 *                            follows the control structure.  If this
 *                            value if 0, then assume that there will
 *                            be no data segment following the control
 *                            structure. (IN)
 *
 * @return value pointer to control structure at head of file.
 */
OMPI_DECLSPEC extern mca_common_sm_module_t *
mca_common_sm_sysv_init(ompi_proc_t **sorted_procs,
                        size_t num_local_procs,
                        size_t size,
                        char *file_name,
                        size_t size_ctl_structure,
                        size_t data_seg_alignment);

/**
 * Callback from the sm mpool
 */
OMPI_DECLSPEC extern void *
mca_common_sm_sysv_seg_alloc(struct mca_mpool_base_module_t *mpool,
                             size_t *size,
                             mca_mpool_base_registration_t **registration);

/**
 * This function will release all local resources attached to the
 * shared memory segment. We assume that the operating system will destroy the
 * shared memory segment when the last process detaches from it.
 *
 * It is assumed that the operating system's System V IPC implementation
 * supports the following IPC_RMID semantics.
 *
 * Calling shmctl(shmid, IPC_RMID, ...) will actually destroy the shared memory
 * segment *after* the last process detaches from it (i.e., when the shm_nattch
 * member of the associated structure shmid_ds is zero).  This behavior is
 * important because we rely on it to release all allocated shared memory
 * segments upon job termination - including abnormal job termination.
 *
 * @param mca_common_sm_module - the control structure at head of the segment.
 *
 * @returnvalue 0 if everything was OK, otherwise a negative value.
 */

OMPI_DECLSPEC extern int
mca_common_sm_sysv_fini(mca_common_sm_module_t *mca_common_sm_module);

/**
 * component query routine
 */

OMPI_DECLSPEC extern int
mca_common_sm_sysv_component_query(void);

END_C_DECLS

#endif /* _COMMON_SM_SYSV_H_ */

