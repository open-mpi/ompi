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

#ifndef _COMMON_SM_WINDOWS_H_
#define _COMMON_SM_WINDOWS_H_

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

typedef struct mca_common_sm_module_windows_t 
{
    mca_common_sm_module_t super;
} mca_common_sm_module_windows_t;

OBJ_CLASS_DECLARATION(mca_common_sm_module_windows_t);

/**
 *  This routine is used to set up a shared memory file, backed
 *  by a specified file.  It is assumed that the file does not
 *  exist before any of the current set of processes try and open
 *  it.
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
 *  @param size - size of the file, in bytes (IN)
 *
 *  @param file_name  name of file to be opened. (IN)
 *
 *  @param size_ctl_structure  size of the control structure at
 *                             the head of the file. The control structure
 *                             is assumed to have mca_common_sm_seg_header_t
 *                             as its first segment (IN)
 *
 *  @param data_set_alignment  alignment of the data segment.  this
 *                             follows the control structure.  If this 
 *                             value if 0, then assume that there will 
 *                             be no data segment following the control 
 *                             structure. (IN)
 *
 *  @return value pointer to control structure at head of file.
 */
OMPI_DECLSPEC extern mca_common_sm_module_t *
mca_common_sm_windows_init(ompi_proc_t **sorted_procs,
                           size_t num_local_procs,
                           size_t size, 
                           char *file_name,
                           size_t size_ctl_structure, 
                           size_t data_seg_alignment);

/*
 * Callback from the sm mpool
 */
OMPI_DECLSPEC extern void *
mca_common_sm_windows_seg_alloc(struct mca_mpool_base_module_t *mpool, 
                                size_t *size, 
                                mca_mpool_base_registration_t **registration);

/**
 * This function will release all local resources attached to the
 * mmapped file. We assume that the operating system will destroy the
 * file when the last process release it.
 *
 * @param sm_windows - the control structure at head of file.
 *
 * @returnvalue 0 if everything was OK, otherwise a negative value.
 */

OMPI_DECLSPEC extern int 
mca_common_sm_windows_fini(mca_common_sm_module_t *mca_common_sm_module);

/**
 * component query routine
 */
OMPI_DECLSPEC extern int 
mca_common_sm_windows_component_query(void);

END_C_DECLS

#endif

