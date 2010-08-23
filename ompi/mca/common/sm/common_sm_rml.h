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
 * Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _COMMON_SM_RML_H_
#define _COMMON_SM_RML_H_

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/common/sm/common_sm.h"

#define MCA_COMMON_SM_RML_MSG_LEN      2

BEGIN_C_DECLS

/**
 * shared memory information used for initialization and setup.
 */
typedef struct mca_common_sm_rml_sm_info_t
{
    char posix_fname_buff[OMPI_COMMON_SM_POSIX_FILE_LEN_MAX];
    int id;
} mca_common_sm_rml_sm_info_t;

/**
 * items on the pending_rml_msgs list
 */
typedef struct mca_common_sm_rml_pending_rml_msg_types_t
{
    opal_list_item_t super;
    char rml_file_name[OPAL_PATH_MAX];
    char posix_fname_buff[OMPI_COMMON_SM_POSIX_FILE_LEN_MAX];
    int id;
} mca_common_sm_rml_pending_rml_msg_types_t;

/**
 * routine used to broadcast common sm initialization information to all local
 * processes in procs.
 */
OMPI_DECLSPEC extern int
mca_common_sm_rml_info_bcast(mca_common_sm_rml_sm_info_t *sm_info,
                             ompi_proc_t **procs,
                             size_t num_procs,
                             int tag,
                             bool bcast_root,
                             const char *file_name,
                             opal_list_t *pending_rml_msgs);

END_C_DECLS

#endif /* _COMMON_SM_RML_H_*/

