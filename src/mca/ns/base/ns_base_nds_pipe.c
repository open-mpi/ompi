/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#include "orte_config.h"
#include <unistd.h>
#include <stdlib.h>

#include "include/orte_constants.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/base/base.h"
#include "mca/ns/base/ns_base_nds.h"


int orte_ns_nds_pipe_get(void)
{
    int rc, fd, id;
    orte_process_name_t name;
    size_t num_procs;

    /* lookup the fd to use */
    id = mca_base_param_register_int("nds","pipe","fd", NULL, 3);
    mca_base_param_lookup_int(id, &fd);

    rc = read(fd,&name,sizeof(name));
    if(rc != sizeof(name)) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if(ORTE_SUCCESS != (rc = orte_ns.copy_process_name(&orte_process_info.my_name, &name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = read(fd,&orte_process_info.vpid_start, sizeof(orte_process_info.vpid_start));
    if(rc != sizeof(orte_process_info.vpid_start)) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = read(fd,&num_procs, sizeof(num_procs));
    if(rc != sizeof(orte_process_info.num_procs)) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = num_procs;

    close(fd);
    return ORTE_SUCCESS;
}


int orte_ns_nds_pipe_put(const orte_process_name_t* name, orte_vpid_t vpid_start, size_t num_procs, int fd)
{
    int rc;

    rc = write(fd,name,sizeof(orte_process_name_t));
    if(rc != sizeof(orte_process_name_t)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = write(fd,&vpid_start, sizeof(vpid_start));
    if(rc != sizeof(vpid_start)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = write(fd,&num_procs, sizeof(num_procs));
    if(rc != sizeof(num_procs)) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
}

