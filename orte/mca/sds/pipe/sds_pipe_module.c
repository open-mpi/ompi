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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/sds/sds.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/sds/pipe/sds_pipe.h"

orte_sds_base_module_t orte_sds_pipe_module =  {
    orte_sds_base_basic_contact_universe,
    orte_sds_pipe_set_name,
    orte_sds_pipe_finalize,
};


int
orte_sds_pipe_set_name(void)
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
    if(ORTE_SUCCESS != (rc = orte_dss.copy((void**)&orte_process_info.my_name, &name, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = read(fd,&orte_process_info.vpid_start, sizeof(orte_process_info.vpid_start));
    if(rc != sizeof(orte_process_info.vpid_start)) {
        opal_output(0, "orte_ns_nds_pipe_get: read returned %d, errno=%d\n", rc, errno);
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    rc = read(fd,&num_procs, sizeof(num_procs));
    if(rc != sizeof(num_procs)) {
        opal_output(0, "orte_ns_nds_pipe_get: read returned %d, errno=%d\n", rc, errno);
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    orte_process_info.num_procs = (orte_std_cntr_t)num_procs;

    close(fd);
    return ORTE_SUCCESS;
}


int 
orte_sds_pipe_finalize(void)
{
    return ORTE_SUCCESS;
}
