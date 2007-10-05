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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml_types.h"

#include "grpcomm_cnos.h"

#if OMPI_GRPCOMM_CNOS_HAVE_BARRIER
#include <catamount/cnos_mpi_os.h>
#endif

/* API functions */
static int xcast_nb(orte_jobid_t job,
                    orte_buffer_t *buffer,
                    orte_rml_tag_t tag);

static int xcast(orte_jobid_t job,
                 orte_buffer_t *buffer,
                 orte_rml_tag_t tag);

static int orte_grpcomm_cnos_barrier(void);

static int allgather(orte_buffer_t *sbuf, orte_buffer_t *rbuf);

static int allgather_list(opal_list_t *names, orte_buffer_t *sbuf, orte_buffer_t *rbuf);

orte_grpcomm_base_module_t orte_grpcomm_cnos_module = {
    xcast,
    xcast_nb,
    allgather,
    allgather_list,
    orte_grpcomm_cnos_barrier
};


/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Non-blocking version */
static int xcast_nb(orte_jobid_t job,
                    orte_buffer_t *buffer,
                    orte_rml_tag_t tag)
{
    return ORTE_SUCCESS;
}

/* Blocking version */
static int xcast(orte_jobid_t job,
                 orte_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    return ORTE_SUCCESS;
}

static int
orte_grpcomm_cnos_barrier(void)
{
#if OMPI_GRPCOMM_CNOS_HAVE_BARRIER
    cnos_barrier();
#endif

    return ORTE_SUCCESS;
}

static int allgather(orte_buffer_t *sbuf, orte_buffer_t *rbuf)
{
    int rc;
    orte_std_cntr_t zero=0;
    
    /* seed the outgoing buffer with num_procs=0 so it won't be unpacked */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(rbuf, &zero, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return rc;
}

static int allgather_list(opal_list_t *names, orte_buffer_t *sbuf, orte_buffer_t *rbuf)
{
    int rc;
    orte_std_cntr_t zero=0;
    
    /* seed the outgoing buffer with num_procs=0 so it won't be unpacked */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(rbuf, &zero, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return rc;
}
