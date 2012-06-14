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
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/dss/dss.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml_types.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "grpcomm_cnos.h"

#if OMPI_GRPCOMM_CNOS_HAVE_BARRIER
#  if defined(HAVE_CNOS_MPI_OS_H)
#    include "cnos_mpi_os.h"
#  elif defined(HAVE_CATAMOUNT_CNOS_MPI_OS_H)
#    include "catamount/cnos_mpi_os.h"
#  endif
#endif

/* API functions */
static int init(void);
static void finalize(void);

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);

static int orte_grpcomm_cnos_barrier(orte_grpcomm_collective_t *coll);

static int allgather(orte_grpcomm_collective_t *coll);

static int set_proc_attr(const char* project,
                         const char* framework,
                         const char *attr_name,
                         const void *data,
                         size_t size);

static int get_proc_attr(const orte_process_name_t proc,
                         const char* project,
                         const char* framework,
                         const char * attribute_name, void **val, 
                         size_t *size);

static int modex(orte_grpcomm_collective_t *coll);

static int purge_proc_attrs(void);

orte_grpcomm_base_module_t orte_grpcomm_cnos_module = {
    init,
    finalize,
    xcast,
    allgather,
    orte_grpcomm_cnos_barrier,
    set_proc_attr,
    get_proc_attr,
    modex,
    purge_proc_attrs
};

/**
 * Init the module
 */
static int init(void)
{
    return ORTE_SUCCESS;
}

/**
 * Finalize module
 */
static void finalize(void)
{
    return;
}


/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

/* Blocking version */
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    return ORTE_SUCCESS;
}

static int
orte_grpcomm_cnos_barrier(orte_grpcomm_collective_t *coll)
{
#if OMPI_GRPCOMM_CNOS_HAVE_BARRIER
    cnos_barrier();
#endif
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return ORTE_SUCCESS;
}

static int allgather(orte_grpcomm_collective_t *coll)
{
    int rc;
    orte_std_cntr_t zero=0;
    opal_buffer_t rbuf;

    coll->active = false;
    if (NULL != coll->cbfunc) {
        /* seed the outgoing buffer with num_procs=0 so it won't be unpacked */
        OBJ_CONSTRUCT(&rbuf, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&rbuf, &zero, 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&rbuf);
            return rc;
        }
        coll->cbfunc(&rbuf, coll->cbdata);
        OBJ_DESTRUCT(&rbuf);
    }
    return rc;
}

static int purge_proc_attrs(void);

static int set_proc_attr(const char* project,
                         const char* framework,
                         const char *attr_name,
                         const void *data,
                         size_t size)
{
    return ORTE_SUCCESS;
}

static int get_proc_attr(const orte_process_name_t proc,
                         const char* project,
                         const char* framework,
                         const char * attribute_name, void **val, 
                         size_t *size)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int modex(orte_grpcomm_collective_t *coll)
{
    modex->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return ORTE_SUCCESS;
}

static int purge_proc_attrs(void)
{
    return ORTE_SUCCESS;
}

