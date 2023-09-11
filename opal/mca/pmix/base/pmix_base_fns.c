/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2022 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <regex.h>

#include <string.h>
#include <time.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "opal/class/opal_pointer_array.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal_stdint.h"

#include "opal/mca/base/mca_base_vari.h"
#include "opal/mca/pmix/base/base.h"

#include "src/include/pmix_frameworks.h"

int opal_pmix_base_exchange(pmix_info_t *indat, pmix_pdata_t *outdat, int timeout)
{
    pmix_status_t rc;
    pmix_info_t info[2];
    pmix_persistence_t firstread = PMIX_PERSIST_FIRST_READ;

    /* publish the provided value - it defaults to
     * "session" range, but we will add a persistence
     * to delete it upon first read */
    PMIX_INFO_XFER(&info[0], indat);
    PMIX_INFO_LOAD(&info[1], PMIX_PERSISTENCE, &firstread, PMIX_PERSIST);
    /* publish it with "session" scope */
    rc = PMIx_Publish(info, 2);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);
    if (PMIX_SUCCESS != rc) {
        return opal_pmix_convert_status(rc);
    }

    /* lookup the other side's info */
    /* tell it to wait for the data to arrive */
    PMIX_INFO_LOAD(&info[0], PMIX_WAIT, NULL, PMIX_BOOL);
    /* pass along the given timeout as we don't know when
     * the other side will publish - it doesn't
     * have to be simultaneous */
    if (0 < timeout) {
        PMIX_INFO_LOAD(&info[1], PMIX_TIMEOUT, &timeout, PMIX_INT);
    } else {
        PMIX_INFO_LOAD(&info[1], PMIX_TIMEOUT, &opal_pmix_base.timeout, PMIX_INT);
    }
    rc = PMIx_Lookup(outdat, 1, info, 2);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);

    return opal_pmix_convert_status(rc);
}

static bool check_pmix_param(char *param)
{
    char *p;
    size_t n;
    int len;

    p = strchr(param, '_');
    len = (int)(p - param);

    if (0 == strncmp(param, "pmix", len)) {
        return true;
    }
    for (n=0; NULL != pmix_framework_names[n]; n++) {
        if (0 == strncmp(param, pmix_framework_names[n], len)) {
            return true;
        }
    }
    return false;
}

static bool check_pmix_overlap(char *var, char *value)
{
    char *tmp;

    if (0 == strncmp(var, "dl_", 3)) {
        opal_asprintf(&tmp, "PMIX_MCA_pdl_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "oob_", 4)) {
        opal_asprintf(&tmp, "PMIX_MCA_ptl_%s", &var[4]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "hwloc_", 6)) {
        opal_asprintf(&tmp, "PMIX_MCA_%s", var);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    } else if (0 == strncmp(var, "if_", 3)) {
        // need to convert if to pif
        opal_asprintf(&tmp, "PMIX_MCA_pif_%s", &var[3]);
        // set it, but don't overwrite if they already
        // have a value in our environment
        setenv(tmp, value, false);
        free(tmp);
        return true;
    }
    return false;
}

// NOTE: This code is fundamentally the same (module PMIX <-> OPAL)
//      as the translate_params() routine in the PRRTE repo's
//      src/mca/schizo/ompi/schizo_ompi.c file.  If there are
//      changes here, there are likely to be changes there.
static void translate_params(void)
{
    char *evar, *tmp, *e2;
    char *file;
    const char *home;
    opal_list_t params;
    mca_base_var_file_value_t *fv;
    bool pmix_overlap;
    int n, len;

    /* Since we are direct launched, we need to check the OMPI default
     * MCA params to see if there is something relating to PRRTE
     * in them - this would be "old" references to things from
     * ORTE, as well as a few OPAL references that also impact us
     *
     * NOTE: we do this in the following precedence order. Note
     * that we do not overwrite at any step - this is so that we
     * don't overwrite something previously set by the user. So
     * the order to execution is the opposite of the intended
     * precedence order.
     *
     * 1. check the environmental paramaters for OMPI_MCA values
     *    that need to be translated
     *
     * 2. the user's home directory file as it should
     *    overwrite the system default file, but not the
     *    envars
     *
     * 3. the system default parameter file
     */
    len = strlen("OMPI_MCA_");
    for (n=0; NULL != environ[n]; n++) {
        if (0 == strncmp(environ[n], "OMPI_MCA_", len)) {
            e2 = strdup(environ[n]);
            evar = strrchr(e2, '=');
            *evar = '\0';
            ++evar;
            pmix_overlap = check_pmix_overlap(&e2[len], evar);
            if (!pmix_overlap && check_pmix_param(&e2[len])) {
                opal_asprintf(&tmp, "PMIX_MCA_%s", &e2[len]);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, evar, false);
                free(tmp);
            }
            free(e2);
        }
    }

    /* try to get user's home directory */
    home = opal_home_directory();
    if (NULL != home) {
        file = opal_os_path(false, home, ".openmpi", "mca-params.conf", NULL);
        OBJ_CONSTRUCT(&params, opal_list_t);
        mca_base_parse_paramfile(file, &params);
        free(file);
        OPAL_LIST_FOREACH (fv, &params, mca_base_var_file_value_t) {
            pmix_overlap = check_pmix_overlap(&e2[len], evar);
            if (!pmix_overlap && check_pmix_param(fv->mbvfv_var)) {
                opal_asprintf(&tmp, "PMIX_MCA_%s", fv->mbvfv_var);
                // set it, but don't overwrite if they already
                // have a value in our environment
                setenv(tmp, fv->mbvfv_value, false);
                free(tmp);
            }
        }
        OPAL_LIST_DESTRUCT(&params);
    }

    /* check if the user has set OMPIHOME in their environment */
    if (NULL != (evar = getenv("OMPIHOME"))) {
        /* look for the default MCA param file */
        file = opal_os_path(false, evar, "etc", "openmpi-mca-params.conf", NULL);
        OBJ_CONSTRUCT(&params, opal_list_t);
        mca_base_parse_paramfile(file, &params);
        free(file);
        OPAL_LIST_FOREACH (fv, &params, mca_base_var_file_value_t) {
            check_pmix_overlap(fv->mbvfv_var, fv->mbvfv_value);
        }
        OPAL_LIST_DESTRUCT(&params);
    }
}

typedef struct {
    opal_list_item_t super;
    pmix_nspace_t nspace;
    opal_jobid_t jobid;
} opal_nptr_t;
static OBJ_CLASS_INSTANCE(opal_nptr_t, opal_list_item_t, NULL, NULL);

static opal_list_t localnspaces;

void opal_pmix_setup_nspace_tracker(void)
{
    /* check if we were launched by PRRTE */
    if (NULL != getenv("PRTE_LAUNCHED")) {
        opal_process_info.nativelaunch = true;
    } else {
        // When direct launched, translate MCA parameters from older releases
        // into newer versions here, since PRRTE isn't involved.  (When
        // natively launched, PRRTE will already have translated the params)
        translate_params();
    }

    OBJ_CONSTRUCT(&localnspaces, opal_list_t);
}

void opal_pmix_finalize_nspace_tracker(void)
{
    OPAL_LIST_DESTRUCT(&localnspaces);
}

int opal_pmix_convert_jobid(pmix_nspace_t nspace, opal_jobid_t jobid)
{
    opal_nptr_t *nptr;

    /* zero out the nspace */
    PMIX_LOAD_NSPACE(nspace, NULL);

    /* cycle across our list of known jobids */
    OPAL_LIST_FOREACH (nptr, &localnspaces, opal_nptr_t) {
        if (jobid == nptr->jobid) {
            PMIX_LOAD_NSPACE(nspace, nptr->nspace);
            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_NOT_FOUND;
}

int opal_pmix_convert_nspace(opal_jobid_t *jobid, pmix_nspace_t nspace)
{
    opal_nptr_t *nptr;
    opal_jobid_t jid;
    uint16_t jobfam;
    uint32_t hash32, localjob = 0;
    char *p = NULL;

    /* set a default */
    if (NULL != jobid) {
        *jobid = OPAL_JOBID_WILDCARD;
    }

    /* if the nspace is empty, there is nothing more to do */
    if (0 == strlen(nspace)) {
        return OPAL_SUCCESS;
    }

    /* cycle across our list of known nspace's */
    OPAL_LIST_FOREACH (nptr, &localnspaces, opal_nptr_t) {
        if (PMIX_CHECK_NSPACE(nspace, nptr->nspace)) {
            if (NULL != jobid) {
                *jobid = nptr->jobid;
            }
            return OPAL_SUCCESS;
        }
    }

    /* if we get here, we don't know this nspace */
    /* find the "." at the end that indicates the child job */
    if (NULL != (p = strrchr(nspace, '@'))) {
        *p = '\0';
    }
    OPAL_HASH_STR(nspace, hash32);
    if (NULL != p) {
        *p = '@';
        ++p;
        localjob = strtoul(p, NULL, 10);
    }

    /* now compress to 16-bits */
    jobfam = (uint16_t)(((0x0000ffff & (0xffff0000 & hash32) >> 16)) ^ (0x0000ffff & hash32));
    jid = (0xffff0000 & ((uint32_t) jobfam << 16)) | (0x0000ffff & localjob);
    if (NULL != jobid) {
        *jobid = jid;
    }
    /* save this jobid/nspace pair */
    nptr = OBJ_NEW(opal_nptr_t);
    nptr->jobid = jid;
    PMIX_LOAD_NSPACE(nptr->nspace, nspace);
    opal_list_append(&localnspaces, &nptr->super);

    return OPAL_SUCCESS;
}

pmix_status_t opal_pmix_convert_rc(int rc)
{
    switch (rc) {
    case OPAL_ERR_DEBUGGER_RELEASE:
        return PMIX_ERR_DEBUGGER_RELEASE;

    case OPAL_ERR_HANDLERS_COMPLETE:
        return PMIX_EVENT_ACTION_COMPLETE;

    case OPAL_ERR_PROC_ABORTED:
        return PMIX_ERR_PROC_ABORTED;

    case OPAL_ERR_PROC_REQUESTED_ABORT:
        return PMIX_ERR_PROC_REQUESTED_ABORT;

    case OPAL_ERR_PROC_ABORTING:
        return PMIX_ERR_PROC_ABORTING;

    case OPAL_ERR_NODE_DOWN:
        return PMIX_ERR_NODE_DOWN;

    case OPAL_ERR_NODE_OFFLINE:
        return PMIX_ERR_NODE_OFFLINE;

    case OPAL_ERR_JOB_TERMINATED:
        return PMIX_ERR_JOB_TERMINATED;

    case OPAL_ERR_PROC_RESTART:
        return PMIX_ERR_PROC_RESTART;

    case OPAL_ERR_PROC_CHECKPOINT:
        return PMIX_ERR_PROC_CHECKPOINT;

    case OPAL_ERR_PROC_MIGRATE:
        return PMIX_ERR_PROC_MIGRATE;

    case OPAL_ERR_EVENT_REGISTRATION:
        return PMIX_ERR_EVENT_REGISTRATION;

    case OPAL_ERR_NOT_IMPLEMENTED:
    case OPAL_ERR_NOT_SUPPORTED:
        return PMIX_ERR_NOT_SUPPORTED;

    case OPAL_ERR_NOT_FOUND:
        return PMIX_ERR_NOT_FOUND;

    case OPAL_ERR_PERM:
    case OPAL_ERR_UNREACH:
    case OPAL_ERR_SERVER_NOT_AVAIL:
        return PMIX_ERR_UNREACH;

    case OPAL_ERR_BAD_PARAM:
        return PMIX_ERR_BAD_PARAM;

    case OPAL_ERR_OUT_OF_RESOURCE:
        return PMIX_ERR_OUT_OF_RESOURCE;

    case OPAL_ERR_DATA_VALUE_NOT_FOUND:
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;

    case OPAL_ERR_TIMEOUT:
        return PMIX_ERR_TIMEOUT;

    case OPAL_ERR_WOULD_BLOCK:
        return PMIX_ERR_WOULD_BLOCK;

    case OPAL_EXISTS:
        return PMIX_EXISTS;

    case OPAL_ERR_PARTIAL_SUCCESS:
        return PMIX_QUERY_PARTIAL_SUCCESS;

    case OPAL_ERR_MODEL_DECLARED:
        return PMIX_MODEL_DECLARED;

    case OPAL_ERROR:
        return PMIX_ERROR;
    case OPAL_SUCCESS:
        return PMIX_SUCCESS;
    default:
        return rc;
    }
}

int opal_pmix_convert_status(pmix_status_t status)
{
    switch (status) {
    case PMIX_ERR_DEBUGGER_RELEASE:
        return OPAL_ERR_DEBUGGER_RELEASE;

    case PMIX_EVENT_ACTION_COMPLETE:
        return OPAL_ERR_HANDLERS_COMPLETE;

    case PMIX_ERR_PROC_ABORTED:
        return OPAL_ERR_PROC_ABORTED;

    case PMIX_ERR_PROC_REQUESTED_ABORT:
        return OPAL_ERR_PROC_REQUESTED_ABORT;

    case PMIX_ERR_PROC_ABORTING:
        return OPAL_ERR_PROC_ABORTING;

    case PMIX_ERR_NODE_DOWN:
        return OPAL_ERR_NODE_DOWN;

    case PMIX_ERR_NODE_OFFLINE:
        return OPAL_ERR_NODE_OFFLINE;

    case PMIX_ERR_JOB_TERMINATED:
        return OPAL_ERR_JOB_TERMINATED;

    case PMIX_ERR_PROC_RESTART:
        return OPAL_ERR_PROC_RESTART;

    case PMIX_ERR_PROC_CHECKPOINT:
        return OPAL_ERR_PROC_CHECKPOINT;

    case PMIX_ERR_PROC_MIGRATE:
        return OPAL_ERR_PROC_MIGRATE;

    case PMIX_ERR_EVENT_REGISTRATION:
        return OPAL_ERR_EVENT_REGISTRATION;

    case PMIX_ERR_NOT_SUPPORTED:
        return OPAL_ERR_NOT_SUPPORTED;

    case PMIX_ERR_NOT_FOUND:
        return OPAL_ERR_NOT_FOUND;

    case PMIX_ERR_OUT_OF_RESOURCE:
        return OPAL_ERR_OUT_OF_RESOURCE;

    case PMIX_ERR_INIT:
        return OPAL_ERROR;

    case PMIX_ERR_BAD_PARAM:
        return OPAL_ERR_BAD_PARAM;

    case PMIX_ERR_UNREACH:
    case PMIX_ERR_NO_PERMISSIONS:
        return OPAL_ERR_UNREACH;

    case PMIX_ERR_TIMEOUT:
        return OPAL_ERR_TIMEOUT;

    case PMIX_ERR_WOULD_BLOCK:
        return OPAL_ERR_WOULD_BLOCK;

    case PMIX_ERR_LOST_CONNECTION_TO_SERVER:
    case PMIX_ERR_LOST_PEER_CONNECTION:
    case PMIX_ERR_LOST_CONNECTION_TO_CLIENT:
        return OPAL_ERR_COMM_FAILURE;

    case PMIX_EXISTS:
        return OPAL_EXISTS;

    case PMIX_QUERY_PARTIAL_SUCCESS:
        return OPAL_ERR_PARTIAL_SUCCESS;

    case PMIX_MONITOR_HEARTBEAT_ALERT:
        return OPAL_ERR_HEARTBEAT_ALERT;

    case PMIX_MONITOR_FILE_ALERT:
        return OPAL_ERR_FILE_ALERT;

    case PMIX_MODEL_DECLARED:
        return OPAL_ERR_MODEL_DECLARED;

    case PMIX_ERROR:
        return OPAL_ERROR;
    case PMIX_SUCCESS:
        return OPAL_SUCCESS;
    default:
        return status;
    }
}

pmix_proc_state_t opal_pmix_convert_state(int state)
{
    switch (state) {
    case 0:
        return PMIX_PROC_STATE_UNDEF;
    case 1:
        return PMIX_PROC_STATE_LAUNCH_UNDERWAY;
    case 2:
        return PMIX_PROC_STATE_RESTART;
    case 3:
        return PMIX_PROC_STATE_TERMINATE;
    case 4:
        return PMIX_PROC_STATE_RUNNING;
    case 5:
        return PMIX_PROC_STATE_CONNECTED;
    case 51:
        return PMIX_PROC_STATE_KILLED_BY_CMD;
    case 52:
        return PMIX_PROC_STATE_ABORTED;
    case 53:
        return PMIX_PROC_STATE_FAILED_TO_START;
    case 54:
        return PMIX_PROC_STATE_ABORTED_BY_SIG;
    case 55:
        return PMIX_PROC_STATE_TERM_WO_SYNC;
    case 56:
        return PMIX_PROC_STATE_COMM_FAILED;
    case 58:
        return PMIX_PROC_STATE_CALLED_ABORT;
    case 59:
        return PMIX_PROC_STATE_MIGRATING;
    case 61:
        return PMIX_PROC_STATE_CANNOT_RESTART;
    case 62:
        return PMIX_PROC_STATE_TERM_NON_ZERO;
    case 63:
        return PMIX_PROC_STATE_FAILED_TO_LAUNCH;
    default:
        return PMIX_PROC_STATE_UNDEF;
    }
}

int opal_pmix_convert_pstate(pmix_proc_state_t state)
{
    switch (state) {
    case PMIX_PROC_STATE_UNDEF:
        return 0;
    case PMIX_PROC_STATE_PREPPED:
    case PMIX_PROC_STATE_LAUNCH_UNDERWAY:
        return 1;
    case PMIX_PROC_STATE_RESTART:
        return 2;
    case PMIX_PROC_STATE_TERMINATE:
        return 3;
    case PMIX_PROC_STATE_RUNNING:
        return 4;
    case PMIX_PROC_STATE_CONNECTED:
        return 5;
    case PMIX_PROC_STATE_UNTERMINATED:
        return 15;
    case PMIX_PROC_STATE_TERMINATED:
        return 20;
    case PMIX_PROC_STATE_KILLED_BY_CMD:
        return 51;
    case PMIX_PROC_STATE_ABORTED:
        return 52;
    case PMIX_PROC_STATE_FAILED_TO_START:
        return 53;
    case PMIX_PROC_STATE_ABORTED_BY_SIG:
        return 54;
    case PMIX_PROC_STATE_TERM_WO_SYNC:
        return 55;
    case PMIX_PROC_STATE_COMM_FAILED:
        return 56;
    case PMIX_PROC_STATE_CALLED_ABORT:
        return 58;
    case PMIX_PROC_STATE_MIGRATING:
        return 60;
    case PMIX_PROC_STATE_CANNOT_RESTART:
        return 61;
    case PMIX_PROC_STATE_TERM_NON_ZERO:
        return 62;
    case PMIX_PROC_STATE_FAILED_TO_LAUNCH:
        return 63;
    default:
        return 0; // undef
    }
}

#if PMIX_NUMERIC_VERSION >= 0x00030000

static void cleanup_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                           pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    opal_pmix_lock_t *lk = (opal_pmix_lock_t *) cbdata;

    OPAL_POST_OBJECT(lk);

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }

    /* release the block */
    lk->status = status;
    OPAL_PMIX_WAKEUP_THREAD(lk);
}
#endif

int opal_pmix_register_cleanup(char *path, bool directory, bool ignore, bool jobscope)
{
#if PMIX_NUMERIC_VERSION >= 0x00030000
    opal_pmix_lock_t lk;
    pmix_info_t pinfo[3];
    size_t n, ninfo = 0;
    pmix_status_t rc, ret;
    pmix_proc_t proc;

    OPAL_PMIX_CONSTRUCT_LOCK(&lk);

    if (ignore) {
        /* they want this path ignored */
        PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_CLEANUP_IGNORE, path, PMIX_STRING);
        ++ninfo;
    } else {
        if (directory) {
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_REGISTER_CLEANUP_DIR, path, PMIX_STRING);
            ++ninfo;
            /* recursively cleanup directories */
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_CLEANUP_RECURSIVE, NULL, PMIX_BOOL);
            ++ninfo;
        } else {
            /* order cleanup of the provided path */
            PMIX_INFO_LOAD(&pinfo[ninfo], PMIX_REGISTER_CLEANUP, path, PMIX_STRING);
            ++ninfo;
        }
    }

    /* if they want this applied to the job, then indicate so */
    if (jobscope) {
        rc = PMIx_Job_control_nb(NULL, 0, pinfo, ninfo, cleanup_cbfunc, (void *) &lk);
    } else {
        /* only applies to us */
        pmix_nspace_t nspace;
        if(OPAL_SUCCESS == opal_pmix_convert_jobid(nspace, OPAL_PROC_MY_NAME.jobid)) {
          (void) snprintf(proc.nspace, PMIX_MAX_NSLEN, "%s", nspace);
        }
        else {
          (void) snprintf(proc.nspace, PMIX_MAX_NSLEN, "%s", OPAL_JOBID_PRINT(OPAL_PROC_MY_NAME.jobid));
        }
        proc.rank = OPAL_PROC_MY_NAME.vpid;
        rc = PMIx_Job_control_nb(&proc, 1, pinfo, ninfo, cleanup_cbfunc, (void *) &lk);
    }
    if (PMIX_SUCCESS != rc) {
        ret = rc;
    } else {
#    if PMIX_VERSION_MAJOR == 3 && PMIX_VERSION_MINOR == 0 && PMIX_VERSION_RELEASE < 3
        /* There is a bug in PMIx 3.0.0 up to 3.0.2 that causes the callback never
         * being called, so assumes the everything went well and avoid a deadlock. */
        cleanup_cbfunc(PMIX_SUCCESS, NULL, 0, (void *) &lk, NULL, NULL);
#    endif
        OPAL_PMIX_WAIT_THREAD(&lk);
        ret = lk.status;
    }
    OPAL_PMIX_DESTRUCT_LOCK(&lk);
    for (n = 0; n < ninfo; n++) {
        PMIX_INFO_DESTRUCT(&pinfo[n]);
    }
    return ret;
#else
    return OPAL_SUCCESS;
#endif
}

/* CLASS INSTANTIATIONS */
static void dsicon(opal_ds_info_t *p)
{
    PMIX_PROC_CONSTRUCT(&p->source);
    p->info = NULL;
#if PMIX_NUMERIC_VERSION >= 0x00030000
    p->persistence = PMIX_PERSIST_INVALID;
#else
    p->persistence = PMIX_PERSIST_INDEF;
#endif
}
OBJ_CLASS_INSTANCE(opal_ds_info_t, opal_list_item_t, dsicon, NULL);

static void infoitmcon(opal_info_item_t *p)
{
    PMIX_INFO_CONSTRUCT(&p->info);
}
static void infoitdecon(opal_info_item_t *p)
{
    PMIX_INFO_DESTRUCT(&p->info);
}
OBJ_CLASS_INSTANCE(opal_info_item_t, opal_list_item_t, infoitmcon, infoitdecon);

OBJ_CLASS_INSTANCE(opal_proclist_t, opal_list_item_t, NULL, NULL);
