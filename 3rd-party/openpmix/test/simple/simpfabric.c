/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "src/include/pmix_config.h"
#include "include/pmix.h"
#include "include/pmix_server.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_types.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"

#include "simptest.h"

static pmix_server_module_t mymodule = {0};

typedef struct {
    mylock_t lock;
    pmix_status_t status;
    pmix_info_t *info;
    size_t ninfo;
} mycaddy_t;

static void local_cbfunc(pmix_status_t status, void *cbdata)
{
    mylock_t *lock = (mylock_t *) cbdata;
    lock->status = status;
    DEBUG_WAKEUP_THREAD(lock);
}

static void setup_cbfunc(pmix_status_t status, pmix_info_t info[], size_t ninfo,
                         void *provided_cbdata, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    mycaddy_t *mq = (mycaddy_t *) provided_cbdata;
    size_t n;
    PMIX_HIDE_UNUSED_PARAMS(status);

    /* transfer it to the caddy for return to the main thread */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(mq->info, ninfo);
        mq->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_XFER(&mq->info[n], &info[n]);
        }
    }

    /* let the library release the data and cleanup from
     * the operation */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, cbdata);
    }

    DEBUG_WAKEUP_THREAD(&mq->lock);
}

int main(int argc, char **argv)
{
    pmix_info_t *info, *iptr;
    pmix_status_t rc;
    size_t ninfo;
    int exit_code = 0;
    size_t n;
    char *hosts, *procs;
    char *regex, *ppn;
    mylock_t lock;
    mycaddy_t cd;
    pmix_value_t *val;
    pmix_topology_t *mytopo;
    pmix_proc_t myproc;
    pmix_cpuset_t mycpuset;
    pmix_device_distance_t *distances;
    size_t ndist;
    pmix_device_type_t type = PMIX_DEVTYPE_OPENFABRICS |
                              PMIX_DEVTYPE_NETWORK |
                              PMIX_DEVTYPE_COPROC |
                              PMIX_DEVTYPE_GPU;
    pmix_nspace_t ncache;
    PMIX_HIDE_UNUSED_PARAMS(argc, argv);

    /* smoke test */
    if (PMIX_SUCCESS != 0) {
        fprintf(stderr, "ERROR IN COMPUTING CONSTANTS: PMIX_SUCCESS = %d\n", PMIX_SUCCESS);
        exit(1);
    }

    fprintf(stderr, "PID: %lu Testing version %s\n", (unsigned long) getpid(), PMIx_Get_version());

    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_SERVER_SCHEDULER, NULL, PMIX_BOOL);
    if (PMIX_SUCCESS != (rc = PMIx_server_init(&mymodule, info, ninfo))) {
        fprintf(stderr, "Init failed with error %d\n", rc);
        return rc;
    }
    PMIX_INFO_FREE(info, ninfo);

    /* get my procID */
    fprintf(stderr, "Getting procID\n");
    rc = PMIx_Get(NULL, PMIX_PROCID, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Get of my procID failed: %s\n", PMIx_Error_string(rc));
        goto cleanup;
    }
    PMIX_LOAD_PROCID(&myproc, val->data.proc->nspace, val->data.proc->rank);
    PMIX_VALUE_FREE(val, 1);

    /* get my topology */
    fprintf(stderr, "GETTING TOPOLOGY\n");
    rc = PMIx_Get(&myproc, PMIX_TOPOLOGY2, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Get of my topology failed: %s\n", PMIx_Error_string(rc));
        goto cleanup;
    }
    mytopo = val->data.topo;
    val->data.topo = NULL;
    PMIX_VALUE_FREE(val, 1);
    fprintf(stderr, "Got my topology: Source = %s\n",
            (NULL == mytopo->source) ? "NULL" : mytopo->source);

    /* get my cpuset */
    fprintf(stderr, "GETTING CPUSET\n");
    PMIX_CPUSET_CONSTRUCT(&mycpuset);
    rc = PMIx_Get_cpuset(&mycpuset, PMIX_CPUBIND_PROCESS);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Get of my cpuset failed: %s\n", PMIx_Error_string(rc));
        goto cleanup;
    }
    PMIx_server_generate_cpuset_string(&mycpuset, &ppn);
    fprintf(stderr, "Got my cpuset: %s\n", ppn);
    free(ppn);

    ninfo = 1;
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_DEVICE_TYPE, &type, PMIX_DEVTYPE);
    rc = PMIx_Compute_distances(mytopo, &mycpuset, info, ninfo, &distances, &ndist);
    PMIX_INFO_FREE(info, ninfo);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Compute distances failed: %s\n", PMIx_Error_string(rc));
    } else {
        for (n = 0; n < ndist; n++) {
            fprintf(stderr, "Device[%d]: UUID %s OSname: %s Type %s MinDist %u MaxDist %u\n",
                    (int) n, distances[n].uuid, distances[n].osname,
                    PMIx_Device_type_string(distances[n].type), distances[n].mindist,
                    distances[n].maxdist);
        }
    }

    /* setup an application */
    PMIX_INFO_CREATE(iptr, 4);
    hosts = "test000,test001,test002";
    PMIx_generate_regex(hosts, &regex);
    PMIX_INFO_LOAD(&iptr[0], PMIX_NODE_MAP, regex, PMIX_REGEX);
    free(regex);

    procs = "0,1,2;3,4,5;6,7";
    PMIx_generate_ppn(procs, &ppn);
    PMIX_INFO_LOAD(&iptr[1], PMIX_PROC_MAP, ppn, PMIX_REGEX);
    free(ppn);

    PMIX_LOAD_KEY(iptr[2].key, PMIX_ALLOC_NETWORK);
    iptr[2].value.type = PMIX_DATA_ARRAY;
    PMIX_DATA_ARRAY_CREATE(iptr[2].value.data.darray, 2, PMIX_INFO);
    info = (pmix_info_t *) iptr[2].value.data.darray->array;
    PMIX_INFO_LOAD(&info[0], PMIX_ALLOC_NETWORK_ID, "SIMPSCHED.net", PMIX_STRING);
    PMIX_INFO_LOAD(&info[1], PMIX_ALLOC_NETWORK_SEC_KEY, NULL, PMIX_BOOL);

    PMIX_INFO_LOAD(&iptr[3], PMIX_SETUP_APP_ENVARS, NULL, PMIX_BOOL);
    PMIX_LOAD_NSPACE(ncache, "SIMPSCHED");
    DEBUG_CONSTRUCT_LOCK(&cd.lock);
    rc = PMIx_server_setup_application(ncache, iptr, 4, setup_cbfunc, &cd);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "[%s:%d] PMIx_server_setup_application failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(rc));
        DEBUG_DESTRUCT_LOCK(&cd.lock);
        goto cleanup;
    }
    DEBUG_WAIT_THREAD(&cd.lock);
    DEBUG_DESTRUCT_LOCK(&cd.lock);

    /* setup the local subsystem */
    DEBUG_CONSTRUCT_LOCK(&lock);
    rc = PMIx_server_setup_local_support(ncache, cd.info, cd.ninfo, local_cbfunc, &lock);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "[%s:%d] PMIx_server_setup_local_support failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(rc));
        DEBUG_DESTRUCT_LOCK(&lock);
        goto cleanup;
    }
    DEBUG_WAIT_THREAD(&lock);
    DEBUG_DESTRUCT_LOCK(&lock);

cleanup:
    if (PMIX_SUCCESS != rc) {
        exit_code = rc;
    }
    /* finalize the server library */
    if (PMIX_SUCCESS != (rc = PMIx_server_finalize())) {
        fprintf(stderr, "Finalize failed with error %d\n", rc);
        exit_code = rc;
    }

    if (0 == exit_code) {
        fprintf(stderr, "Test finished OK!\n");
    } else {
        fprintf(stderr, "TEST FAILED WITH ERROR %d\n", exit_code);
    }

    return exit_code;
}
