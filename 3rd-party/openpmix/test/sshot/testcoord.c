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
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include <getopt.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <pmix_server.h>

#include "src/class/pmix_object.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    volatile bool active;
    pmix_status_t status;
} mylock_t;

#define DEBUG_CONSTRUCT_LOCK(l)                \
    do {                                       \
        pthread_mutex_init(&(l)->mutex, NULL); \
        pthread_cond_init(&(l)->cond, NULL);   \
        (l)->active = true;                    \
        (l)->status = PMIX_SUCCESS;            \
    } while (0)

#define DEBUG_DESTRUCT_LOCK(l)              \
    do {                                    \
        pthread_mutex_destroy(&(l)->mutex); \
        pthread_cond_destroy(&(l)->cond);   \
    } while (0)

#define DEBUG_WAIT_THREAD(lck)                              \
    do {                                                    \
        pthread_mutex_lock(&(lck)->mutex);                  \
        while ((lck)->active) {                             \
            pthread_cond_wait(&(lck)->cond, &(lck)->mutex); \
        }                                                   \
        pthread_mutex_unlock(&(lck)->mutex);                \
    } while (0)

#define DEBUG_WAKEUP_THREAD(lck)              \
    do {                                      \
        pthread_mutex_lock(&(lck)->mutex);    \
        (lck)->active = false;                \
        pthread_cond_broadcast(&(lck)->cond); \
        pthread_mutex_unlock(&(lck)->mutex);  \
    } while (0)

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

static int help = 0;

int main(int argc, char **argv)
{
    pmix_status_t rc;
    mycaddy_t cd;
    mylock_t lock;
    pmix_proc_t proc;
    int tclass;
    pmix_value_t *val;
    pmix_geometry_t *geos;
    size_t ngeos, n, m;
    char *tmp;
    pmix_info_t info[2];
    static struct option myoptions[] = {{"num_nodes", required_argument, NULL, 'n'},
                                        {"num_devs", required_argument, NULL, 'd'},
                                        {"ppn", required_argument, NULL, 'p'},
                                        {"help", no_argument, &help, 1},
                                        {0, 0, 0, 0}};
    int option_index;
    int opt;
    int nnodes = 2;
    int ndevs = 8;
    int ppn = 4;

    while ((opt = getopt_long(argc, argv, "n:d:p:", myoptions, &option_index)) != -1) {
        switch (opt) {
        case 'n':
            nnodes = strtol(optarg, NULL, 10);
            break;
        case 'd':
            ndevs = strtol(optarg, NULL, 10);
            break;
        case 'p':
            ppn = strtol(optarg, NULL, 10);
            break;
        default:
            fprintf(stderr,
                    "Usage: %s\n    Options:\n"
                    "        [-n] [number of nodes]\n"
                    "        [-d] [number of devices/node]\n"
                    "        [-p] [number of procs/node]\n",
                    argv[0]);
            exit(1);
        }
    }
    if (help) {
        fprintf(stderr,
                "Usage: %s\n    Options:\n"
                "        [-n] [number of nodes]\n"
                "        [-d] [number of devices/node]\n"
                "        [-p] [number of procs/node]\n",
                argv[0]);
        exit(0);
    }

    pmix_asprintf(&tmp, "PMIX_MCA_pnet_sshot_num_nodes=%d", nnodes);
    putenv(tmp);
    pmix_asprintf(&tmp, "PMIX_MCA_pnet_sshot_devs_per_node=%d", ndevs);
    putenv(tmp);
    pmix_asprintf(&tmp, "PMIX_MCA_pnet_sshot_ppn=%d", ppn);
    putenv(tmp);

    if (PMIX_SUCCESS != (rc = PMIx_server_init(NULL, NULL, 0))) {
        pmix_output(0, "Server init failed: %s", PMIx_Error_string(rc));
        exit(rc);
    }
    pmix_output(0, "Server running");

    DEBUG_CONSTRUCT_LOCK(&cd.lock);
    if (PMIX_SUCCESS
        != (rc = PMIx_server_setup_application("SIMPSCHED", NULL, 0, setup_cbfunc, &cd))) {
        pmix_output(0, "[%s:%d] PMIx_server_setup_application failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(rc));
        DEBUG_DESTRUCT_LOCK(&cd.lock);
        goto done;
    }
    DEBUG_WAIT_THREAD(&cd.lock);
    DEBUG_DESTRUCT_LOCK(&cd.lock);

    DEBUG_CONSTRUCT_LOCK(&lock);
    if (PMIX_SUCCESS
        != (rc = PMIx_server_setup_local_support("SIMPSCHED", cd.info, cd.ninfo, local_cbfunc,
                                                 &lock))) {
        pmix_output(0, "[%s:%d] PMIx_server_setup_local_support failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(rc));
        DEBUG_DESTRUCT_LOCK(&lock);
        goto done;
    }
    DEBUG_WAIT_THREAD(&lock);
    DEBUG_DESTRUCT_LOCK(&lock);

    /* check a few things */
    PMIX_LOAD_PROCID(&proc, "SIMPSCHED", PMIX_RANK_WILDCARD);
    rc = PMIx_Get(&proc, "HPE_TRAFFIC_CLASS", NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "[%s:%d] Get of traffic class failed: %s", __FILE__, __LINE__,
                    PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, tclass, int);
    if (PMIX_SUCCESS != rc) {
        pmix_output(0, "[%s:%d] Get of traffic class returned non-number", __FILE__, __LINE__);
        goto done;
    }
    pmix_output(0, "[%s:%d] Got traffic class %d", __FILE__, __LINE__, tclass);
    PMIX_VALUE_RELEASE(val);

    /* the test nodes are "nid000000" and "nid000001" */
    PMIX_INFO_LOAD(&info[0], PMIX_NODE_INFO, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&info[1], PMIX_HOSTNAME, "nid000000", PMIX_STRING);

    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_FABRIC_COORDINATES, info, 2, &val))
        || NULL == val) {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get fabric coordinate failed: %s", proc.nspace,
                    proc.rank, PMIx_Error_string(rc));
        goto done;
    }
    if (PMIX_DATA_ARRAY == val->type) {
        geos = (pmix_geometry_t *) val->data.darray->array;
        ngeos = val->data.darray->size;
        /* print them out for diagnostics - someday we can figure
         * out an automated way of testing the answer */
        for (m = 0; m < ngeos; m++) {
            char **foo = NULL;
            char *view;
            pmix_output(0, "Device[%d]: %s Osname: %s", (int) m, geos[m].uuid, geos[m].osname);
            for (n = 0; n < geos[m].coordinates[0].dims; n++) {
                asprintf(&tmp, "%d", geos[m].coordinates[0].coord[n]);
                PMIx_Argv_append_nosize(&foo, tmp);
                free(tmp);
            }
            tmp = PMIx_Argv_join(foo, ',');
            PMIx_Argv_free(foo);
            if (PMIX_COORD_LOGICAL_VIEW == geos[m].coordinates[0].view) {
                view = "LOGICAL";
            } else {
                view = "PHYSICAL";
            }
            pmix_output(0, "\tCOORD VIEW %s: %s", view, tmp);
            free(tmp);
        }
    } else {
        pmix_output(0, "Client ns %s rank %d: PMIx_Get fabric coordinate returned wrong type: %s",
                    proc.nspace, proc.rank, PMIx_Data_type_string(val->type));
    }

done:
    /* finalize us */
    pmix_output(0, "Server finalizing");
    if (PMIX_SUCCESS != (rc = PMIx_server_finalize())) {
        fprintf(stderr, "Server finalize failed: %s\n", PMIx_Error_string(rc));
    } else {
        fprintf(stderr, "Server finalize successfully completed\n");
    }
    fflush(stderr);
    return (rc);
}
