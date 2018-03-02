/*
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <time.h>

#include <pmix_common.h>

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/class/pmix_list.h"
#include "src/util/alfg.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/mca/preg/preg.h"

#include "src/mca/pnet/pnet.h"
#include "src/mca/pnet/base/base.h"
#include "pnet_test.h"

static pmix_status_t test_init(void);
static void test_finalize(void);
static pmix_status_t setup_app(pmix_nspace_t *nptr,
                               pmix_info_t info[], size_t ninfo,
                               pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(pmix_nspace_t *nptr, char ***env);
static void child_finalized(pmix_peer_t *peer);
static void local_app_finalized(char *nspace);

pmix_pnet_module_t pmix_test_module = {
    .init = test_init,
    .finalize = test_finalize,
    .setup_app = setup_app,
    .setup_local_network = setup_local_network,
    .setup_fork = setup_fork,
    .child_finalized = child_finalized,
    .local_app_finalized = local_app_finalized
};

static pmix_status_t test_init(void)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: test init");
    return PMIX_SUCCESS;
}

static void test_finalize(void)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: test finalize");
}

/* NOTE: if there is any binary data to be transferred, then
 * this function MUST pack it for transport as the host will
 * not know how to do so */
static pmix_status_t setup_app(pmix_nspace_t *nptr,
                               pmix_info_t info[], size_t ninfo,
                               pmix_list_t *ilist)
{
    uint64_t unique_key[2];
    char *string_key, *cs_env;
    int fd_rand;
    size_t n, bytes_read, len;
    pmix_kval_t *kv, *next;
    int i, j;
    bool envars, seckeys;

    if (NULL == info) {
        envars = true;
        seckeys = true;
    } else {
        envars = false;
        seckeys = false;
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_SETUP_APP_ENVARS, PMIX_MAX_KEYLEN)) {
                envars = PMIX_INFO_TRUE(&info[n]);
            } else if (0 == strncmp(info[n].key, PMIX_SETUP_APP_ALL, PMIX_MAX_KEYLEN)) {
                envars = PMIX_INFO_TRUE(&info[n]);
                seckeys = PMIX_INFO_TRUE(&info[n]);
            } else if (0 == strncmp(info[n].key, PMIX_SETUP_APP_NONENVARS, PMIX_MAX_KEYLEN)) {
                seckeys = PMIX_INFO_TRUE(&info[n]);
            }
        }
    }

    if (seckeys) {
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            return PMIX_ERR_NOMEM;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, "PMIX_TEST_SECKEY", "1", ':');
        pmix_list_append(ilist, &kv->super);
    }

    if (envars) {
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            return PMIX_ERR_NOMEM;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            return PMIX_ERR_NOMEM;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, "PMIX_TEST_ENVAR", "1", ':');
        pmix_list_append(ilist, &kv->super);
    }

    /* provide a blob so setup_local_network will get called */
    kv = PMIX_NEW(pmix_kval_t);
    if (NULL == kv) {
        return PMIX_ERR_NOMEM;
    }
    kv->key = strdup("pmix-pnet-test-blob");
    kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    if (NULL == kv->value) {
        PMIX_RELEASE(kv);
        return PMIX_ERR_NOMEM;
    }
    kv->value->type = PMIX_STRING;
    kv->value->data.string = strdup("foobar");
    pmix_list_append(ilist, &kv->super);


    return PMIX_SUCCESS;
}

static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    size_t n, m;
    pmix_status_t rc;
    pmix_kval_t *kv;
    char *nodestring, **nodes;
    pmix_proc_t *procs;
    size_t nprocs;

    /* get the list of nodes in this job - returns a regex */
    pmix_output(0, "pnet:setup_local_network NSPACE %s", (NULL == nptr) ? "NULL" : nptr->nspace);
    pmix_preg.resolve_nodes(nptr->nspace, &nodestring);
    if (NULL == nodestring) {
        return PMIX_SUCCESS;
    }
    pmix_preg.parse_nodes(nodestring, &nodes);  // get an argv array of node names
    pmix_output(0, "pnet:setup_local_network NODES %s", (NULL == nodes) ? "NULL" : "NON-NULL");
    if (NULL == nodes) {
        free(nodestring);
        return PMIX_SUCCESS;
    }
    for (n=0; NULL != nodes[n]; n++) {
        pmix_output(0, "pnet:setup_local_network NODE: %s", nodes[n]);
    }

   for (n=0; NULL != nodes[n]; n++) {
    /* get an array of pmix_proc_t containing the names of the procs on that node */
          pmix_preg.resolve_peers(nodes[n], nptr->nspace, &procs, &nprocs);
          if (NULL == procs) {
            continue;
          }
          for (m=0; m < nprocs; m++) {
            pmix_output(0, "pnet:setup_local_network NODE %s: peer %s:%d", nodes[n], procs[m].nspace, procs[m].rank);
          }
          /* do stuff */
          free(procs);
   }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(pmix_nspace_t *nptr, char ***env)
{
    return PMIX_SUCCESS;
}

static void child_finalized(pmix_peer_t *peer)
{

}

static void local_app_finalized(char *nspace)
{

}
