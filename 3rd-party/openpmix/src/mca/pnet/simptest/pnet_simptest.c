/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#include <time.h>

#include "pmix_common.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_pointer_array.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/preg/preg.h"
#include "src/util/pmix_alfg.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_show_help.h"

#include "pnet_simptest.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/pnet/pnet.h"

static pmix_status_t simptest_init(void);
static void simptest_finalize(void);
static pmix_status_t allocate(pmix_namespace_t *nptr, pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_namespace_t *nptr, pmix_info_t info[], size_t ninfo);

pmix_pnet_module_t pmix_simptest_module = {
    .name = "simptest",
    .init = simptest_init,
    .finalize = simptest_finalize,
    .allocate = allocate,
    .setup_local_network = setup_local_network};

/* internal tracking structures */
typedef struct {
    pmix_list_item_t super;
    char *name;
    pmix_geometry_t *devices;
    size_t ndevices;
    pmix_endpoint_t *endpts;
    size_t nendpts;
    pmix_device_distance_t *distances;
    size_t ndists;
} pnet_node_t;
static void ndcon(pnet_node_t *p)
{
    p->name = NULL;
    p->devices = NULL;
    p->endpts = NULL;
    p->distances = NULL;
}
static void nddes(pnet_node_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->devices) {
        PMIX_GEOMETRY_FREE(p->devices, p->ndevices);
    }
    if (NULL != p->endpts) {
        PMIX_ENDPOINT_FREE(p->endpts, p->nendpts);
    }
    if (NULL != p->distances) {
        PMIX_DEVICE_DIST_FREE(p->distances, p->ndists);
    }
}
static PMIX_CLASS_INSTANCE(pnet_node_t, pmix_list_item_t, ndcon, nddes);

/* internal variables */
static pmix_list_t mynodes;

#define PMIX_SIMPTEST_MAX_LINE_LENGTH 1024

static char *localgetline(FILE *fp)
{
    char *ret, *buff;
    char input[PMIX_SIMPTEST_MAX_LINE_LENGTH];
    int i = 0;

    ret = fgets(input, PMIX_SIMPTEST_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        if ('\0' != input[0]) {
            input[strlen(input) - 1] = '\0'; /* remove newline */
                                             /* strip any leading whitespace */
            while (' ' == input[i] || '\t' == input[i]) {
                i++;
            }
        }
        buff = strdup(&input[i]);
        return buff;
    }

    return NULL;
}

static pmix_status_t simptest_init(void)
{
    FILE *fp = NULL;
    char *line, **tmp;
    pnet_node_t *nd;
    int n, cache[1024];

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output, "pnet: simptest init");

    PMIX_CONSTRUCT(&mynodes, pmix_list_t);

    /* if the configuration was given in a file, then build
     * the topology so we can respond to requests */
    if (NULL == pmix_mca_pnet_simptest_component.configfile) {
        /* we cannot function */
        return PMIX_ERR_INIT;
    }

    fp = fopen(pmix_mca_pnet_simptest_component.configfile, "r");
    if (NULL == fp) {
        pmix_show_help("help-pnet-simptest.txt", "missing-file", true,
                       pmix_mca_pnet_simptest_component.configfile);
        return PMIX_ERR_FATAL;
    }
    while (NULL != (line = localgetline(fp))) {
        /* if the line starts with a '#' or is blank, then
         * it is a comment and we ignore it */
        if (0 == strlen(line) || '#' == line[0]) {
            free(line);
            continue;
        }
        tmp = PMIx_Argv_split(line, ' ');
        nd = PMIX_NEW(pnet_node_t);
        nd->name = strdup(tmp[0]);
        pmix_list_append(&mynodes, &nd->super);
        n = 0;
        while (n < 1024 && NULL != tmp[n + 1]) {
            cache[n] = strtol(tmp[n + 1], NULL, 10);
            ++n;
        }

        nd->coord.dims = n;
        nd->coord.coord = (int *) malloc(nd->coord.dims * sizeof(int));
        memcpy(nd->coord.coord, cache, nd->coord.dims * sizeof(int));
        free(line);
        PMIx_Argv_free(tmp);
    }

    if (NULL != fp) {
        fclose(fp);
    }
    return PMIX_SUCCESS;
}

static void simptest_finalize(void)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output, "pnet: simptest finalize");

    PMIX_LIST_DESTRUCT(&mynodes);
}

/* NOTE: if there is any binary data to be transferred, then
 * this function MUST pack it for transport as the host will
 * not know how to do so */
static pmix_status_t allocate(pmix_namespace_t *nptr, pmix_info_t info[], size_t ninfo,
                              pmix_list_t *ilist)
{
    size_t m, n, q;
    char **procs = NULL;
    char **nodes = NULL;
    pmix_status_t rc;
    pmix_list_t mylist;
    char **locals;
    pnet_node_t *nd, *nd2;
    pmix_kval_t *kv;
    pmix_info_t *iptr, *ip2;
    pmix_data_array_t *darray, *d2, *d3;
    pmix_rank_t rank;
    pmix_buffer_t buf;
    pmix_byte_object_t *bptr;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:simptest:allocate for nspace %s", nptr->nspace);

    /* if I am not the scheduler, then ignore this call - should never
     * happen, but check to be safe */
    if (!PMIX_PEER_IS_SCHEDULER(pmix_globals.mypeer)) {
        return PMIX_SUCCESS;
    }

    if (NULL == info) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* check directives to see if a crypto key and/or
     * fabric resource allocations requested */
    for (n = 0; n < ninfo; n++) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:simptest:allocate processing key %s", info[n].key);
        if (PMIX_CHECK_KEY(&info[n], PMIX_PROC_MAP)) {
            rc = pmix_preg.parse_procs(info[n].value.data.string, &procs);
            if (PMIX_SUCCESS != rc) {
                return PMIX_ERR_BAD_PARAM;
            }
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_NODE_MAP)) {
            rc = pmix_preg.parse_nodes(info[n].value.data.string, &nodes);
            if (PMIX_SUCCESS != rc) {
                return PMIX_ERR_BAD_PARAM;
            }
        }
    }

    if (NULL == procs || NULL == nodes) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet:simptest:allocate missing proc/node map for nspace %s",
                            nptr->nspace);
        /* not an error - continue to next active component */
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    PMIX_CONSTRUCT(&mylist, pmix_list_t);

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:simptest:allocate assigning endpoints for nspace %s", nptr->nspace);

    /* cycle across the nodes and add the endpoints
     * for each proc on the node - we assume the same
     * list of static endpoints on each node */
    for (n = 0; NULL != nodes[n]; n++) {
        /* split the procs for this node */
        locals = PMIx_Argv_split(procs[n], ',');
        if (NULL == locals) {
            /* aren't any on this node */
            continue;
        }
        /* find this node in our list */
        nd = NULL;
        PMIX_LIST_FOREACH (nd2, &mynodes, pnet_node_t) {
            if (0 == strcmp(nd2->name, nodes[n])) {
                nd = nd2;
                break;
            }
        }
        if (NULL == nd) {
            /* should be impossible */
            rc = PMIX_ERR_NOT_FOUND;
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->key = strdup(PMIX_ALLOC_FABRIC_ENDPTS);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_DATA_ARRAY;
        /* for each proc, we will assign an endpt
         * for each NIC on the node */
        q = PMIx_Argv_count(locals);
        PMIX_DATA_ARRAY_CREATE(darray, q, PMIX_INFO);
        kv->value->data.darray = darray;
        iptr = (pmix_info_t *) darray->array;
        for (m = 0; NULL != locals[m]; m++) {
            /* each proc has one endpoint and one coord corresponding to the
             * node they upon which they are executing */
            PMIX_LOAD_KEY(iptr[m].key, PMIX_PROC_DATA);
            PMIX_DATA_ARRAY_CREATE(d2, 3, PMIX_INFO);
            iptr[m].value.type = PMIX_DATA_ARRAY;
            iptr[m].value.data.darray = d2;
            ip2 = (pmix_info_t *) d2->array;
            /* start with the rank */
            rank = strtoul(locals[m], NULL, 10);
            pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                "pnet:simptest:allocate assigning %d endpoints for rank %u",
                                (int) q, rank);
            PMIX_INFO_LOAD(&ip2[0], PMIX_RANK, &rank, PMIX_PROC_RANK);
            /* the second element in this array is the coord */
            PMIX_INFO_LOAD(&ip2[1], PMIX_FABRIC_COORDINATE, &nd->coord, PMIX_COORD);
            /* third element is the endpt */
            PMIX_DATA_ARRAY_CREATE(d3, 1, PMIX_BYTE_OBJECT);
            PMIX_LOAD_KEY(ip2[2].key, PMIX_FABRIC_ENDPT);
            ip2[2].value.type = PMIX_DATA_ARRAY;
            ip2[2].value.data.darray = d3;
            bptr = (pmix_byte_object_t *) d3->array;
            bptr[0].bytes = strdup(nd->endpt.bytes);
            bptr[0].size = nd->endpt.size;
        }
        PMIx_Argv_free(locals);
        locals = NULL;
        pmix_list_append(&mylist, &kv->super);
    }

    /* pack all our results into a buffer for xmission to the backend */
    n = pmix_list_get_size(&mylist);
    if (0 < n) {
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        /* cycle across the list and pack the kvals */
        while (NULL != (kv = (pmix_kval_t *) pmix_list_remove_first(&mylist))) {
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &buf, kv, 1, PMIX_KVAL);
            PMIX_RELEASE(kv);
            if (PMIX_SUCCESS != rc) {
                PMIX_DESTRUCT(&buf);
                goto cleanup;
            }
        }
        kv = PMIX_NEW(pmix_kval_t);
        kv->key = strdup("pmix-pnet-simptest-blob");
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            PMIX_RELEASE(kv);
            PMIX_DESTRUCT(&buf);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        kv->value->type = PMIX_BYTE_OBJECT;
        PMIX_UNLOAD_BUFFER(&buf, kv->value->data.bo.bytes, kv->value->data.bo.size);
        PMIX_DESTRUCT(&buf);
        pmix_list_append(ilist, &kv->super);
    }

cleanup:
    PMIX_LIST_DESTRUCT(&mylist);
    if (NULL != nodes) {
        PMIx_Argv_free(nodes);
    }
    if (NULL != procs) {
        PMIx_Argv_free(procs);
    }
    if (NULL != locals) {
        PMIx_Argv_free(locals);
    }
    return rc;
}

static pmix_status_t setup_local_network(pmix_namespace_t *nptr, pmix_info_t info[], size_t ninfo)
{
    size_t n, nvals;
    pmix_buffer_t bkt;
    int32_t cnt;
    pmix_kval_t *kv;
    pmix_status_t rc;
    pmix_info_t *iptr;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:simptest:setup_local_network with %lu info", (unsigned long) ninfo);

    if (NULL != info) {
        for (n = 0; n < ninfo; n++) {
            /* look for my key */
            if (PMIX_CHECK_KEY(&info[n], "pmix-pnet-simptest-blob")) {
                pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                    "pnet:simptest:setup_local_network found my blob");
                /* this macro NULLs and zero's the incoming bo */
                PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt, info[n].value.data.bo.bytes,
                                 info[n].value.data.bo.size);
                /* cycle thru the blob and extract the kvals */
                kv = PMIX_NEW(pmix_kval_t);
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, kv, &cnt, PMIX_KVAL);
                while (PMIX_SUCCESS == rc) {
                    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                        "recvd KEY %s %s", kv->key,
                                        PMIx_Data_type_string(kv->value->type));
                    /* check for the fabric ID */
                    if (PMIX_CHECK_KEY(kv, PMIX_ALLOC_FABRIC_ENDPTS)) {
                        iptr = (pmix_info_t *) kv->value->data.darray->array;
                        nvals = kv->value->data.darray->size;
                        /* each element in this array is itself an array containing
                         * the rank and the endpts and coords assigned to that rank. This is
                         * precisely the data we need to cache for the job, so
                         * just do so) */
                        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                            "pnet:simptest:setup_local_network caching %d endpts",
                                            (int) nvals);
                        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr, iptr, nvals);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_RELEASE(kv);
                            return rc;
                        }
                    }
                    PMIX_RELEASE(kv);
                    kv = PMIX_NEW(pmix_kval_t);
                    cnt = 1;
                    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &bkt, kv, &cnt, PMIX_KVAL);
                }
                PMIX_RELEASE(kv);
                /* restore the incoming data */
                info[n].value.data.bo.bytes = bkt.base_ptr;
                info[n].value.data.bo.size = bkt.bytes_used;
                bkt.base_ptr = NULL;
                bkt.bytes_used = 0;
            }
        }
    }

    return PMIX_SUCCESS;
}
