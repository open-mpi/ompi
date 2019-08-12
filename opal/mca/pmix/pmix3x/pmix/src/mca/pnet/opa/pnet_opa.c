/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
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

#if PMIX_WANT_OPAMGT
#include <opamgt/opamgt.h>
#include <opamgt/opamgt_sa.h>
#endif

#include <pmix_common.h>

#include "src/mca/base/pmix_mca_base_var.h"
#include "src/class/pmix_list.h"
#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/class/pmix_list.h"
#include "src/util/alfg.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/mca/preg/preg.h"
#include "src/hwloc/hwloc-internal.h"

#include "src/mca/pnet/pnet.h"
#include "src/mca/pnet/base/base.h"
#include "pnet_opa.h"

static pmix_status_t opa_init(void);
static void opa_finalize(void);
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t *info,
                              pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_namespace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(pmix_namespace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env);
static void child_finalized(pmix_proc_t *peer);
static void local_app_finalized(pmix_namespace_t *nptr);
static void deregister_nspace(pmix_namespace_t *nptr);
static pmix_status_t collect_inventory(pmix_info_t directives[], size_t ndirs,
                                       pmix_inventory_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t deliver_inventory(pmix_info_t info[], size_t ninfo,
                                       pmix_info_t directives[], size_t ndirs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata);

pmix_pnet_module_t pmix_opa_module = {
    .name = "opa",
    .init = opa_init,
    .finalize = opa_finalize,
    .allocate = allocate,
    .setup_local_network = setup_local_network,
    .setup_fork = setup_fork,
    .child_finalized = child_finalized,
    .local_app_finalized = local_app_finalized,
    .deregister_nspace = deregister_nspace,
    .collect_inventory = collect_inventory,
    .deliver_inventory = deliver_inventory
};

/* local object definitions */
typedef struct {
    pmix_list_item_t super;
    char *name;
    char *value;
} opa_attr_t;
static void atcon(opa_attr_t *p)
{
    p->name = NULL;
    p->value = NULL;
}
static void atdes(opa_attr_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->value) {
        free(p->value);
    }
}
static PMIX_CLASS_INSTANCE(opa_attr_t,
                           pmix_list_item_t,
                           atcon, atdes);

typedef struct {
    pmix_list_item_t super;
    char *device;
    pmix_list_t attributes;
} opa_resource_t;
static void rcon(opa_resource_t *p)
{
    p->device = NULL;
    PMIX_CONSTRUCT(&p->attributes, pmix_list_t);
}
static void rdes(opa_resource_t *p)
{
    if (NULL != p->device) {
        free(p->device);
    }
    PMIX_LIST_DESTRUCT(&p->attributes);
}
static PMIX_CLASS_INSTANCE(opa_resource_t,
                           pmix_list_item_t,
                           rcon, rdes);


static pmix_status_t opa_init(void)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: opa init");
    return PMIX_SUCCESS;
}

static void opa_finalize(void)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: opa finalize");
}

/* some network transports require a little bit of information to
 * "pre-condition" them - i.e., to setup their individual transport
 * connections so they can generate their endpoint addresses. This
 * function provides a means for doing so. The resulting info is placed
 * into the app_context's env array so it will automatically be pushed
 * into the environment of every MPI process when launched.
 */

static inline void transports_use_rand(uint64_t* unique_key) {
    pmix_rng_buff_t rng;
    pmix_srand(&rng,(unsigned int)time(NULL));
    unique_key[0] = pmix_rand(&rng);
    unique_key[1] = pmix_rand(&rng);
}

static char* transports_print(uint64_t *unique_key)
{
    unsigned int *int_ptr;
    size_t i, j, string_key_len, written_len;
    char *string_key = NULL, *format = NULL;

    /* string is two 64 bit numbers printed in hex with a dash between
     * and zero padding.
     */
    string_key_len = (sizeof(uint64_t) * 2) * 2 + strlen("-") + 1;
    string_key = (char*) malloc(string_key_len);
    if (NULL == string_key) {
        return NULL;
    }

    string_key[0] = '\0';
    written_len = 0;

    /* get a format string based on the length of an unsigned int.  We
     * want to have zero padding for sizeof(unsigned int) * 2
     * characters -- when printing as a hex number, each byte is
     * represented by 2 hex characters.  Format will contain something
     * that looks like %08lx, where the number 8 might be a different
     * number if the system has a different sized long (8 would be for
     * sizeof(int) == 4)).
     */
    if (0 > asprintf(&format, "%%0%dx", (int)(sizeof(unsigned int)) * 2)) {
        return NULL;
    }

    /* print the first number */
    int_ptr = (unsigned int*) &unique_key[0];
    for (i = 0 ; i < sizeof(uint64_t) / sizeof(unsigned int) ; ++i) {
        if (0 == int_ptr[i]) {
            /* inject some energy */
            for (j=0; j < sizeof(unsigned int); j++) {
                int_ptr[i] |= j << j;
            }
        }
        snprintf(string_key + written_len,
                 string_key_len - written_len,
                 format, int_ptr[i]);
        written_len = strlen(string_key);
    }

    /* print the middle dash */
    snprintf(string_key + written_len, string_key_len - written_len, "-");
    written_len = strlen(string_key);

    /* print the second number */
    int_ptr = (unsigned int*) &unique_key[1];
    for (i = 0 ; i < sizeof(uint64_t) / sizeof(unsigned int) ; ++i) {
        if (0 == int_ptr[i]) {
            /* inject some energy */
            for (j=0; j < sizeof(unsigned int); j++) {
                int_ptr[i] |= j << j;
            }
        }
        snprintf(string_key + written_len,
                 string_key_len - written_len,
                 format, int_ptr[i]);
        written_len = strlen(string_key);
    }
    free(format);

    return string_key;
}

/* NOTE: if there is any binary data to be transferred, then
 * this function MUST pack it for transport as the host will
 * not know how to do so */
static pmix_status_t allocate(pmix_namespace_t *nptr,
                              pmix_info_t *info,
                              pmix_list_t *ilist)
{
    uint64_t unique_key[2];
    char *string_key, *cs_env;
    int fd_rand;
    size_t bytes_read;
    pmix_kval_t *kv;
    bool envars, seckeys;
    pmix_status_t rc;

    envars = false;
    seckeys = false;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa:allocate for nspace %s", nptr->nspace);

    if (NULL == info) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (PMIX_CHECK_KEY(info, PMIX_SETUP_APP_ENVARS)) {
        envars = PMIX_INFO_TRUE(info);
    } else if (PMIX_CHECK_KEY(info, PMIX_SETUP_APP_ALL)) {
        envars = PMIX_INFO_TRUE(info);
        seckeys = PMIX_INFO_TRUE(info);
    } else if (PMIX_CHECK_KEY(info, PMIX_SETUP_APP_NONENVARS) ||
               PMIX_CHECK_KEY(info, PMIX_ALLOC_NETWORK_SEC_KEY)) {
        seckeys = PMIX_INFO_TRUE(info);
    }

    if (seckeys) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet: opa providing seckeys");
        /* put the number here - or else create an appropriate string. this just needs to
         * eventually be a string variable
         */
        if(-1 == (fd_rand = open("/dev/urandom", O_RDONLY))) {
            transports_use_rand(unique_key);
        } else {
            bytes_read = read(fd_rand, (char *) unique_key, 16);
            if(bytes_read != 16) {
                transports_use_rand(unique_key);
            }
            close(fd_rand);
        }

        if (NULL == (string_key = transports_print(unique_key))) {
            PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }

        if (PMIX_SUCCESS != pmix_mca_base_var_env_name("opa_precondition_transports", &cs_env)) {
            PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
            free(string_key);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }

        kv = PMIX_NEW(pmix_kval_t);
        if (NULL == kv) {
            free(string_key);
            free(cs_env);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->key = strdup(PMIX_SET_ENVAR);
        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
        if (NULL == kv->value) {
            free(string_key);
            free(cs_env);
            PMIX_RELEASE(kv);
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        kv->value->type = PMIX_ENVAR;
        PMIX_ENVAR_LOAD(&kv->value->data.envar, cs_env, string_key, ':');
        pmix_list_append(ilist, &kv->super);
        free(cs_env);
        free(string_key);
        if (!envars) {
            /* providing envars does not constitute allocating resources */
            return PMIX_ERR_TAKE_NEXT_OPTION;
        }
    }

    if (envars) {
        pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                            "pnet: opa harvesting envars %s excluding %s",
                            (NULL == mca_pnet_opa_component.incparms) ? "NONE" : mca_pnet_opa_component.incparms,
                            (NULL == mca_pnet_opa_component.excparms) ? "NONE" : mca_pnet_opa_component.excparms);
        /* harvest envars to pass along */
        if (NULL != mca_pnet_opa_component.include) {
            rc = pmix_pnet_base_harvest_envars(mca_pnet_opa_component.include,
                                               mca_pnet_opa_component.exclude,
                                               ilist);
            if (PMIX_SUCCESS == rc) {
                return PMIX_ERR_TAKE_NEXT_OPTION;
            }
            return rc;
        }
    }

    /* we don't currently manage OPA resources */
    return PMIX_ERR_TAKE_NEXT_OPTION;
}

static pmix_status_t setup_local_network(pmix_namespace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    size_t n;
    pmix_kval_t *kv;


    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: opa setup_local_network");

    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_PNET_OPA_BLOB, PMIX_MAX_KEYLEN)) {
                /* the byte object contains a packed blob that needs to be
                 * cached until we determine we have local procs for this
                 * nspace, and then delivered to the local OPA driver when
                 * we have a means for doing so */
                kv = PMIX_NEW(pmix_kval_t);
                if (NULL == kv) {
                    return PMIX_ERR_NOMEM;
                }
                kv->key = strdup(info[n].key);
                kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
                if (NULL == kv->value) {
                    PMIX_RELEASE(kv);
                    return PMIX_ERR_NOMEM;
                }
                pmix_value_xfer(kv->value, &info[n].value);
                if (PMIX_ENVAR == kv->value->type) {
                    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                        "pnet:opa:setup_local_network adding %s=%s to environment",
                                        kv->value->data.envar.envar, kv->value->data.envar.value);
                } else {
                    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                                        "pnet:opa:setup_local_network loading blob");
                }
                pmix_list_append(&nptr->setup_data, &kv->super);
            }
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(pmix_namespace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env)
{
    pmix_kval_t *kv, *next;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet: opa setup fork");

    /* if there are any cached nspace prep blobs, execute them,
     * ensuring that we only do so once per nspace - note that
     * we don't expect to find any envars here, though we could
     * have included some if we needed to set them per-client */
    PMIX_LIST_FOREACH_SAFE(kv, next, &nptr->setup_data, pmix_kval_t) {
        if (0 == strcmp(kv->key, PMIX_PNET_OPA_BLOB)) {
            pmix_list_remove_item(&nptr->setup_data, &kv->super);
            /* deliver to the local lib */
            PMIX_RELEASE(kv);
        }
    }
    return PMIX_SUCCESS;
}

static void child_finalized(pmix_proc_t *peer)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa child finalized");
}

static void local_app_finalized(pmix_namespace_t *nptr)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa app finalized");

}

static void deregister_nspace(pmix_namespace_t *nptr)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa deregister nspace");

}

static pmix_status_t collect_inventory(pmix_info_t directives[], size_t ndirs,
                                       pmix_inventory_cbfunc_t cbfunc, void *cbdata)
{
    pmix_inventory_rollup_t *cd = (pmix_inventory_rollup_t*)cbdata;
#if PMIX_HAVE_HWLOC
    hwloc_obj_t obj;
#endif
    unsigned n;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_buffer_t bucket, pbkt;
    bool found = false;
    pmix_byte_object_t pbo;
    char nodename[PMIX_MAXHOSTNAMELEN] = {0}, *foo;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa collect inventory");

    /* setup the bucket - we will pass the results as a blob */
    PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
    /* pack our node name */
    gethostname(nodename, sizeof(nodename)-1);
    foo = &nodename[0];
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &foo, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DESTRUCT(&bucket);
        return rc;
    }

#if PMIX_HAVE_HWLOC
    if (NULL == pmix_hwloc_topology) {
        goto query;
    }

    /* search the topology for OPA devices */
    obj = hwloc_get_next_osdev(pmix_hwloc_topology, NULL);
    while (NULL != obj) {
        if (obj->attr->osdev.type != HWLOC_OBJ_OSDEV_OPENFABRICS ||
            0 != strncmp(obj->name, "hfi", 3)) {
            obj = hwloc_get_next_osdev(pmix_hwloc_topology, obj);
            continue;
        }
        found = true;
        if (9 < pmix_output_get_verbosity(pmix_pnet_base_framework.framework_output)) {
            /* dump the discovered node resources */
            pmix_output(0, "OPA resource discovered on node: %s", nodename);
            pmix_output(0, "\tDevice name: %s", obj->name);
            for (n=0; n < obj->infos_count; n++) {
                pmix_output(0, "\t\t%s: %s", obj->infos[n].name, obj->infos[n].value);
            }
        }
        /* pack the name of the device */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, &obj->name, 1, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pbkt);
            PMIX_DESTRUCT(&bucket);
            return rc;
        }
        /* pack the number of attributes */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, &obj->infos_count, 1, PMIX_UINT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pbkt);
            PMIX_DESTRUCT(&bucket);
            return rc;
        }
        /* pack each descriptive object */
        for (n=0; n < obj->infos_count; n++) {
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, &obj->infos[n].name, 1, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&bucket);
                return rc;
            }
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, &obj->infos[n].value, 1, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&bucket);
                return rc;
            }
        }
        /* extract the resulting blob - this is a device unit */
        PMIX_UNLOAD_BUFFER(&pbkt, pbo.bytes, pbo.size);
        /* now load that into the blob */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &pbo, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            PMIX_DESTRUCT(&bucket);
            return rc;
        }
        obj = hwloc_get_next_osdev(pmix_hwloc_topology, obj);
    }

  query:
#if 0
#if PMIX_WANT_OPAMGT
    if (PMIX_PROC_IS_GATEWAY(pmix_globals.mypeer)) {
        /* collect the switch information from the FM */
        OMGT_STATUS_T status = OMGT_STATUS_SUCCESS;
        struct omgt_port * port = NULL;
        omgt_sa_selector_t selector;

        /* create a session */
        status = omgt_open_port_by_num(&port, 1 /* hfi */, 1 /* port */, NULL);
        if (OMGT_STATUS_SUCCESS != status) {
            pmix_output_verbose(1, pmix_pnet_base_framework.framework_output,
                                "Unable to open port to FM");
            goto complete;
        }
        /* specify how and what we want to query by */
        selector.InputType = InputTypeLid;
        selector.InputValue.PortInfoRecord.Lid = 1;

    }
#endif
#endif
    /* if we found any devices, then return the blob */
    if (!found) {
        PMIX_DESTRUCT(&bucket);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the resulting blob */
    PMIX_UNLOAD_BUFFER(&bucket, pbo.bytes, pbo.size);
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup(PMIX_OPA_INVENTORY_KEY);
    PMIX_VALUE_CREATE(kv->value, 1);
    pmix_value_load(kv->value, &pbo, PMIX_BYTE_OBJECT);
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    pmix_list_append(&cd->payload, &kv->super);

#else  // have_hwloc
#if 0
#if PMIX_WANT_OPAMGT
    if (PMIX_PROC_IS_GATEWAY(pmix_globals.mypeer)) {
        /* query the FM for the inventory */
    }

  complete:
    /* if we found any devices, then return the blob */
    if (!found) {
        PMIX_DESTRUCT(&bucket);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the resulting blob */
    PMIX_UNLOAD_BUFFER(&bucket, pbo.bytes, pbo.size);
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup(PMIX_OPA_INVENTORY_KEY);
    PMIX_VALUE_CREATE(kv->value, 1);
    pmix_value_load(kv->value, &pbo, PMIX_BYTE_OBJECT);
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    pmix_list_append(&cd->payload, &kv->super);

#endif
#endif
    return PMIX_ERR_TAKE_NEXT_OPTION;
#endif  // have_hwloc

    return PMIX_SUCCESS;
}

static pmix_status_t deliver_inventory(pmix_info_t info[], size_t ninfo,
                                       pmix_info_t directives[], size_t ndirs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t bkt, pbkt;
    size_t n;
    int32_t cnt;
    unsigned m, nattrs;
    char *hostname;
    pmix_byte_object_t pbo;
    pmix_pnet_node_t *nd, *ndptr;
    pmix_pnet_resource_t *lt, *lst;
    opa_attr_t *attr;
    opa_resource_t *res;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa deliver inventory");

    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_OPA_INVENTORY_KEY, PMIX_MAX_KEYLEN)) {
            /* this is our inventory in the form of a blob */
            PMIX_CONSTRUCT(&bkt,pmix_buffer_t);
            PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt,
                             info[n].value.data.bo.bytes,
                             info[n].value.data.bo.size);
            /* first is the host this came from */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                               &bkt, &hostname, &cnt, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                /* must _not_ destruct bkt as we don't
                 * own the bytes! */
                return rc;
            }
            /* do we already have this node? */
            nd = NULL;
            PMIX_LIST_FOREACH(ndptr, &pmix_pnet_globals.nodes, pmix_pnet_node_t) {
                if (0 == strcmp(hostname, ndptr->name)) {
                    nd = ndptr;
                    break;
                }
            }
            if (NULL == nd) {
                nd = PMIX_NEW(pmix_pnet_node_t);
                nd->name = strdup(hostname);
                pmix_list_append(&pmix_pnet_globals.nodes, &nd->super);
            }
            /* does this node already have an OPA entry? */
            lst = NULL;
            PMIX_LIST_FOREACH(lt, &nd->resources, pmix_pnet_resource_t) {
                if (0 == strcmp(lt->name, "opa")) {
                    lst = lt;
                    break;
                }
            }
            if (NULL == lst) {
                lst = PMIX_NEW(pmix_pnet_resource_t);
                lst->name = strdup("opa");
                pmix_list_append(&nd->resources, &lst->super);
            }
            /* each device was packed as a "blob" */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                               &bkt, &pbo, &cnt, PMIX_BYTE_OBJECT);
            while (PMIX_SUCCESS == rc) {
                /* load the blob for unpacking */
                PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                PMIX_LOAD_BUFFER(pmix_globals.mypeer, &pbkt,
                                 pbo.bytes, pbo.size);

                res = PMIX_NEW(opa_resource_t);
                /* starts with the name of the device */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                   &pbkt, &res->device, &cnt, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_RELEASE(res);
                    return rc;
                }
                /* next comes the numbers of attributes for that device */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                   &pbkt, &nattrs, &cnt, PMIX_UINT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_RELEASE(res);
                    return rc;
                }
                for (m=0; m < nattrs; m++) {
                    attr = PMIX_NEW(opa_attr_t);
                    /* unpack the name of the attribute */
                    cnt = 1;
                    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                       &pbkt, &attr->name, &cnt, PMIX_STRING);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_RELEASE(attr);
                        PMIX_RELEASE(res);
                        return rc;
                    }
                    /* unpack the attribute value */
                    cnt = 1;
                    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                       &pbkt, &attr->value, &cnt, PMIX_STRING);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_RELEASE(attr);
                        PMIX_RELEASE(res);
                        return rc;
                    }
                    pmix_list_append(&res->attributes, &attr->super);
                }
                pmix_list_append(&lst->resources, &res->super);
                PMIX_DESTRUCT(&pbkt);

                /* get the next device unit */
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                                   &bkt, &pbo, &cnt, PMIX_BYTE_OBJECT);
            }
            if (5 < pmix_output_get_verbosity(pmix_pnet_base_framework.framework_output)) {
                /* dump the resulting node resources */
                pmix_output(0, "OPA resources for node: %s", nd->name);
                PMIX_LIST_FOREACH(lt, &nd->resources, pmix_pnet_resource_t) {
                    if (0 == strcmp(lt->name, "opa")) {
                        PMIX_LIST_FOREACH(res, &lt->resources, opa_resource_t) {
                            pmix_output(0, "\tDevice: %s", res->device);
                            PMIX_LIST_FOREACH(attr, &res->attributes, opa_attr_t) {
                                pmix_output(0, "\t\t%s: %s", attr->name, attr->value);
                            }
                        }
                    }
                }
            }
        }
    }

    return PMIX_SUCCESS;
}
