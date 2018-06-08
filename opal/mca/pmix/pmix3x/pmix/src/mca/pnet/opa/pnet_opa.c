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

#if 0
#if PMIX_WANT_OPAMGT
#include "opamgt.h"
#endif
#endif

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
#include "src/hwloc/hwloc-internal.h"

#include "src/mca/pnet/pnet.h"
#include "src/mca/pnet/base/base.h"
#include "pnet_opa.h"

static pmix_status_t opa_init(void);
static void opa_finalize(void);
static pmix_status_t allocate(pmix_nspace_t *nptr,
                              pmix_info_t *info,
                              pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(pmix_nspace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env);
static void child_finalized(pmix_proc_t *peer);
static void local_app_finalized(pmix_nspace_t *nptr);
static void deregister_nspace(pmix_nspace_t *nptr);
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
static pmix_status_t allocate(pmix_nspace_t *nptr,
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
    if (NULL == info) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (0 == strncmp(info->key, PMIX_SETUP_APP_ENVARS, PMIX_MAX_KEYLEN)) {
        envars = PMIX_INFO_TRUE(info);
    } else if (0 == strncmp(info->key, PMIX_SETUP_APP_ALL, PMIX_MAX_KEYLEN)) {
        envars = PMIX_INFO_TRUE(info);
        seckeys = PMIX_INFO_TRUE(info);
    } else if (0 == strncmp(info->key, PMIX_SETUP_APP_NONENVARS, PMIX_MAX_KEYLEN)) {
        seckeys = PMIX_INFO_TRUE(info);
    }

    if (seckeys) {
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

static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    size_t n;
    pmix_kval_t *kv;


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
                pmix_list_append(&nptr->setup_data, &kv->super);
            }
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(pmix_nspace_t *nptr,
                                const pmix_proc_t *proc,
                                char ***env)
{
    pmix_kval_t *kv, *next;

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

static void local_app_finalized(pmix_nspace_t *nptr)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa app finalized");

}

static void deregister_nspace(pmix_nspace_t *nptr)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa deregister nspace");

}

static pmix_status_t collect_inventory(pmix_info_t directives[], size_t ndirs,
                                       pmix_inventory_cbfunc_t cbfunc, void *cbdata)
{
#if PMIX_HAVE_HWLOC
    pmix_inventory_rollup_t *cd = (pmix_inventory_rollup_t*)cbdata;
    hwloc_obj_t obj;
    unsigned n;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_buffer_t bucket, pbkt;
    bool found = false;
    pmix_byte_object_t pbo;
    char nodename[PMIX_MAXHOSTNAMELEN], *foo;

    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa collect inventory");

    if (NULL == pmix_hwloc_topology) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the bucket - we will pass the results as a blob */
    PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
    /* pack our node name */
    gethostname(nodename, sizeof(nodename));
    foo = &nodename[0];
    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &foo, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DESTRUCT(&bucket);
        return rc;
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
        /* pack the name of the device */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, &obj->name, 1, PMIX_STRING);
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

    /* if we found any devices, then return the blob */
    if (!found) {
        PMIX_DESTRUCT(&bucket);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the resulting blob */
    PMIX_UNLOAD_BUFFER(&bucket, pbo.bytes, pbo.size);
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup(PMIX_PNET_OPA_BLOB);
    PMIX_VALUE_CREATE(kv->value, 1);
    pmix_value_load(kv->value, &pbo, PMIX_BYTE_OBJECT);
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    pmix_list_append(&cd->payload, &kv->super);
#else
    return PMIX_ERR_TAKE_NEXT_OPTION;
#endif

    return PMIX_SUCCESS;
}

static pmix_status_t deliver_inventory(pmix_info_t info[], size_t ninfo,
                                       pmix_info_t directives[], size_t ndirs,
                                       pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_output_verbose(2, pmix_pnet_base_framework.framework_output,
                        "pnet:opa deliver inventory");

    return PMIX_ERR_NOT_SUPPORTED;
}
