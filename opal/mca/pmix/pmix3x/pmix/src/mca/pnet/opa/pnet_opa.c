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

#include "src/mca/pnet/pnet.h"
#include "src/mca/pnet/base/base.h"
#include "pnet_opa.h"

static pmix_status_t opa_init(void);
static void opa_finalize(void);
static pmix_status_t setup_app(pmix_nspace_t *nptr,
                               pmix_info_t info[], size_t ninfo,
                               pmix_list_t *ilist);
static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(pmix_nspace_t *nptr, char ***env);
static void child_finalized(pmix_peer_t *peer);
static void local_app_finalized(char *nspace);

pmix_pnet_module_t pmix_opa_module = {
    .init = opa_init,
    .finalize = opa_finalize,
    .setup_app = setup_app,
    .setup_local_network = setup_local_network,
    .setup_fork = setup_fork,
    .child_finalized = child_finalized,
    .local_app_finalized = local_app_finalized
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
    }

    if (envars) {
        /* harvest envars to pass along */
        if (NULL != mca_pnet_opa_component.include) {
            for (j=0; NULL != mca_pnet_opa_component.include[j]; j++) {
                len = strlen(mca_pnet_opa_component.include[j]);
                if ('*' == mca_pnet_opa_component.include[j][len-1]) {
                    --len;
                }
                for (i = 0; NULL != environ[i]; ++i) {
                    if (0 == strncmp(environ[i], mca_pnet_opa_component.include[j], len)) {
                        cs_env = strdup(environ[i]);
                        kv = PMIX_NEW(pmix_kval_t);
                        if (NULL == kv) {
                            return PMIX_ERR_OUT_OF_RESOURCE;
                        }
                        kv->key = strdup(PMIX_SET_ENVAR);
                        kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
                        if (NULL == kv->value) {
                            PMIX_RELEASE(kv);
                            return PMIX_ERR_OUT_OF_RESOURCE;
                        }
                        kv->value->type = PMIX_ENVAR;
                        string_key = strchr(cs_env, '=');
                        *string_key = '\0';
                        ++string_key;
                        PMIX_ENVAR_LOAD(&kv->value->data.envar, cs_env, string_key, ':');
                        pmix_list_append(ilist, &kv->super);
                        free(cs_env);
                    }
                }
            }
        }
        /* now check the exclusions and remove any that match */
        if (NULL != mca_pnet_opa_component.exclude) {
            for (j=0; NULL != mca_pnet_opa_component.exclude[j]; j++) {
                len = strlen(mca_pnet_opa_component.exclude[j]);
                if ('*' == mca_pnet_opa_component.exclude[j][len-1]) {
                    --len;
                }
                PMIX_LIST_FOREACH_SAFE(kv, next, ilist, pmix_kval_t) {
                    if (0 == strncmp(kv->value->data.envar.envar, mca_pnet_opa_component.exclude[j], len)) {
                        pmix_list_remove_item(ilist, &kv->super);
                        PMIX_RELEASE(kv);
                    }
                }
            }
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t setup_local_network(pmix_nspace_t *nptr,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    size_t n;
    pmix_status_t rc;
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

static pmix_status_t setup_fork(pmix_nspace_t *nptr, char ***env)
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

static void child_finalized(pmix_peer_t *peer)
{

}

static void local_app_finalized(char *nspace)
{

}
