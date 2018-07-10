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
#include "pnet_opa.h"

static pmix_status_t opa_init(void);
static void opa_finalize(void);
static pmix_status_t setup_app(char *nspace, pmix_list_t *ilist);
static pmix_status_t setup_local_network(char *nspace,
                                         pmix_info_t info[],
                                         size_t ninfo);
static pmix_status_t setup_fork(const pmix_proc_t *peer, char ***env);
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
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pnet: opa init");
    return PMIX_SUCCESS;
}

static void opa_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
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

static pmix_status_t setup_app(char *nspace, pmix_list_t *ilist)
{
    uint64_t unique_key[2];
    char *string_key, *cs_env;
    int fd_rand;
    size_t bytes_read;
    pmix_kval_t *kv;

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

    if (PMIX_SUCCESS != pmix_mca_base_var_env_name("pmix_precondition_transports", &cs_env)) {
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
    kv->value->type = PMIX_STRING;
    if (0 > asprintf(&kv->value->data.string, "%s=%s", cs_env, string_key)) {
        free(string_key);
        free(cs_env);
        PMIX_RELEASE(kv);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    pmix_list_append(ilist, &kv->super);
    free(cs_env);
    free(string_key);

    return PMIX_SUCCESS;
}

static pmix_status_t setup_local_network(char *nspace,
                                         pmix_info_t info[],
                                         size_t ninfo)
{
    return PMIX_SUCCESS;
}

static pmix_status_t setup_fork(const pmix_proc_t *peer, char ***env)
{
    return PMIX_SUCCESS;
}

static void child_finalized(pmix_peer_t *peer)
{

}

static void local_app_finalized(char *nspace)
{

}
