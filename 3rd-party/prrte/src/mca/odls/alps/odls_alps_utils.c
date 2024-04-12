/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "prte_config.h"
#include "constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <ctype.h>
#include <sys/syscall.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/util/pmix_environ.h"

#include "src/mca/odls/alps/odls_alps.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/odls/odls.h"

int prte_odls_alps_get_rdma_creds(void)
{
    int alps_status = 0, num_creds, i, len;
    uint64_t apid;
    size_t alps_count;
    int ret = PRTE_SUCCESS;
    alpsAppLLIGni_t *rdmacred_rsp = NULL;
    alpsAppGni_t *rdmacred_buf;
    char *ptr;
    char env_buffer[1024];
    static int already_got_creds = 0;

    /*
     * If we already put the GNI RDMA credentials into prte_launch_environ,
     * no need to do anything.
     * TODO: kind of ugly, need to implement an prte_getenv
     */

    if (1 == already_got_creds) {
        return PRTE_SUCCESS;
    }

    /*
     * get the Cray HSN RDMA credentials here and stuff them in to the
     * PMI env variable format expected by uGNI consumers like the uGNI
     * BTL, etc. Stuff into the prte_launch_environ to make sure the
     * application processes can actually use the HSN API (uGNI).
     */

    ret = alps_app_lli_lock();

    /*
     * First get our apid
     */

    ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_APID, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_put_request returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    ret = alps_app_lli_get_response(&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_get_response returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), alps_status));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit;
    }

    ret = alps_app_lli_get_response_bytes(&apid, sizeof(apid));
    if (ALPS_APP_LLI_ALPS_STAT_OK != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_get_response_bytes returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit;
    }

    /*
     * now get the GNI rdma credentials info
     */

    ret = alps_app_lli_put_request(ALPS_APP_LLI_ALPS_REQ_GNI, NULL, 0);
    if (ALPS_APP_LLI_ALPS_STAT_OK != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_put_request returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = PRTE_ERR_FILE_WRITE_FAILURE;
        goto fn_exit;
    }

    ret = alps_app_lli_get_response(&alps_status, &alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != alps_status) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_get_response returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), alps_status));
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit;
    }

    rdmacred_rsp = (alpsAppLLIGni_t *) malloc(alps_count);
    if (NULL == rdmacred_rsp) {
        ret = PRTE_ERR_OUT_OF_RESOURCE;
        goto fn_exit;
    }

    memset(rdmacred_rsp, 0, alps_count);

    ret = alps_app_lli_get_response_bytes(rdmacred_rsp, alps_count);
    if (ALPS_APP_LLI_ALPS_STAT_OK != ret) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: alps_app_lli_get_response_bytes returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        free(rdmacred_rsp);
        ret = PRTE_ERR_FILE_READ_FAILURE;
        goto fn_exit;
    }

    ret = alps_app_lli_unlock();

    rdmacred_buf = (alpsAppGni_t *) (rdmacred_rsp->u.buf);

    /*
     * now set up the env. variables -
     * The cray pmi sets up 4 environment variables:
     * PMI_GNI_DEV_ID - format (id0:id1....idX)
     * PMI_GNI_LOC_ADDR - format (locaddr0:locaddr1:....locaddrX)
     * PMI_GNI_COOKIE - format (cookie0:cookie1:...cookieX)
     * PMI_GNI_PTAG - format (ptag0:ptag1:....ptagX)
     *
     * where X == num_creds - 1
     *
     * TODO: need in theory to check for possible overrun of env_buffer
     */

    num_creds = rdmacred_rsp->count;

    /*
     * first build ptag env
     */

    memset(env_buffer, 0, sizeof(env_buffer));
    ptr = env_buffer;
    for (i = 0; i < num_creds - 1; i++) {
        len = sprintf(ptr, "%d:", rdmacred_buf[i].ptag);
        ptr += len;
    }
    sprintf(ptr, "%d", rdmacred_buf[num_creds - 1].ptag);
    ret = PMIX_SETENV_COMPAT("PMI_GNI_PTAG", env_buffer, false, &prte_launch_environ);
    if (ret != PMIX_SUCCESS) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMIX_SETENV_COMPAT for PMI_GNI_TAG failed - returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = prte_pmix_convert_status(ret);
        goto fn_exit;

    } else {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMI_GNI_TAG = %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             env_buffer));
    }

    /*
     * the cookie env
     */

    memset(env_buffer, 0, sizeof(env_buffer));
    ptr = env_buffer;
    for (i = 0; i < num_creds - 1; i++) {
        len = sprintf(ptr, "%d:", rdmacred_buf[i].cookie);
        ptr += len;
    }
    sprintf(ptr, "%d", rdmacred_buf[num_creds - 1].cookie);
    ret = PMIX_SETENV_COMPAT("PMI_GNI_COOKIE", env_buffer, false, &prte_launch_environ);
    if (ret != PMIX_SUCCESS) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMIX_SETENV_COMPAT for PMI_GNI_COOKIE returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = prte_pmix_convert_status(ret);
        goto fn_exit;

    } else {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMI_GNI_COOKIE = %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), env_buffer));
    }

    /*
     * nic loc addrs
     */

    memset(env_buffer, 0, sizeof(env_buffer));
    ptr = env_buffer;
    for (i = 0; i < num_creds - 1; i++) {
        len = sprintf(ptr, "%d:", rdmacred_buf[i].local_addr);
        ptr += len;
    }
    sprintf(ptr, "%d", rdmacred_buf[num_creds - 1].local_addr);
    ret = PMIX_SETENV_COMPAT("PMI_GNI_LOC_ADDR", env_buffer, false, &prte_launch_environ);
    if (ret != PMIX_SUCCESS) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMIX_SETENV_COMPAT for PMI_GNI_LOC_ADDR returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = prte_pmix_convert_status(ret);
        goto fn_exit;

    } else {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMI_GNI_LOC_ADDR = %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), env_buffer));
    }

    /*
     * finally device ids
     */

    memset(env_buffer, 0, sizeof(env_buffer));
    ptr = env_buffer;
    for (i = 0; i < num_creds - 1; i++) {
        len = sprintf(ptr, "%d:", rdmacred_buf[i].device_id);
        ptr += len;
    }
    sprintf(ptr, "%d", rdmacred_buf[num_creds - 1].device_id);
    ret = PMIX_SETENV_COMPAT("PMI_GNI_DEV_ID", env_buffer, false, &prte_launch_environ);
    if (ret != PMIX_SUCCESS) {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMIX_SETENV_COMPAT for PMI_GNI_DEV_ID returned %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret));
        ret = prte_pmix_convert_status(ret);
        goto fn_exit;

    } else {
        PMIX_OUTPUT_VERBOSE((20, prte_odls_base_framework.framework_output,
                             "%s odls:alps: PMI_GNI_DEV_ID = %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), env_buffer));
    }

fn_exit:
    if (PRTE_SUCCESS == ret)
        already_got_creds = 1;
    return ret;
}
