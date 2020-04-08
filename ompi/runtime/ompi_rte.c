/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2012-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018-2020 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 */
#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif  /* HAVE_PWD_H */

#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/os_path.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/mca/threads/tsd.h"
#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"

#include "ompi/runtime/ompi_rte.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"

/* storage to support OMPI */
opal_process_name_t pmix_name_wildcard = {UINT32_MAX-1, UINT32_MAX-1};
opal_process_name_t pmix_name_invalid = {UINT32_MAX, UINT32_MAX};
hwloc_cpuset_t ompi_proc_applied_binding = NULL;
pmix_process_info_t pmix_process_info = {
    .my_name = {OPAL_JOBID_INVALID, OPAL_VPID_INVALID},
    .myprocid = {{0}, PMIX_RANK_INVALID},
    .nodename = NULL,
    .pid = 0,
    .top_session_dir = NULL,
    .job_session_dir = NULL,
    .proc_session_dir = NULL,
    .my_local_rank = 0,
    .my_node_rank = 0,
    .my_numa_rank = UINT16_MAX,     /* Assume invalid NUMA rank, set to UINT16_MAX */
    .num_local_peers = 0,
    .num_procs = 0,
    .app_num = 0,
    .univ_size = 0,
    .app_sizes = NULL,
    .app_ldrs = NULL,
    .cpuset = NULL,
    .command = NULL,
    .num_apps = 0,
    .initial_wdir = NULL,
    .reincarnation = 0
};
bool pmix_proc_is_bound = false;
bool ompi_singleton = false;

static int _setup_top_session_dir(char **sdir);
static int _setup_job_session_dir(char **sdir);
static int _setup_proc_session_dir(char **sdir);

#define OPAL_SCHEMA_DELIMITER_CHAR      '.'
#define OPAL_SCHEMA_WILDCARD_CHAR       '*'
#define OPAL_SCHEMA_WILDCARD_STRING     "*"
#define OPAL_SCHEMA_INVALID_CHAR        '$'
#define OPAL_SCHEMA_INVALID_STRING      "$"

#define OPAL_PRINT_NAME_ARGS_MAX_SIZE   50
#define OPAL_PRINT_NAME_ARG_NUM_BUFS    16

static bool fns_init=false;
static opal_tsd_key_t print_args_tsd_key;
static char* opal_print_args_null = "NULL";
typedef struct {
    char *buffers[OPAL_PRINT_NAME_ARG_NUM_BUFS];
    int cntr;
} opal_print_args_buffers_t;

static void
buffer_cleanup(void *value)
{
    int i;
    opal_print_args_buffers_t *ptr;

    if (NULL != value) {
        ptr = (opal_print_args_buffers_t*)value;
        for (i=0; i < OPAL_PRINT_NAME_ARG_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
        free (ptr);
    }
}

static opal_print_args_buffers_t*
get_print_name_buffer(void)
{
    opal_print_args_buffers_t *ptr;
    int ret, i;

    if (!fns_init) {
        /* setup the print_args function */
        if (OPAL_SUCCESS != (ret = opal_tsd_key_create(&print_args_tsd_key, buffer_cleanup))) {
            OPAL_ERROR_LOG(ret);
            return NULL;
        }
        fns_init = true;
    }

    ret = opal_tsd_getspecific(print_args_tsd_key, (void**)&ptr);
    if (OPAL_SUCCESS != ret) return NULL;

    if (NULL == ptr) {
        ptr = (opal_print_args_buffers_t*)malloc(sizeof(opal_print_args_buffers_t));
        for (i=0; i < OPAL_PRINT_NAME_ARG_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((OPAL_PRINT_NAME_ARGS_MAX_SIZE+1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = opal_tsd_setspecific(print_args_tsd_key, (void*)ptr);
    }

    return (opal_print_args_buffers_t*) ptr;
}

static char* ompi_pmix_print_jobids(const opal_jobid_t job)
{
    opal_print_args_buffers_t *ptr;
    unsigned long tmp1, tmp2;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return opal_print_args_null;
    }

    /* cycle around the ring */
    if (OPAL_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    if (OPAL_JOBID_INVALID == job) {
        snprintf(ptr->buffers[ptr->cntr++], OPAL_PRINT_NAME_ARGS_MAX_SIZE, "[INVALID]");
    } else if (OPAL_JOBID_WILDCARD == job) {
        snprintf(ptr->buffers[ptr->cntr++], OPAL_PRINT_NAME_ARGS_MAX_SIZE, "[WILDCARD]");
    } else {
        tmp1 = OMPI_JOB_FAMILY((unsigned long)job);
        tmp2 = OMPI_LOCAL_JOBID((unsigned long)job);
        snprintf(ptr->buffers[ptr->cntr++],
                 OPAL_PRINT_NAME_ARGS_MAX_SIZE,
                 "[%lu,%lu]", tmp1, tmp2);
    }
    return ptr->buffers[ptr->cntr-1];
}

static char* ompi_pmix_print_vpids(const opal_vpid_t vpid)
{
    opal_print_args_buffers_t *ptr;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return opal_print_args_null;
    }

    /* cycle around the ring */
    if (OPAL_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    if (OPAL_VPID_INVALID == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], OPAL_PRINT_NAME_ARGS_MAX_SIZE, "INVALID");
    } else if (OPAL_VPID_WILDCARD == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], OPAL_PRINT_NAME_ARGS_MAX_SIZE, "WILDCARD");
    } else {
        snprintf(ptr->buffers[ptr->cntr++],
                 OPAL_PRINT_NAME_ARGS_MAX_SIZE,
                 "%ld", (long)vpid);
    }
    return ptr->buffers[ptr->cntr-1];
}

char* ompi_pmix_print_name(const ompi_process_name_t *name)
{
    opal_print_args_buffers_t *ptr;
    char *job, *vpid;

    /* protect against NULL names */
    if (NULL == name) {
        /* get the next buffer */
        ptr = get_print_name_buffer();
        if (NULL == ptr) {
            OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
            return opal_print_args_null;
        }
        /* cycle around the ring */
        if (OPAL_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
            ptr->cntr = 0;
        }
        snprintf(ptr->buffers[ptr->cntr++], OPAL_PRINT_NAME_ARGS_MAX_SIZE, "[NO-NAME]");
        return ptr->buffers[ptr->cntr-1];
    }

    /* get the jobid, vpid strings first - this will protect us from
     * stepping on each other's buffer. This also guarantees
     * that the print_args function has been initialized, so
     * we don't need to duplicate that here
     */
    job = ompi_pmix_print_jobids(name->jobid);
    vpid = ompi_pmix_print_vpids(name->vpid);

    /* get the next buffer */
    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return opal_print_args_null;
    }

    /* cycle around the ring */
    if (OPAL_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    snprintf(ptr->buffers[ptr->cntr++],
             OPAL_PRINT_NAME_ARGS_MAX_SIZE,
             "[%s,%s]", job, vpid);

    return ptr->buffers[ptr->cntr-1];
}

int ompi_rte_compare_name_fields(ompi_rte_cmp_bitmask_t fields,
                                 const opal_process_name_t* name1,
                                 const opal_process_name_t* name2)
{
    /* handle the NULL pointer case */
    if (NULL == name1 && NULL == name2) {
        return OPAL_EQUAL;
    } else if (NULL == name1) {
        return OPAL_VALUE2_GREATER;
    } else if (NULL == name2) {
        return OPAL_VALUE1_GREATER;
    }

    /* in this comparison function, we check for exact equalities.
     * In the case of wildcards, we check to ensure that the fields
     * actually match those values - thus, a "wildcard" in this
     * function does not actually stand for a wildcard value, but
     * rather a specific value - UNLESS the CMP_WILD bitmask value
     * is set
    */

    /* check job id */
    if (OMPI_RTE_CMP_JOBID & fields) {
        if (OMPI_RTE_CMP_WILD & fields &&
            (pmix_name_wildcard.jobid == name1->jobid ||
             pmix_name_wildcard.jobid == name2->jobid)) {
            goto check_vpid;
        }
        if (name1->jobid < name2->jobid) {
            return OPAL_VALUE2_GREATER;
        } else if (name1->jobid > name2->jobid) {
            return OPAL_VALUE1_GREATER;
        }
    }

    /* get here if jobid's are equal, or not being checked
     * now check vpid
     */
  check_vpid:
    if (OMPI_RTE_CMP_VPID & fields) {
        if (OMPI_RTE_CMP_WILD & fields &&
            (pmix_name_wildcard.vpid == name1->vpid ||
             pmix_name_wildcard.vpid == name2->vpid)) {
            return OPAL_EQUAL;
        }
        if (name1->vpid < name2->vpid) {
            return OPAL_VALUE2_GREATER;
        } else if (name1->vpid > name2->vpid) {
            return OPAL_VALUE1_GREATER;
        }
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return OPAL_EQUAL;
}

int ompi_rte_convert_string_to_process_name(opal_process_name_t *name,
                                            const char* name_string)
{
    char *temp, *token;
    opal_jobid_t job;
    opal_vpid_t vpid;
    int return_code=OPAL_SUCCESS;

    /* set default */
    name->jobid = pmix_name_invalid.jobid;
    name->vpid = pmix_name_invalid.vpid;

    /* check for NULL string - error */
    if (NULL == name_string) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    temp = strdup(name_string);  /** copy input string as the strtok process is destructive */
    token = strchr(temp, OPAL_SCHEMA_DELIMITER_CHAR); /** get first field -> jobid */

    /* check for error */
    if (NULL == token) {
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        free(temp);
        return OPAL_ERR_BAD_PARAM;
    }
    *token = '\0';
    token++;

    /* check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(temp, OPAL_SCHEMA_WILDCARD_STRING)) {
        job = pmix_name_wildcard.jobid;
    } else if (0 == strcmp(temp, OPAL_SCHEMA_INVALID_STRING)) {
        job = pmix_name_invalid.jobid;
    } else {
        job = strtoul(temp, NULL, 10);
    }

    /* check for WILDCARD character - assign
     * value accordingly, if found
     */
    if (0 == strcmp(token, OPAL_SCHEMA_WILDCARD_STRING)) {
        vpid = pmix_name_wildcard.vpid;
    } else if (0 == strcmp(token, OPAL_SCHEMA_INVALID_STRING)) {
        vpid = pmix_name_invalid.vpid;
    } else {
        vpid = strtoul(token, NULL, 10);
    }

    name->jobid = job;
    name->vpid = vpid;

    free(temp);

    return return_code;
}

int ompi_rte_convert_process_name_to_string(char** name_string,
                                            const opal_process_name_t *name)
{
    char *tmp, *tmp2;

    if (NULL == name) { /* got an error */
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        return OPAL_ERR_BAD_PARAM;
    }

    /* check for wildcard and invalid values - where encountered, insert the
     * corresponding string so we can correctly parse the name string when
     * it is passed back to us later
     */
    if (pmix_name_wildcard.jobid == name->jobid) {
        opal_asprintf(&tmp, "%s", OPAL_SCHEMA_WILDCARD_STRING);
    } else if (pmix_name_invalid.jobid == name->jobid) {
        opal_asprintf(&tmp, "%s", OPAL_SCHEMA_INVALID_STRING);
    } else {
        opal_asprintf(&tmp, "%lu", (unsigned long)name->jobid);
    }

    if (pmix_name_wildcard.vpid == name->vpid) {
        opal_asprintf(&tmp2, "%s%c%s", tmp, OPAL_SCHEMA_DELIMITER_CHAR, OPAL_SCHEMA_WILDCARD_STRING);
    } else if (pmix_name_invalid.vpid == name->vpid) {
        opal_asprintf(&tmp2, "%s%c%s", tmp, OPAL_SCHEMA_DELIMITER_CHAR, OPAL_SCHEMA_INVALID_STRING);
    } else {
        opal_asprintf(&tmp2, "%s%c%lu", tmp, OPAL_SCHEMA_DELIMITER_CHAR, (unsigned long)name->vpid);
    }

    opal_asprintf(name_string, "%s", tmp2);

    free(tmp);
    free(tmp2);

    return OPAL_SUCCESS;
}

static int ompi_pmix_convert_string_to_jobid(opal_jobid_t *jobid, const char* jobidstring)
{
    if (NULL == jobidstring) {  /* got an error */
        OPAL_ERROR_LOG(OPAL_ERR_BAD_PARAM);
        *jobid = OPAL_JOBID_INVALID;
        return OPAL_ERR_BAD_PARAM;
    }

    /** check for wildcard character - handle appropriately */
    if (0 == strcmp(OPAL_SCHEMA_WILDCARD_STRING, jobidstring)) {
        *jobid = OPAL_JOBID_WILDCARD;
        return OPAL_SUCCESS;
    }

    /* check for invalid value */
    if (0 == strcmp(OPAL_SCHEMA_INVALID_STRING, jobidstring)) {
        *jobid = OPAL_JOBID_INVALID;
        return OPAL_SUCCESS;
    }

    *jobid = strtoul(jobidstring, NULL, 10);

    return OPAL_SUCCESS;
}

static int ompi_pmix_snprintf_jobid(char *jobid_string, size_t size, const opal_jobid_t jobid)
{
    int rc;

    /* check for wildcard value - handle appropriately */
    if (OPAL_JOBID_WILDCARD == jobid) {
        (void)opal_string_copy(jobid_string,
                               OPAL_SCHEMA_WILDCARD_STRING, size);
    } else {
        rc = snprintf(jobid_string, size, "%ld", (long) jobid);
        if (0 > rc) {
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}


/**
 * Static functions used to configure the interactions between the OPAL and
 * the runtime.
 */

static char*
_process_name_print_for_opal(const opal_process_name_t procname)
{
    ompi_process_name_t* rte_name = (ompi_process_name_t*)&procname;
    return ompi_pmix_print_name(rte_name);
}

static char*
_jobid_print_for_opal(const opal_jobid_t jobid)
{
    return ompi_pmix_print_jobids(jobid);
}

static char*
_vpid_print_for_opal(const opal_vpid_t vpid)
{
    return ompi_pmix_print_vpids(vpid);
}

static int
_process_name_compare(const opal_process_name_t p1, const opal_process_name_t p2)
{
    return ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL, &p1, &p2);
}

static int _convert_string_to_process_name(opal_process_name_t *name,
                                           const char* name_string)
{
    return ompi_rte_convert_string_to_process_name(name, name_string);
}

static int _convert_process_name_to_string(char** name_string,
                                          const opal_process_name_t *name)
{
    return ompi_rte_convert_process_name_to_string(name_string, name);
}

static int
_convert_string_to_jobid(opal_jobid_t *jobid, const char *jobid_string)
{
    return ompi_pmix_convert_string_to_jobid(jobid, jobid_string);
}

int ompi_rte_init(int *pargc, char ***pargv)
{
    int ret;
    char *error = NULL;
    opal_process_name_t pname;
    pmix_proc_t rproc;
    uint32_t u32, *u32ptr;
    uint16_t u16, *u16ptr;
    char **peers=NULL;
    char *ev1;
    char *val;
    size_t i;
    pmix_value_t pval;
    pmix_status_t rc;
    char **tmp;

    u32ptr = &u32;
    u16ptr = &u16;
    memset(&pmix_process_info, 0, sizeof(pmix_process_info));

    /* Convince OPAL to use our naming scheme */
    opal_process_name_print = _process_name_print_for_opal;
    opal_vpid_print = _vpid_print_for_opal;
    opal_jobid_print = _jobid_print_for_opal;
    opal_compare_proc = _process_name_compare;
    opal_convert_string_to_process_name = _convert_string_to_process_name;
    opal_convert_process_name_to_string = _convert_process_name_to_string;
    opal_snprintf_jobid = ompi_pmix_snprintf_jobid;
    opal_convert_string_to_jobid = _convert_string_to_jobid;

    /* initialize the opal layer */
    if (OPAL_SUCCESS != (ret = opal_init(pargc, pargv))) {
        error = "opal_init";
        goto error;
    }

    /* setup our internal nspace hack */
    opal_pmix_setup_nspace_tracker();

    /* initialize the selected module */
    if (!PMIx_Initialized() && (PMIX_SUCCESS != (ret = PMIx_Init(&pmix_process_info.myprocid, NULL, 0)))) {
        /* if we get PMIX_ERR_UNREACH indicating that we cannot reach the
         * server, then we assume we are operating as a singleton */
        if (PMIX_ERR_UNREACH == ret) {
            ompi_singleton = true;
        } else {
            /* we cannot run - this could be due to being direct launched
             * without the required PMI support being built, so print
             * out a help message indicating it */
            opal_show_help("help-mpi-runtime.txt", "no-pmi", true, PMIx_Error_string(ret));
            return OPAL_ERR_SILENT;
        }
    }

    /* setup the process name fields - also registers the new nspace */
    OPAL_PMIX_CONVERT_PROCT(rc, &pname, &pmix_process_info.myprocid);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
    OPAL_PROC_MY_NAME.jobid = pname.jobid;
    OPAL_PROC_MY_NAME.vpid = pname.vpid;
    pmix_process_info.my_name.jobid = OPAL_PROC_MY_NAME.jobid;
    pmix_process_info.my_name.vpid = OPAL_PROC_MY_NAME.vpid;

    /* set our hostname */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_HOSTNAME, &OPAL_PROC_MY_NAME,
                                   (char**)&ev1, PMIX_STRING);
    if (PMIX_SUCCESS == ret) {
        if (NULL != opal_process_info.nodename) {
            free(opal_process_info.nodename);
        }
        opal_process_info.nodename = ev1;  // ev1 is an allocated string
    }
    pmix_process_info.nodename = opal_process_info.nodename;

    /* get our local rank from PMIx */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCAL_RANK,
                                   &pmix_process_info.my_name, &u16ptr, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        if (ompi_singleton) {
            /* just assume 0 */
            u16 = 0;
        } else {
            ret = opal_pmix_convert_status(rc);
            error = "local rank";
            goto error;
        }
    }
    pmix_process_info.my_local_rank = u16;

    /* get our node rank from PMIx */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_NODE_RANK,
                                   &pmix_process_info.my_name, &u16ptr, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        if (ompi_singleton) {
            /* just assume 0 */
            u16 = 0;
        } else {
            ret = opal_pmix_convert_status(rc);
            error = "node rank";
            goto error;
        }
    }
    pmix_process_info.my_node_rank = u16;

    /* get job size */
    pname.jobid = pmix_process_info.my_name.jobid;
    pname.vpid = OPAL_VPID_WILDCARD;
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_JOB_SIZE,
                                   &pname, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        if (ompi_singleton) {
            /* just assume 1 */
            u32 = 1;
        } else {
            ret = opal_pmix_convert_status(rc);
            error = "job size";
            goto error;
        }
    }
    pmix_process_info.num_procs = u32;

    /* get universe size */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_UNIV_SIZE,
                                   &pname, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        if (ompi_singleton) {
            /* just assume 1 */
            u32 = 1;
        } else {
            /* default to job size */
            u32 = pmix_process_info.num_procs;
        }
    }
    pmix_process_info.univ_size = u32;

    /* get number of app contexts */
    pname.jobid = pmix_process_info.my_name.jobid;
    pname.vpid = OPAL_VPID_WILDCARD;
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_JOB_NUM_APPS,
                                   &pname, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS == rc) {
        pmix_process_info.num_apps = u32;
    } else {
        pmix_process_info.num_apps = 1;
    }

    /* get our app number from PMIx - ok if not found */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_APPNUM,
                                   &pmix_process_info.my_name, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS == rc) {
        pmix_process_info.app_num = u32;
    } else {
        pmix_process_info.app_num = 0;
    }

    /* if more than one app context, get the number of procs and first rank of each */
    if (1 == pmix_process_info.num_apps) {
        pmix_process_info.app_ldrs = strdup("0");
        opal_asprintf(&pmix_process_info.app_sizes, "%u", pmix_process_info.num_procs);
    } else {
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, "OMPI_APP_SIZES", &pname, &val, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            /* assume it is just us */
            opal_asprintf(&pmix_process_info.app_sizes, "%u", pmix_process_info.num_procs);
        } else {
            pmix_process_info.app_sizes = val;
        }
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, "OMPI_FIRST_RANKS", &pname, &val, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            /* assume it is just us */
            pmix_process_info.app_ldrs = strdup("0");
        } else {
            pmix_process_info.app_ldrs = val;
        }
    }

    /* get our command - defaults to our appnum */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_APP_ARGV,
                                   &pname, (char**)&ev1, PMIX_STRING);
    if (PMIX_SUCCESS == rc) {
        pmix_process_info.command = ev1;  // ev1 is an allocated string
    } else if (NULL != pargv) {
        tmp = *pargv;
        if (NULL != tmp) {
            pmix_process_info.command = opal_argv_join(tmp, ' ');
        }
    }

    /* get our reincarnation number */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_REINCARNATION,
                                   &OPAL_PROC_MY_NAME, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS == rc) {
        pmix_process_info.reincarnation = u32;
    }

    /* get the number of local peers - required for wireup of
     * shared memory BTL, defaults to local node */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCAL_SIZE,
                                   &pname, &u32ptr, PMIX_UINT32);
    if (PMIX_SUCCESS == rc) {
        pmix_process_info.num_local_peers = u32 - 1;  // want number besides ourselves
    } else {
        ret = opal_pmix_convert_status(rc);
        error = "local size";
        goto error;
    }

    /* retrieve temp directories info */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_TMPDIR, &pname, &val, PMIX_STRING);
    if (OPAL_SUCCESS == rc && NULL != val) {
        pmix_process_info.top_session_dir = val;
    } else {
        /* we need to create something */
        rc = _setup_top_session_dir(&pmix_process_info.top_session_dir);
        if (OPAL_SUCCESS != rc) {
            error = "top session directory";
            goto error;
        }
    }

    /* retrieve job-session directory info */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_NSDIR, &pname, &val, PMIX_STRING);
    if (PMIX_SUCCESS == rc && NULL != val) {
        pmix_process_info.job_session_dir = val;
        val = NULL;
    } else {
        /* we need to create something */
        rc = _setup_job_session_dir(&pmix_process_info.job_session_dir);
        if (OPAL_SUCCESS != rc) {
            error = "job session directory";
            goto error;
        }
    }

    /* retrieve proc-session directory info */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_PROCDIR, &OPAL_PROC_MY_NAME, &val, PMIX_STRING);
    if (OPAL_SUCCESS == rc && NULL != val) {
        pmix_process_info.proc_session_dir = val;
    } else {
        /* we need to create something */
        rc = _setup_proc_session_dir(&pmix_process_info.proc_session_dir);
        if (OPAL_SUCCESS != rc) {
            error = "proc session directory";
            goto error;
        }
    }

    /* get our initial working directory - defaults to getting the value
     * for our app */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_WDIR, &pname, &val, PMIX_STRING);
    if (PMIX_SUCCESS == rc && NULL != val) {
        pmix_process_info.initial_wdir = val;
        val = NULL;
    }

    /* identify our location */
    val = NULL;
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCALITY_STRING,
                                   &pmix_process_info.my_name, &val, PMIX_STRING);
    if (PMIX_SUCCESS == rc && NULL != val) {
        pmix_process_info.cpuset = val;
        pmix_proc_is_bound = true;
    } else {
        pmix_process_info.cpuset = NULL;
        pmix_proc_is_bound = false;
    }

    /* get our numa rank from PMIx */
    if (pmix_proc_is_bound) {
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_NUMA_RANK,
                                       &pmix_process_info.my_name, &u16ptr, PMIX_UINT16);
        if (PMIX_SUCCESS != rc) {
            if (ompi_singleton) {
                /* just assume the numa_rank is invalid, set to UINT16_MAX */
                u16 = UINT16_MAX;
            } else {
                ret = opal_pmix_convert_status(rc);
                error = "numa rank";
                goto error;
            }
        }
        pmix_process_info.my_numa_rank = u16;
    } else {
        /* If processes are not bound, the numa_rank is not available
         * Assign UINT16_MAX to the numa_rank to indicate an invalid value
         */
        pmix_process_info.my_numa_rank = UINT16_MAX;
    }

    /* get our local peers */
    if (0 < pmix_process_info.num_local_peers) {
        /* if my local rank if too high, then that's an error */
        if (pmix_process_info.num_local_peers < pmix_process_info.my_local_rank) {
            ret = OPAL_ERR_BAD_PARAM;
            error = "num local peers";
            goto error;
        }
        /* retrieve the local peers - defaults to local node */
        OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_PEERS,
                              &pname, &val, PMIX_STRING);
        if (PMIX_SUCCESS == rc && NULL != val) {
            peers = opal_argv_split(val, ',');
            free(val);
        } else {
            ret = opal_pmix_convert_status(rc);
            error = "local peers";
            goto error;
        }
    } else {
        peers = NULL;
    }

    /* set the locality */
    if (NULL != peers) {
        pname.jobid = pmix_process_info.my_name.jobid;
        for (i=0; NULL != peers[i]; i++) {
            pname.vpid = strtoul(peers[i], NULL, 10);
            if (pname.vpid == pmix_process_info.my_name.vpid) {
                /* we are fully local to ourselves */
                u16 = OPAL_PROC_ALL_LOCAL;
            } else {
                val = NULL;
                OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCALITY_STRING,
                                               &pname, &val, PMIX_STRING);
                if (PMIX_SUCCESS == rc && NULL != val) {
                    u16 = opal_hwloc_compute_relative_locality(pmix_process_info.cpuset, val);
                    free(val);
                } else {
                    /* all we can say is that it shares our node */
                    u16 = OPAL_PROC_ON_CLUSTER | OPAL_PROC_ON_CU | OPAL_PROC_ON_NODE;
                }
            }
            pval.type = PMIX_UINT16;
            pval.data.uint16 = u16;
            OPAL_PMIX_CONVERT_NAME(&rproc, &pname);
            rc = PMIx_Store_internal(&rproc, PMIX_LOCALITY, &pval);
            if (PMIX_SUCCESS != rc) {
                ret = opal_pmix_convert_status(rc);
                error = "local store of locality";
                opal_argv_free(peers);
                if (NULL != pmix_process_info.cpuset) {
                    free(pmix_process_info.cpuset);
                }
                goto error;
            }
        }
        opal_argv_free(peers);
    }

    /*
     * stdout/stderr buffering
     * If the user requested to override the default setting then do
     * as they wish.
     */
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, "OMPI_STREAM_BUFFERING",
                                   &pmix_process_info.my_name, &u16ptr, PMIX_UINT16);
    if (PMIX_SUCCESS == rc) {
        if (0 == u16) {
            setvbuf(stdout, NULL, _IONBF, 0);
            setvbuf(stderr, NULL, _IONBF, 0);
        } else if (1 == u16) {
            setvbuf(stdout, NULL, _IOLBF, 0);
            setvbuf(stderr, NULL, _IOLBF, 0);
        } else if (2 == u16 ) {
            setvbuf(stdout, NULL, _IOFBF, 0);
            setvbuf(stderr, NULL, _IOFBF, 0);
        }
    }

    /* set the remaining opal_process_info fields. Note that
     * the OPAL layer will have initialized these to NULL, and
     * anyone between us would not have strdup'd the string, so
     * we cannot free it here */
    opal_process_info.top_session_dir  = pmix_process_info.top_session_dir;
    opal_process_info.job_session_dir  = pmix_process_info.job_session_dir;
    opal_process_info.proc_session_dir = pmix_process_info.proc_session_dir;
    opal_process_info.num_local_peers  = (int32_t)pmix_process_info.num_local_peers;
    opal_process_info.my_local_rank    = (int32_t)pmix_process_info.my_local_rank;
    opal_process_info.my_numa_rank     = pmix_process_info.my_numa_rank;
    opal_process_info.cpuset           = pmix_process_info.cpuset;

    return OPAL_SUCCESS;

  error:
    if (OPAL_ERR_SILENT != ret ) {
        opal_show_help("help-mpi-runtime.txt",
                       "mpi_init:startup:internal-failure",
                       true, "MPI runtime init", "RTE init",
                       error, opal_strerror(ret), ret);
    }
    opal_finalize();
    return ret;

}

static bool check_file(const char *root, const char *path)
{
    struct stat st;
    char *fullpath;

    /*
     * Keep:
     *  - non-zero files starting with "output-"
     */
    if (0 == strncmp(path, "output-", strlen("output-"))) {
        fullpath = opal_os_path(false, &fullpath, root, path, NULL);
        stat(fullpath, &st);
        free(fullpath);
        if (0 == st.st_size) {
            return true;
        }
        return false;
    }

    return true;
}

int ompi_rte_finalize(void)
{
    /* shutdown pmix */
    PMIx_Finalize(NULL, 0);

    /* cleanup the session directory we created */
    if (NULL != pmix_process_info.job_session_dir) {
        opal_os_dirpath_destroy(pmix_process_info.job_session_dir,
                                false, check_file);
        free(pmix_process_info.job_session_dir);
        pmix_process_info.job_session_dir = NULL;
    }

    if (NULL != pmix_process_info.top_session_dir) {
        free(pmix_process_info.top_session_dir);
        pmix_process_info.top_session_dir = NULL;
    }

    if (NULL != pmix_process_info.proc_session_dir) {
        free(pmix_process_info.proc_session_dir);
        pmix_process_info.proc_session_dir = NULL;
    }

    if (NULL != pmix_process_info.app_sizes) {
        free(pmix_process_info.app_sizes);
        pmix_process_info.app_sizes = NULL;
    }

    if (NULL != pmix_process_info.app_ldrs) {
        free(pmix_process_info.app_ldrs);
        pmix_process_info.app_ldrs = NULL;
    }

    if (NULL != pmix_process_info.cpuset) {
        free(pmix_process_info.cpuset);
        pmix_process_info.cpuset = NULL;
    }

    if (NULL != pmix_process_info.command) {
        free(pmix_process_info.command);
        pmix_process_info.command = NULL;
    }

    if (NULL != pmix_process_info.initial_wdir) {
        free(pmix_process_info.initial_wdir);
        pmix_process_info.initial_wdir = NULL;
    }

    /* cleanup our internal nspace hack */
    opal_pmix_finalize_nspace_tracker();


    return OMPI_SUCCESS;
}

void ompi_rte_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    char* buffer = NULL;
    struct timespec tp = {0, 100000};

    /* If there was a message, output it */
    va_start(arglist, fmt);
    if( NULL != fmt ) {
        opal_vasprintf( &buffer, fmt, arglist );
    }
    va_end(arglist);

    /* call abort */
    PMIx_Abort(error_code, buffer, NULL, 0);
    if (NULL != buffer) {
        free(buffer);
    }

    /* provide a little delay for the PMIx thread to
     * get the info out */
    nanosleep(&tp, NULL);

    /* Now Exit */
    _exit(error_code);
}

void ompi_rte_abort_peers(opal_process_name_t *procs,
                          int32_t num_procs,
                          int error_code)
{
    return;
}

static size_t handler = SIZE_MAX;
static bool debugger_event_active = true;

static void _release_fn(size_t refid, pmix_status_t status,
                        const pmix_proc_t *source,
                        pmix_info_t info[], size_t ninfo,
                        pmix_info_t *results, size_t nresults,
                        pmix_event_notification_cbfunc_fn_t cbfunc,
                        void *cbdata)
{
    /* must let the notifier know we are done */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    debugger_event_active = false;
}

/*
 * Wait for a debugger if asked.  We support two ways of waiting for
 * attaching debuggers -- see big comment in
 * pmix/tools/pmixrun/debuggers.c explaining the two scenarios.
 */
void ompi_rte_wait_for_debugger(void)
{
    pmix_info_t directive;
    char *evar;
    int time, code = PMIX_ERR_DEBUGGER_RELEASE;

    /* check PMIx to see if we are under a debugger */
    if (NULL == getenv("PMIX_DEBUG_WAIT_FOR_NOTIFY") &&
        NULL == getenv("PMIX_TEST_DEBUGGER_ATTACH")) {
        /* if not, just return */
        return;
    }

    /* if we are being debugged, then we need to find
     * the correct plug-ins
     */
    ompi_debugger_setup_dlls();

    if (NULL != (evar = getenv("PMIX_TEST_DEBUGGER_SLEEP"))) {
        time = strtol(evar, NULL, 10);
        sleep(time);
        return;
    }

    /* register an event handler for the PMIX_ERR_DEBUGGER_RELEASE event */
    PMIX_INFO_LOAD(&directive, PMIX_EVENT_HDLR_NAME, "MPI-DEBUGGER-ATTACH", PMIX_STRING);
    PMIx_Register_event_handler(&code, 1, &directive, 1, _release_fn, NULL, NULL);
    PMIX_INFO_DESTRUCT(&directive);

    /* let the MPI progress engine run while we wait for debugger release */
    OMPI_WAIT_FOR_COMPLETION(debugger_event_active);

    /* deregister the event handler */
    PMIx_Deregister_event_handler(handler, NULL, NULL);
}

static int _setup_top_session_dir(char **sdir)
{
    char *tmpdir;

    if( NULL == (tmpdir = getenv("TMPDIR")) )
        if( NULL == (tmpdir = getenv("TEMP")) )
            if( NULL == (tmpdir = getenv("TMP")) )
                tmpdir = "/tmp";

    *sdir = strdup(tmpdir);
    return OPAL_SUCCESS;
}

static int _setup_job_session_dir(char **sdir)
{
    /* get the effective uid */
    uid_t uid = geteuid();

    if (0 > opal_asprintf(sdir, "%s/ompi.%s.%lu/jf.0/%u",
                          pmix_process_info.top_session_dir,
                          pmix_process_info.nodename,
                          (unsigned long)uid,
                          pmix_process_info.my_name.jobid)) {
        pmix_process_info.job_session_dir = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}

static int _setup_proc_session_dir(char **sdir)
{
    if (0 > opal_asprintf(sdir,  "%s/%d",
                          pmix_process_info.job_session_dir,
                          pmix_process_info.my_name.vpid)) {
        pmix_process_info.proc_session_dir = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    return OPAL_SUCCESS;
}
