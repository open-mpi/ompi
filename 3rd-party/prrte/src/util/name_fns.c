/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <stdio.h>
#include <string.h>

#include "src/threads/pmix_tsd.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_string_copy.h"
#include "src/mca/errmgr/errmgr.h"

#include "src/util/name_fns.h"

#define PRTE_PRINT_NAME_ARGS_MAX_SIZE 1024
#define PRTE_PRINT_NAME_ARG_NUM_BUFS  16

/* constructor - used to initialize namelist instance */
static void prte_namelist_construct(prte_namelist_t *list)
{
    PMIX_LOAD_PROCID(&list->name, NULL, PMIX_RANK_INVALID);
}

/* define instance of prte_class_t */
PMIX_CLASS_INSTANCE(prte_namelist_t,           /* type name */
                    pmix_list_item_t,          /* parent "class" name */
                    prte_namelist_construct,   /* constructor */
                    NULL); /* destructor */

static bool fns_init = false;

static pmix_tsd_key_t print_args_tsd_key;
char *prte_print_args_null = "NULL";
typedef struct {
    char *buffers[PRTE_PRINT_NAME_ARG_NUM_BUFS];
    int cntr;
} prte_print_args_buffers_t;

static void buffer_cleanup(void *value)
{
    int i;
    prte_print_args_buffers_t *ptr;

    if (NULL != value) {
        ptr = (prte_print_args_buffers_t *) value;
        for (i = 0; i < PRTE_PRINT_NAME_ARG_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
        free(ptr);
    }
}

static prte_print_args_buffers_t *get_print_name_buffer(void)
{
    prte_print_args_buffers_t *ptr;
    int ret, i;

    if (!fns_init) {
        /* setup the print_args function */
        if (PRTE_SUCCESS != (ret = pmix_tsd_key_create(&print_args_tsd_key, buffer_cleanup))) {
            PRTE_ERROR_LOG(ret);
            return NULL;
        }
        fns_init = true;
    }

    ret = pmix_tsd_getspecific(print_args_tsd_key, (void **) &ptr);
    if (PRTE_SUCCESS != ret)
        return NULL;

    if (NULL == ptr) {
        ptr = (prte_print_args_buffers_t *) malloc(sizeof(prte_print_args_buffers_t));
        for (i = 0; i < PRTE_PRINT_NAME_ARG_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((PRTE_PRINT_NAME_ARGS_MAX_SIZE + 1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = pmix_tsd_setspecific(print_args_tsd_key, (void *) ptr);
    }

    return (prte_print_args_buffers_t *) ptr;
}

char *prte_util_print_name_args(const pmix_proc_t *name)
{
    prte_print_args_buffers_t *ptr;
    char *job, *vpid;

    /* protect against NULL names */
    if (NULL == name) {
        /* get the next buffer */
        ptr = get_print_name_buffer();
        if (NULL == ptr) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            return prte_print_args_null;
        }
        /* cycle around the ring */
        if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
            ptr->cntr = 0;
        }
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "[NO-NAME]");
        return ptr->buffers[ptr->cntr - 1];
    }

    /* get the jobid, vpid strings first - this will protect us from
     * stepping on each other's buffer. This also guarantees
     * that the print_args function has been initialized, so
     * we don't need to duplicate that here
     */
    job = prte_util_print_jobids(name->nspace);
    vpid = prte_util_print_vpids(name->rank);

    /* get the next buffer */
    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return prte_print_args_null;
    }

    /* cycle around the ring */
    if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "[%s,%s]", job, vpid);

    return ptr->buffers[ptr->cntr - 1];
}

char *prte_util_print_jobids(const pmix_nspace_t job)
{
    prte_print_args_buffers_t *ptr;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return prte_print_args_null;
    }

    /* cycle around the ring */
    if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    if (0 == strlen(job)) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "[INVALID]");
    } else {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", job);
    }
    return ptr->buffers[ptr->cntr - 1];
}

char *prte_util_print_job_family(const pmix_nspace_t job)
{
    prte_print_args_buffers_t *ptr;
    char *cptr;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return prte_print_args_null;
    }

    /* cycle around the ring */
    if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    /* see if the job is invalid */
    if (PMIX_NSPACE_INVALID(job)) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "[INVALID]");
    } else {
        /* find the '@' sign delimiting the job family */
        cptr = strrchr(job, '@');
        if (NULL == cptr) {
            /* this isn't a PRRTE job */
            snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", job);
        } else {
            *cptr = '\0';
            snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", job);
            *cptr = '@';
        }
    }
    return ptr->buffers[ptr->cntr - 1];
}

char *prte_util_print_local_jobid(const pmix_nspace_t job)
{
    prte_print_args_buffers_t *ptr;
    char *cptr;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return prte_print_args_null;
    }

    /* cycle around the ring */
    if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    /* see if the job is invalid */
    if (PMIX_NSPACE_INVALID(job)) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "[INVALID]");
    } else {
        /* find the '@' sign delimiting the job family */
        cptr = strrchr(job, '@');
        if (NULL == cptr) {
            /* this isn't a PRRTE job */
            snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", job);
        } else {
            ++cptr;
            snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", cptr);
        }
    }
    return ptr->buffers[ptr->cntr - 1];
}

char *prte_util_print_vpids(const pmix_rank_t vpid)
{
    prte_print_args_buffers_t *ptr;

    ptr = get_print_name_buffer();

    if (NULL == ptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return prte_print_args_null;
    }

    /* cycle around the ring */
    if (PRTE_PRINT_NAME_ARG_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    if (PMIX_RANK_INVALID == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "INVALID");
    } else if (PMIX_RANK_WILDCARD == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "WILDCARD");
    } else if (PMIX_RANK_LOCAL_NODE == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "LOCALNODE");
    } else if (PMIX_RANK_LOCAL_PEERS == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "LOCALPEERS");
    } else if (PMIX_RANK_UNDEF == vpid) {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%s", "UNDEFINED");
    } else {
        snprintf(ptr->buffers[ptr->cntr++], PRTE_PRINT_NAME_ARGS_MAX_SIZE, "%u", vpid);
    }
    return ptr->buffers[ptr->cntr - 1];
}

/***   STRING FUNCTIONS   ***/
int prte_util_convert_vpid_to_string(char **vpid_string, const pmix_rank_t vpid)
{
    /* check for wildcard value - handle appropriately */
    if (PMIX_RANK_WILDCARD == vpid) {
        *vpid_string = strdup("WILDCARD");
    } else if (PMIX_RANK_INVALID == vpid) {
        *vpid_string = strdup("INVALID");
    } else if (PMIX_RANK_LOCAL_NODE == vpid) {
        *vpid_string = strdup("LOCALNODE");
    } else if (PMIX_RANK_LOCAL_PEERS == vpid) {
        *vpid_string = strdup("LOCALPEERS");
    } else if (PMIX_RANK_UNDEF == vpid) {
        *vpid_string = strdup("UNDEFINED");
    } else {
        if (0 > pmix_asprintf(vpid_string, "%u", vpid)) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
    }
    return PRTE_SUCCESS;
}

int prte_util_convert_string_to_process_name(pmix_proc_t *name, const char *name_string)
{
    char *p;

    /* check for NULL string - error */
    if (NULL == name_string) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }

    p = strrchr(name_string, '.'); /** get last field -> vpid */

    /* check for error */
    if (NULL == p) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    *p = '\0';
    PMIX_LOAD_NSPACE(name->nspace, name_string);
    *p = '.';
    ++p;
    name->rank = strtoul(p, NULL, 10);

    return PRTE_SUCCESS;
}

int prte_util_convert_process_name_to_string(char **name_string, const pmix_proc_t *name)
{
    char *job, *rank;

    if (NULL == name) { /* got an error */
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }

    job = prte_util_print_jobids(name->nspace);
    rank = prte_util_print_vpids(name->rank);
    pmix_asprintf(name_string, "%s.%s", job, rank);

    return PRTE_SUCCESS;
}

/****    COMPARE NAME FIELDS     ****/
int prte_util_compare_name_fields(prte_ns_cmp_bitmask_t fields, const pmix_proc_t *name1,
                                  const pmix_proc_t *name2)
{
    /* handle the NULL pointer case */
    if (NULL == name1 && NULL == name2) {
        return PRTE_EQUAL;
    } else if (NULL == name1) {
        return PRTE_VALUE2_GREATER;
    } else if (NULL == name2) {
        return PRTE_VALUE1_GREATER;
    }

    /* in this comparison function, we check for exact equalities.
     * In the case of wildcards, we check to ensure that the fields
     * actually match those values - thus, a "wildcard" in this
     * function does not actually stand for a wildcard value, but
     * rather a specific value - UNLESS the CMP_WILD bitmask value
     * is set
     */

    /* check job id */
    if (PRTE_NS_CMP_JOBID & fields) {
        if (PRTE_NS_CMP_WILD & fields
            && (0 == strlen(name1->nspace) || 0 == strlen(name2->nspace))) {
            goto check_vpid;
        }
        if (strlen(name1->nspace) < strlen(name2->nspace)) {
            return PRTE_VALUE2_GREATER;
        } else if (strlen(name1->nspace) > strlen(name2->nspace)) {
            return PRTE_VALUE1_GREATER;
        }
    }

    /* get here if jobid's are equal, or not being checked
     * now check vpid
     */
check_vpid:
    if (PRTE_NS_CMP_VPID & fields) {
        if (PRTE_NS_CMP_WILD & fields
            && (PMIX_RANK_WILDCARD == name1->rank || PMIX_RANK_WILDCARD == name2->rank)) {
            return PRTE_EQUAL;
        }
        if (name1->rank < name2->rank) {
            return PRTE_VALUE2_GREATER;
        } else if (name1->rank > name2->rank) {
            return PRTE_VALUE1_GREATER;
        }
    }

    /* only way to get here is if all fields are being checked and are equal,
     * or jobid not checked, but vpid equal,
     * only vpid being checked, and equal
     * return that fact
     */
    return PRTE_EQUAL;
}

char *prte_pretty_print_timing(int64_t secs, int64_t usecs)
{
    unsigned long minutes, seconds;
    float fsecs;
    char *timestring;

    seconds = secs + (usecs / 1000000l);
    minutes = seconds / 60l;
    seconds = seconds % 60l;
    if (0 == minutes && 0 == seconds) {
        fsecs = ((float) (secs) *1000000.0 + (float) usecs) / 1000.0;
        pmix_asprintf(&timestring, "%8.2f millisecs", fsecs);
    } else {
        pmix_asprintf(&timestring, "%3lu:%02lu min:sec", minutes, seconds);
    }

    return timestring;
}

char *prte_util_make_version_string(const char *scope, int major, int minor, int release,
                                    const char *greek, const char *repo)
{
    char *str = NULL, *tmp;
    char temp[BUFSIZ];

    temp[BUFSIZ - 1] = '\0';
    if (0 == strcmp(scope, "full") || 0 == strcmp(scope, "all")) {
        snprintf(temp, BUFSIZ - 1, "%d.%d", major, minor);
        str = strdup(temp);
        if (release >= 0) {
            snprintf(temp, BUFSIZ - 1, ".%d", release);
            pmix_asprintf(&tmp, "%s%s", str, temp);
            free(str);
            str = tmp;
        }
        if (NULL != greek) {
            pmix_asprintf(&tmp, "%s%s", str, greek);
            free(str);
            str = tmp;
        }
        if (NULL != repo) {
            pmix_asprintf(&tmp, "%s%s", str, repo);
            free(str);
            str = tmp;
        }
    } else if (0 == strcmp(scope, "major")) {
        snprintf(temp, BUFSIZ - 1, "%d", major);
    } else if (0 == strcmp(scope, "minor")) {
        snprintf(temp, BUFSIZ - 1, "%d", minor);
    } else if (0 == strcmp(scope, "release")) {
        snprintf(temp, BUFSIZ - 1, "%d", release);
    } else if (0 == strcmp(scope, "greek")) {
        str = strdup(greek);
    } else if (0 == strcmp(scope, "repo")) {
        str = strdup(repo);
    }

    if (NULL == str) {
        str = strdup(temp);
    }

    return str;
}
