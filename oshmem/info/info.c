/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"

#include "oshmem/version.h"
#include "oshmem/constants.h"
#include "oshmem/info/info.h"
#include "oshmem/shmem/shmem_api_logger.h"


/*
 * Global variables
 */
oshmem_info_t oshmem_shmem_info_env = {
    false,
    false,
    false,
    256 * 1024 * 1024
};

#define OSHMEM_MAX_LIBRARY_VERSION_STRING 256

/*
 * Local functions
 */
static int oshmem_info_value_to_bool(char *value, bool *interp);
static int oshmem_info_value_to_int(char *value, int *interp);
static int oshmem_info_get_heap_size(char *value, size_t *interp);
static int oshmem_info_get_library_version(char *version, int *len);


/*
 * This function is called during oshmem_init.
 */
int oshmem_info_init(void)
{
    int ret = OSHMEM_SUCCESS;
    char *cptr;

    /* fill the env info object */

    if (NULL != (cptr = getenv(OSHMEM_ENV_VERSION))) {
        ret = oshmem_info_value_to_bool(cptr, &oshmem_shmem_info_env.print_version);
        if (OSHMEM_SUCCESS != ret) {
            goto out;
        }
    }
    if (oshmem_shmem_info_env.print_version && 0 == ORTE_PROC_MY_NAME->vpid) {
        char version[OSHMEM_MAX_LIBRARY_VERSION_STRING];
        int len;

        ret = oshmem_info_get_library_version(version, &len);
        if (OSHMEM_SUCCESS != ret || 0 == len) {
            goto out;
        }
        orte_show_help("help-shmem-runtime.txt",
                       "oshmem_init:print-version",
                       true,
                       version);
    }

    if (NULL != (cptr = getenv(OSHMEM_ENV_INFO))) {
        ret = oshmem_info_value_to_bool(cptr, &oshmem_shmem_info_env.print_info);
        if (OSHMEM_SUCCESS != ret) {
            goto out;
        }
    }
    if (oshmem_shmem_info_env.print_info && 0 == ORTE_PROC_MY_NAME->vpid) {
        orte_show_help("help-shmem-runtime.txt",
                       "oshmem_init:print-info",
                       true,
                       OSHMEM_ENV_VERSION,
                       OSHMEM_ENV_INFO,
                       OSHMEM_ENV_SYMMETRIC_SIZE,
                       OSHMEM_ENV_DEBUG);
    }

    if (NULL != (cptr = getenv(OSHMEM_ENV_DEBUG))) {
        ret = oshmem_info_value_to_bool(cptr, &oshmem_shmem_info_env.debug);
        if (OSHMEM_SUCCESS != ret) {
            goto out;
        }
    }

    if (NULL != (cptr = getenv(OSHMEM_ENV_SYMMETRIC_SIZE))) {
        char *p1 = getenv(SHMEM_HEAP_SIZE);
        if (p1 && strcmp(cptr, p1)) {
            SHMEM_API_ERROR("Found conflict between env '%s' and '%s'.\n",
                          OSHMEM_ENV_SYMMETRIC_SIZE, SHMEM_HEAP_SIZE);
            ret = OSHMEM_ERR_BAD_PARAM;
            goto out;
        }
        ret = oshmem_info_get_heap_size(cptr, &oshmem_shmem_info_env.symmetric_heap_size);
        if (OSHMEM_SUCCESS != ret) {
            goto out;
        }
    } else if (NULL != (cptr = getenv(SHMEM_HEAP_SIZE))) {
        ret = oshmem_info_get_heap_size(cptr, &oshmem_shmem_info_env.symmetric_heap_size);
        if (OSHMEM_SUCCESS != ret) {
            goto out;
        }
    }

    /* All done */

    return OSHMEM_SUCCESS;

out:
    return ret;
}


/*
 * Shut down oshmem_info handling
 */
int oshmem_info_finalize(void)
{

    /* All done -- destroy the table */

    return OSHMEM_SUCCESS;
}


/**
 * Convert value string to boolean
 *
 * Convert value string \c value into a boolean.  The
 * strings "true", "false", and integer numbers can be converted
 * into booleans.  All others will return \c OSHMEM_ERR_BAD_PARAM
 *
 * @param value Value string for info key to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OSHMEM_SUCCESS string was successfully interpreted
 * @retval OSHMEM_ERR_BAD_PARAM string was not able to be interpreted
 */
static int oshmem_info_value_to_bool(char *value, bool *interp)
{
    int tmp;

    /* idiot case */
    if (NULL == value || NULL == interp) return OSHMEM_ERR_BAD_PARAM;

    /* is it true / false? */
    if (0 == strcmp(value, "true")) {
        *interp = true;
        return OSHMEM_SUCCESS;
    } else if (0 == strcmp(value, "false")) {
        *interp = false;
        return OSHMEM_SUCCESS;

    /* is it a number? */
    } else if (OSHMEM_SUCCESS == oshmem_info_value_to_int(value, &tmp)) {
        if (tmp == 0) {
            *interp = false;
        } else {
            *interp = true;
        }
        return OSHMEM_SUCCESS;
    }

    return OSHMEM_ERR_BAD_PARAM;
}


/**
 * Convert value string to integer
 *
 * Convert value string \c value into a integer.
 * All others will return \c OSHMEM_ERR_BAD_PARAM
 *
 * @param value Value string for info key to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OSHMEM_SUCCESS string was successfully interpreted
 * @retval OSHMEM_ERR_BAD_PARAM string was not able to be interpreted
 */
static int oshmem_info_value_to_int(char *value, int *interp)
{
    long tmp;
    char *endp;

    if (NULL == value || '\0' == value[0]) return OSHMEM_ERR_BAD_PARAM;

    errno = 0;
    tmp = strtol(value, &endp, 10);
    /* we found something not a number */
    if (*endp != '\0') return OSHMEM_ERR_BAD_PARAM;
    /* underflow */
    if (tmp == 0 && errno == EINVAL) return OSHMEM_ERR_BAD_PARAM;

    *interp = (int) tmp;

    return OSHMEM_SUCCESS;
}

static int oshmem_info_get_heap_size(char *value, size_t *interp)
{
    char *p;
    long long factor = 1;
    int idx;
    long long size = 0;

    p = value;
    *interp = 0;

    if (!p) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    /* Sanity check for buffer overflow attack (coverity issue: Use of untrusted string value) */
    if (16 < strlen(p)) {
        return OSHMEM_ERR_BAD_PARAM;
    }

    if (1 == sscanf(p, "%lld%n", &size, &idx)) {
        if (p[idx] != '\0') {
            if (p[idx + 1] == '\0') {
                if (p[idx] == 'k' || p[idx] == 'K') {
                    factor = 1024;
                } else if (p[idx] == 'm' || p[idx] == 'M') {
                    factor = 1024 * 1024;
                } else if (p[idx] == 'g' || p[idx] == 'G') {
                    factor = 1024 * 1024 * 1024;
                } else if (p[idx] == 't' || p[idx] == 'T') {
                    factor = 1024UL * 1024UL * 1024UL * 1024UL;
                } else {
                    size = 0;
                }
            } else {
                size = 0;
            }
        }
    }

    if (size <= 0) {
        return OSHMEM_ERR_BAD_PARAM;
    } else {
        opal_setenv(OSHMEM_ENV_SYMMETRIC_SIZE, p, true, &environ);
        opal_setenv(SHMEM_HEAP_SIZE, p, true, &environ);
/* Probably needless code */
#if 0
        char *tmp = p;

        if(!p) {
            asprintf(&tmp, "%lld", size * factor);
        }

        if (tmp) {
            opal_setenv(OSHMEM_ENV_SYMMETRIC_SIZE, p, true, &environ);
            opal_setenv(SHMEM_HEAP_SIZE, p, true, &environ);
        }

        if (!p && tmp) {
            free(tmp);
        }
#endif
    }

    *interp = size * factor;

    return OSHMEM_SUCCESS;
}

static int oshmem_info_get_library_version(char *version, int *resultlen)
{
    int len_left;
    char *ptr, tmp[OSHMEM_MAX_LIBRARY_VERSION_STRING];

    ptr = tmp;
    len_left = sizeof(tmp);
    memset(tmp, 0, OSHMEM_MAX_LIBRARY_VERSION_STRING);

    snprintf(tmp, OSHMEM_MAX_LIBRARY_VERSION_STRING, "Open SHMEM v%d.%d.%d",
             OSHMEM_MAJOR_VERSION, OSHMEM_MINOR_VERSION, OSHMEM_RELEASE_VERSION);
    ptr += strlen(tmp);
    len_left -= strlen(tmp);

    if (strlen(OSHMEM_GREEK_VERSION) > 0) {
        snprintf(ptr, len_left, "%s", OSHMEM_GREEK_VERSION);
        ptr = tmp + strlen(tmp);
        len_left = OSHMEM_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Package name */
    if (strlen(OPAL_PACKAGE_STRING) > 0) {
        snprintf(ptr, len_left, ", package: %s", OPAL_PACKAGE_STRING);
        ptr = tmp + strlen(tmp);
        len_left = OSHMEM_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Ident string */
    if (strlen(OSHMEM_IDENT_STRING) > 0) {
        snprintf(ptr, len_left, ", ident: %s", OSHMEM_IDENT_STRING);
        ptr = tmp + strlen(tmp);
        len_left = OSHMEM_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Repository revision */
    if (strlen(OSHMEM_REPO_REV) > 0) {
        snprintf(ptr, len_left, ", repo rev: %s", OSHMEM_REPO_REV);
        ptr = tmp + strlen(tmp);
        len_left = OSHMEM_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    /* Release date */
    if (strlen(OSHMEM_RELEASE_DATE) > 0) {
        snprintf(ptr, len_left, ", %s", OSHMEM_RELEASE_DATE);
        ptr = tmp + strlen(tmp);
        len_left = OSHMEM_MAX_LIBRARY_VERSION_STRING - strlen(tmp);
    }

    memcpy(version, tmp, strlen(tmp) + 1);
    *resultlen = strlen(tmp) + 1;

    return OSHMEM_SUCCESS;
}
