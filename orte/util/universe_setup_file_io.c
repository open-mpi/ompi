/*
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * $Id: orte_universe_setup_file I/O functions $
 * 
 */
#include "orte_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "orte/util/univ_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"
#include "orte/util/universe_setup_file_io.h"

#define ORTE_UNIV_SETUP_FILE_MAX_LINE_LENGTH 1024

static char *orte_getline(FILE *fp);

int orte_write_universe_setup_file(char *filename, orte_universe_t *info)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    if (NULL == info->name) {
        /* fatal error - must have a name */
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        fclose(fp);
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    } else {
        fprintf(fp, "%s\n", info->name);
    }

    if (NULL == info->host) {
        fprintf(fp, "LOCALHOST\n");
    } else {
        fprintf(fp, "%s\n", info->host);
    }
    
    if (NULL == info->uid) {
        fprintf(fp, "NO-UID\n");
    } else {
        fprintf(fp, "%s\n", info->uid);
    }

    if (info->persistence) {
        fprintf(fp, "persistent\n");
    } else {
        fprintf(fp, "non-persistent\n");
    }

    if (NULL == info->scope) {
        fprintf(fp, "NO-SCOPE\n");
    } else {
        fprintf(fp, "%s\n", info->scope);
    }

    if (info->console) {
        fprintf(fp, "console\n");
    } else {
        fprintf(fp, "silent\n");
    }

    if (NULL == info->seed_uri) {
        fprintf(fp, "NO-SEED-URI\n");
    } else {
        fprintf(fp, "%s\n", info->seed_uri);
    }

    fclose(fp);

    return ORTE_SUCCESS;
}

int orte_read_universe_setup_file(char *filename, orte_universe_t *info)
{
    char *input;
    FILE *fp;
    int rc;

    fp = fopen(filename, "r");
    if (NULL == fp) { /* failed on first read - wait and try again */
        fp = fopen(filename, "r");
        if (NULL == fp) { /* failed twice - give up */
            return ORTE_ERR_FILE_OPEN_FAILURE;
        }
    }

    /* fill in universe info */
    info->name = orte_getline(fp);
    if (NULL == info->name) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    }

    info->host = orte_getline(fp);
    if (NULL == info->host) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    } else if (0 == strcmp("LOCALHOST", info->host)) {
        free(info->host);
        info->host = NULL;
    }

    info->uid = orte_getline(fp);
    if (NULL == info->uid) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    } else if (0 == strcmp("NO-UID", info->uid)) {
        free(info->uid);
        info->uid = NULL;
    }

    input = orte_getline(fp);
    if (NULL == input) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    }
    if (0 == strncmp(input, "persistent", strlen("persistent"))) {
	   info->persistence = true;
    } else if (0 == strncmp(input, "non-persistent", strlen("non-persistent"))) {
	   info->persistence = false;
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        free(input);
        goto CLEANUP;
    }
    free(input);

    info->scope = orte_getline(fp);
    if (NULL == info->scope) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    } else if (0 == strcmp("NO-SCOPE", info->scope)) {
        free(info->scope);
        info->scope = strdup("exclusive");
    }
 
    input = orte_getline(fp);
    if (NULL == input) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    }
    if (0 == strncmp(input, "silent", strlen("silent"))) {
	    info->console = false;
    } else if (0 == strncmp(input, "console", strlen("console"))) {
	    info->console = true;
    } else {
	    free(input);
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
        goto CLEANUP;
    }
    free(input);

    info->seed_uri = orte_getline(fp);
    if (NULL == info->seed_uri) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
        rc = ORTE_ERR_FILE_READ_FAILURE;
        goto CLEANUP;
    } else if (0 == strcmp("NO-SEED-URI", info->seed_uri)) {
        free(info->seed_uri);
        info->seed_uri = NULL;
    }

    fclose(fp);
    return ORTE_SUCCESS;

 CLEANUP:
    fclose(fp);
    return ORTE_ERROR;
}

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[ORTE_UNIV_SETUP_FILE_MAX_LINE_LENGTH];

    ret = fgets(input, ORTE_UNIV_SETUP_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
	   input[strlen(input)-1] = '\0';  /* remove newline */
	   buff = strdup(input);
	   return buff;
    }
    
    return NULL;
}

