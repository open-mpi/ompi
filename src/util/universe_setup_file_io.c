/*
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "include/orte_constants.h"

#include "util/output.h"
#include "util/univ_info.h"
#include "runtime/runtime.h"
#include "util/universe_setup_file_io.h"

#define ORTE_UNIV_SETUP_FILE_MAX_LINE_LENGTH 1024

static char *orte_getline(FILE *fp);

int orte_write_universe_setup_file(char *filename)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
	    return ORTE_ERROR;
    }

    if (NULL == orte_universe_info.name) {
        /* fatal error - must have a name */
        fclose(fp);
        return ORTE_ERROR;
    } else {
        fprintf(fp, "%s\n", orte_universe_info.name);
    }

    if (NULL == orte_universe_info.host) {
	   fprintf(fp, "LOCALHOST\n");
    } else {
        fprintf(fp, "%s\n", orte_universe_info.host);
    }
    
    if (NULL == orte_universe_info.uid) {
        fprintf(fp, "NO-UID\n");
    } else {
        fprintf(fp, "%s\n", orte_universe_info.uid);
    }

    if (orte_universe_info.persistence) {
	   fprintf(fp, "persistent\n");
    } else {
	   fprintf(fp, "non-persistent\n");
    }

    if (NULL == orte_universe_info.scope) {
        fprintf(fp, "NO-SCOPE\n");
    } else {
        fprintf(fp, "%s\n", orte_universe_info.scope);
    }

    if (orte_universe_info.console) {
	   fprintf(fp, "console\n");
    } else {
	   fprintf(fp, "silent\n");
    }

    if (NULL == orte_universe_info.seed_uri) {
        fprintf(fp, "NO-SEED-URI\n");
    } else {
        fprintf(fp, "%s\n", orte_universe_info.seed_uri);
    }
    
    fclose(fp);

    return ORTE_SUCCESS;
}

int orte_read_universe_setup_file(char *filename)
{
    char *input;
    FILE *fp;

    fp = fopen(filename, "r");
    if (NULL == fp) { /* failed on first read - wait and try again */
	   fp = fopen(filename, "r");
	   if (NULL == fp) { /* failed twice - give up */
	       return ORTE_ERR_NOT_FOUND;
	   }
    }

    orte_universe_info.name = orte_getline(fp);
    if (NULL == orte_universe_info.name) {
	   goto CLEANUP;
    }

    orte_universe_info.host = orte_getline(fp);
    if (NULL == orte_universe_info.host) {
       goto CLEANUP;
    } else if (0 == strcmp("LOCALHOST", orte_universe_info.host)) {
        free(orte_universe_info.host);
        orte_universe_info.host = NULL;
    }

    orte_universe_info.uid = orte_getline(fp);
    if (NULL == orte_universe_info.uid) {
       goto CLEANUP;
    } else if (0 == strcmp("NO-UID", orte_universe_info.uid)) {
        free(orte_universe_info.uid);
        orte_universe_info.uid = NULL;
    }

    input = orte_getline(fp);
    if (NULL == input) {
       goto CLEANUP;
    }
    if (0 == strncmp(input, "persistent", strlen("persistent"))) {
	   orte_universe_info.persistence = true;
    } else if (0 == strncmp(input, "non-persistent", strlen("non-persistent"))) {
	   orte_universe_info.persistence = false;
    } else {
	   free(input);
       goto CLEANUP;
    }
    free(input);

    orte_universe_info.scope = orte_getline(fp);
    if (NULL == orte_universe_info.scope) {
       goto CLEANUP;
    } else if (0 == strcmp("NO-SCOPE", orte_universe_info.scope)) {
        free(orte_universe_info.scope);
        orte_universe_info.scope = strdup("exclusive");
    }
 
    input = orte_getline(fp);
    if (NULL == input) {
       goto CLEANUP;
    }
    if (0 == strncmp(input, "silent", strlen("silent"))) {
	    orte_universe_info.console = false;
    } else if (0 == strncmp(input, "console", strlen("console"))) {
	    orte_universe_info.console = true;
    } else {
	    free(input);
	    goto CLEANUP;
    }
    free(input);

    orte_universe_info.seed_uri = orte_getline(fp);
    if (NULL == orte_universe_info.seed_uri) {
	    goto CLEANUP;
    } else if (0 == strcmp("NO-SEED-URI", orte_universe_info.seed_uri)) {
        free(orte_universe_info.seed_uri);
        orte_universe_info.seed_uri = NULL;
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

