/*
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * $Id: ompi_universe_setup_file I/O functions $
 * 
 */
#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/constants.h"

#include "util/output.h"
#include "runtime/runtime.h"
#include "util/universe_setup_file_io.h"

#define OMPI_UNIV_SETUP_FILE_MAX_LINE_LENGTH 1024

static char *ompi_getline(FILE *fp);

int ompi_write_universe_setup_file(char *filename)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
	return OMPI_ERROR;
    }

    if (NULL == ompi_universe_info.name) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.name);

    if (NULL == ompi_universe_info.host) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.host);

    if (NULL == ompi_universe_info.uid) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.uid);

    fprintf(fp, "%d\n", ompi_universe_info.pid);

    if (ompi_universe_info.persistence) {
	fprintf(fp, "persistent\n");
    } else {
	fprintf(fp, "non-persistent\n");
    }

    if (NULL == ompi_universe_info.scope) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.scope);

    if (ompi_universe_info.console) {
	fprintf(fp, "console\n");
    } else {
	fprintf(fp, "silent\n");
    }

    if (NULL == ompi_universe_info.seed_contact_info) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.seed_contact_info);
    fclose(fp);

    return OMPI_SUCCESS;

 CLEANUP:
    fclose(fp);
    return OMPI_ERROR;
}

int ompi_read_universe_setup_file(char *filename)
{
    char *input;
    FILE *fp;

    fp = fopen(filename, "r");
    if (NULL == fp) { /* failed on first read - wait and try again */
	fp = fopen(filename, "r");
	if (NULL == fp) { /* failed twice - give up */
	    return OMPI_ERR_NOT_FOUND;
	}
    }

    ompi_universe_info.name = ompi_getline(fp);
    if (NULL == ompi_universe_info.name) {
	goto CLEANUP;
    }

    ompi_universe_info.host = ompi_getline(fp);
    if (NULL == ompi_universe_info.host) {
	goto CLEANUP;
    }

    ompi_universe_info.uid = ompi_getline(fp);
    if (NULL == ompi_universe_info.uid) {
	goto CLEANUP;
    }

    input = ompi_getline(fp);
    if (NULL == input) {
	goto CLEANUP;
    }
    ompi_universe_info.pid = (pid_t)atoi(input);

    input = ompi_getline(fp);
    if (NULL == input) {
	goto CLEANUP;
    }
    if (0 == strncmp(input, "persistent", strlen("persistent"))) {
	ompi_universe_info.persistence = true;
    } else if (0 == strncmp(input, "non-persistent", strlen("non-persistent"))) {
	ompi_universe_info.persistence = false;
    } else {
	free(input);
	goto CLEANUP;
    }
    free(input);

    ompi_universe_info.scope = ompi_getline(fp);
    if (NULL == ompi_universe_info.scope) {
	goto CLEANUP;
    }
 
    input = ompi_getline(fp);
    if (NULL == input) {
	goto CLEANUP;
    }
    if (0 == strncmp(input, "silent", strlen("silent"))) {
	ompi_universe_info.console = false;
    } else if (0 == strncmp(input, "console", strlen("console"))) {
	ompi_universe_info.console = true;
    } else {
	free(input);
	goto CLEANUP;
    }
    free(input);

    ompi_universe_info.seed_contact_info = ompi_getline(fp);
    if (NULL == ompi_universe_info.seed_contact_info) {
	goto CLEANUP;
    }

    fclose(fp);
    return OMPI_SUCCESS;

 CLEANUP:
    fclose(fp);
    return OMPI_ERROR;
}

static char *ompi_getline(FILE *fp)
{
    char *ret, *buff;
    char input[OMPI_UNIV_SETUP_FILE_MAX_LINE_LENGTH];

    ret = fgets(input, OMPI_UNIV_SETUP_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
	input[strlen(input)-1] = '\0';  /* remove newline */
	buff = strdup(input);
	return buff;
    }
    return NULL;
}

