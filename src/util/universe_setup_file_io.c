/*
 * 
 * $HEADER$
 *
 * $Id: ompi_universe_setup_file I/O functions $
 * 
 */
#include "ompi_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>

#include "include/constants.h"

#include "util/output.h"
#include "runtime/runtime.h"
#include "util/universe_setup_file_io.h"

#define OMPI_UNIV_SETUP_FILE_MAX_LINE_LENGTH 1024

char *ompi_getline(FILE *fp);

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

    if (ompi_universe_info.persistence) {
	fprintf(fp, "persistent\n");
    } else {
	fprintf(fp, "non-persistent\n");
    }

    if (NULL == ompi_universe_info.scope) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.scope);

    if (ompi_universe_info.silent_mode) {
	fprintf(fp, "silent\n");
    } else {
	fprintf(fp, "console\n");
    }

    if (ompi_universe_info.web_server && NULL != ompi_universe_info.socket_contact_info) {
	fprintf(fp, "%s\n", ompi_universe_info.socket_contact_info);
    } else {
	fprintf(fp, "none\n");
    }

    if (NULL == ompi_universe_info.oob_contact_info) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", ompi_universe_info.oob_contact_info);
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
	sleep(1);
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
	ompi_universe_info.silent_mode = true;
    } else if (0 == strncmp(input, "console", strlen("console"))) {
	ompi_universe_info.silent_mode = false;
    } else {
	free(input);
	goto CLEANUP;
    }
    free(input);

    ompi_universe_info.socket_contact_info = ompi_getline(fp);
    if (NULL == ompi_universe_info.socket_contact_info) {
	goto CLEANUP;
    }
    if (0 == strncmp(ompi_universe_info.socket_contact_info, "none", strlen("none"))) {
	ompi_universe_info.web_server = false;
    } else {
	ompi_universe_info.web_server = true;
    }

    ompi_universe_info.oob_contact_info = ompi_getline(fp);
    if (NULL == ompi_universe_info.oob_contact_info) {
	goto CLEANUP;
    }

    fclose(fp);
    return OMPI_SUCCESS;

 CLEANUP:
    fclose(fp);
    return OMPI_ERROR;
}

char *ompi_getline(FILE *fp)
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

