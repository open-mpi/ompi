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
#include "tools/openmpi/openmpi.h"
#include "util/universe_setup_file_io.h"

#define OMPI_UNIV_SETUP_FILE_MAX_LINE_LENGTH 1024

char *ompi_getline(FILE *fp);

int ompi_write_universe_setup_file(char *filename, ompi_universe_t *universe)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
	return OMPI_ERROR;
    }

    if (NULL == universe->name) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", universe->name);

    if (NULL == universe->host) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", universe->host);

    if (NULL == universe->uid) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", universe->uid);

    if (universe->persistence) {
	fprintf(fp, "persistent\n");
    } else {
	fprintf(fp, "non-persistent\n");
    }

    if (NULL == universe->scope) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", universe->scope);

    if (universe->silent_mode) {
	fprintf(fp, "silent\n");
    } else {
	fprintf(fp, "console\n");
    }

    if (universe->web_server && NULL != universe->socket_contact_info) {
	fprintf(fp, "%s\n", universe->socket_contact_info);
    } else {
	fprintf(fp, "none\n");
    }

    if (NULL == universe->oob_contact_info) {
	goto CLEANUP;
    }
    fprintf(fp, "%s\n", universe->oob_contact_info);
    fclose(fp);

    return OMPI_SUCCESS;

 CLEANUP:
    fclose(fp);
    return OMPI_ERROR;
}

int ompi_read_universe_setup_file(char *filename, ompi_universe_t *universe)
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

    universe->name = ompi_getline(fp);
    if (NULL == universe->name) {
	goto CLEANUP;
    }

    universe->host = ompi_getline(fp);
    if (NULL == universe->host) {
	goto CLEANUP;
    }

    universe->uid = ompi_getline(fp);
    if (NULL == universe->uid) {
	goto CLEANUP;
    }

    input = ompi_getline(fp);
    if (NULL == input) {
	goto CLEANUP;
    }
    if (0 == strncmp(input, "persistent", strlen("persistent"))) {
	universe->persistence = true;
    } else if (0 == strncmp(input, "non-persistent", strlen("non-persistent"))) {
	universe->persistence = false;
    } else {
	free(input);
	goto CLEANUP;
    }
    free(input);

    universe->scope = ompi_getline(fp);
    if (NULL == universe->scope) {
	goto CLEANUP;
    }
 
    input = ompi_getline(fp);
    if (NULL == input) {
	goto CLEANUP;
    }
    if (0 == strncmp(input, "silent", strlen("silent"))) {
	universe->silent_mode = true;
    } else if (0 == strncmp(input, "console", strlen("console"))) {
	universe->silent_mode = false;
    } else {
	free(input);
	goto CLEANUP;
    }
    free(input);

    universe->socket_contact_info = ompi_getline(fp);
    if (NULL == universe->socket_contact_info) {
	goto CLEANUP;
    }
    if (0 == strncmp(universe->socket_contact_info, "none", strlen("none"))) {
	universe->web_server = false;
    } else {
	universe->web_server = true;
    }

    universe->oob_contact_info = ompi_getline(fp);
    if (NULL == universe->oob_contact_info) {
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

