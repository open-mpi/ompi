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

char *ompi_getline_buffer(FILE *fp);

int ompi_write_universe_setup_file(char *filename, ompi_universe_t *universe)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
	ompi_output(0, "cannot open file to save contact info");
	return OMPI_ERROR;
    }
    fprintf(fp, "%ld name: %s\n", strlen(universe->name), universe->name);
    fprintf(fp, "%ld host: %s\n", strlen(universe->host), universe->host);
    fprintf(fp, "%ld user: %s\n", strlen(universe->uid), universe->uid);
    if (universe->persistence) {
	fprintf(fp, "state: persistent\n");
    } else {
	fprintf(fp, "state: non-persistent\n");
    }
    if (universe->silent_mode) {
	fprintf(fp, "mode: silent\n");
    } else {
	fprintf(fp, "mode: console\n");
    }
    if (universe->web_server) {
	fprintf(fp, "%ld socket: %s\n", strlen(universe->socket_contact_info),
		universe->socket_contact_info);
    } else {
	fprintf(fp, "0\n");
    }
    fprintf(fp, "%ld oob: %s\n", strlen(universe->oob_contact_info),
	    universe->oob_contact_info);
    fclose(fp);
    return OMPI_SUCCESS;
}

int ompi_read_universe_setup_file(char *filename, ompi_universe_t *universe)
{
    char persist[20], mode[10];
    FILE *fp;

    fp = fopen(filename, "r");
    if (NULL == fp) { /* failed on first read - wait and try again */
	sleep(1);
	fp = fopen(filename, "r");
	if (NULL == fp) { /* failed twice - give up */
	    return OMPI_ERR_NOT_FOUND;
	}
    }

    universe->name = ompi_getline_buffer(fp);
    fscanf(fp, "name: %s\n", universe->name);

    universe->host = ompi_getline_buffer(fp);
    fscanf(fp, "host: %s\n", universe->host);

    universe->uid = ompi_getline_buffer(fp);
    fscanf(fp, "user: %s\n", universe->uid);

    fscanf(fp, "state: %s", persist);
    if (0 == strncmp(persist, "persistent", strlen("persistent"))) {
	universe->persistence = true;
    } else if (0 == strncmp(persist, "non-persistent", strlen("non-persistent"))) {
	universe->persistence = false;
    } else {
	return OMPI_ERROR;
    }

    fscanf(fp, "mode: %s", mode);
    if (0 == strncmp(mode, "silent", strlen("silent"))) {
	universe->silent_mode = true;
    } else if (0 == strncmp(mode, "console", strlen("console"))) {
	universe->silent_mode = false;
    } else {
	return OMPI_ERROR;
    }

    universe->socket_contact_info = ompi_getline_buffer(fp);
    if (NULL != universe->socket_contact_info) {
	fscanf(fp, "socket: %s", universe->socket_contact_info);
	universe->web_server = true;
    } else {
	universe->web_server = false;
    }

    universe->oob_contact_info = ompi_getline_buffer(fp);
    fscanf(fp, "oob: %s", universe->oob_contact_info);

    fclose(fp);

    return OMPI_SUCCESS;
}

char *ompi_getline_buffer(FILE *fp)
{
    int len, i, in_val;
    char in_buf[100], in_char, *buffer;

    i = 0;
    while ((EOF != (in_val = fgetc(fp))) && (' ' != (in_char = (char)in_val))) {
	in_buf[i] = in_char;
	i++;
    }
    in_buf[i] = '\0';
    len = atoi(in_buf);
    if (len > 0) {
	buffer = (char*)malloc((len+1)*sizeof(char));
    } else {
	buffer = NULL;
    }

    return buffer;
}
