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

int ompi_write_universe_setup_file(char *filename)
{
#if 1
    ompi_output(0, "WARNING: Called non-functional ompi_write_universe_setup_file\n");
    return OMPI_ERROR;
#else
    FILE *fp;

    fp = fopen(filename, "w");
    if (NULL == fp) {
	ompi_output(0, "cannot open file to save contact info");
	return OMPI_ERROR;
    }
    fprintf(fp, "%ld name: %s\n", strlen(ompi_universe.name), ompi_universe.name);
    fprintf(fp, "%ld host: %s\n", strlen(ompi_universe.host), ompi_universe.host);
    fprintf(fp, "%ld user: %s\n", strlen(ompi_universe.uid), ompi_universe.uid);
    if (ompi_universe.persistence) {
	fprintf(fp, "state: persistent\n");
    } else {
	fprintf(fp, "state: non-persistent\n");
    }
    if (ompi_universe.silent_mode) {
	fprintf(fp, "mode: silent\n");
    } else {
	fprintf(fp, "mode: console\n");
    }
    if (ompi_universe.web_server) {
	fprintf(fp, "%ld socket: %s\n", strlen(ompi_universe.socket_contact_info),
		ompi_universe.socket_contact_info);
    } else {
	fprintf(fp, "0\n");
    }
    fprintf(fp, "%ld oob: %s\n", strlen(ompi_universe.oob_contact_info),
	    ompi_universe.oob_contact_info);
    fclose(fp);
    return OMPI_SUCCESS;
#endif
}

int ompi_read_universe_setup_file(char *filename)
{
#if 1
    ompi_output(0, "WARNING: Called non-functional ompi_read_universe_setup_file\n");
    return OMPI_ERROR;
#else
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

    ompi_universe.name = ompi_getline_buffer(fp);
    fscanf(fp, "name: %s\n", ompi_universe.name);

    ompi_universe.host = ompi_getline_buffer(fp);
    fscanf(fp, "host: %s\n", ompi_universe.host);

    ompi_universe.uid = ompi_getline_buffer(fp);
    fscanf(fp, "user: %s\n", ompi_universe.uid);

    fscanf(fp, "state: %s", persist);
    if (0 == strncmp(persist, "persistent", strlen("persistent"))) {
	ompi_universe.persistence = true;
    } else if (0 == strncmp(persist, "non-persistent", strlen("non-persistent"))) {
	ompi_universe.persistence = false;
    } else {
	return OMPI_ERROR;
    }

    fscanf(fp, "mode: %s", mode);
    if (0 == strncmp(mode, "silent", strlen("silent"))) {
	ompi_universe.silent_mode = true;
    } else if (0 == strncmp(mode, "console", strlen("console"))) {
	ompi_universe.silent_mode = false;
    } else {
	return OMPI_ERROR;
    }

    ompi_universe.socket_contact_info = ompi_getline_buffer(fp);
    if (NULL != ompi_universe.socket_contact_info) {
	fscanf(fp, "socket: %s", ompi_universe.socket_contact_info);
	ompi_universe.web_server = true;
    } else {
	ompi_universe.web_server = false;
    }

    ompi_universe.oob_contact_info = ompi_getline_buffer(fp);
    fscanf(fp, "oob: %s", ompi_universe.oob_contact_info);

    fclose(fp);

    return OMPI_SUCCESS;
#endif
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
