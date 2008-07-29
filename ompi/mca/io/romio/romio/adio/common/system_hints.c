/* -*- Mode: C; c-basic-offset:4 ; -*- 
 * vim: ts=8 sts=4 sw=4 noexpandtab
 *
 *   Copyright (C) 2007 UChicago/Argonne LLC. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include <adio.h>

#include <stdio.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_IO_H
#include <io.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 65535
#endif

/*#define SYSHINT_DEBUG 1  */

#define ROMIO_HINT_DEFAULT_CFG "/etc/romio-hints"
#define ROMIO_HINT_ENV_VAR "ROMIO_HINTS"

/* if user set the environment variable, use its value to find the
 * file-of-hints.  Otherwise, we'll look for the default config file.  i.e. let
 * the user override systemwide hint processing */

static int find_file(void)
{
    int fd=-1;
    char * hintfile;
    
    hintfile = getenv(ROMIO_HINT_ENV_VAR);
    if(hintfile)
    fd = open(hintfile, O_RDONLY);
    if (fd < 0 )
	fd = open(ROMIO_HINT_DEFAULT_CFG, O_RDONLY);

    return fd;
}

/* parse the file-of-hints.  Format is zero or more lines of "<key> <value>\n".
 * A # in collumn zero is a comment and the line will be ignored.  Do our best
 * to ignore badly formed lines too. 
 *
 * The caller provides an 'info' object.  Each key-value pair found by the
 * parser will get added to the info object.  any keys already set will be left
 * alone on the assumption that the caller knows best. 
 *
 * because MPI-IO hints are optional, we can get away with limited error
 * reporting.  */
static int file_to_info(int fd, MPI_Info info)
{
    char *buffer, *token, *key, *val, *garbage;
    char *pos1, *pos2;
    int flag, ret;
    char dummy;
    struct stat statbuf;

    /* assumption: config files will be small (less than 1MB) */
    fstat(fd, &statbuf);
    /* add 1 to size to make room for NULL termination */
    buffer = (char *)calloc(statbuf.st_size + 1, sizeof (char));
    if (buffer == NULL) return -1;

    ret = read(fd, buffer, statbuf.st_size);
    if (ret < 0) return -1;
    token = strtok_r(buffer, "\n", &pos1);
    do {
	if ( (key = strtok_r(token, " \t", &pos2)) == NULL) 
	    /* malformed line: found no items */
	    continue;
	if (token[0] == '#') 
	    /* ignore '#'-delimited comments */
	    continue;
	if ( (val = strtok_r(NULL, " \t", &pos2))  == NULL) 
	    /* malformed line: found key without value */
	    continue;
	if ( (garbage = strtok_r(NULL, " \t", &pos2)) != NULL) 
	    /* malformed line: more than two items */
	    continue;
	    
#ifdef SYSHINT_DEBUG
	printf("found: key=%s val=%s\n", key, val);
#endif
	/* don't actually care what the value is. only want to know if key
	 * exists: we leave it alone if so*/
	MPI_Info_get(info, key, 0, &dummy, &flag);
	if (flag == 1) continue;
	MPI_Info_set(info, key, val);
    } while ((token = strtok_r(NULL, "\n", &pos1)) != NULL);
    free(buffer);
    return 0;
}

void ADIOI_process_system_hints(MPI_Info info)
{
    int hintfd;

    hintfd = find_file();
    if (hintfd < 0) {
#ifdef SYSHINT_DEBUG
	perror("ADIOI_process_system_hints");
#endif
	return;
    }
    file_to_info(hintfd, info);
    close(hintfd);
}

/* OMPI: Commented out so that we don't get compiler warnings */
#if 0
/* debug function: a routine I want in the library to make my life easier when
 * using a source debugger. please ignore any "defined but not used" warnings
 */
static void dump_keys(MPI_Info info) {
    int i, nkeys, flag;
    char key[MPI_MAX_INFO_KEY];
    char value[MPI_MAX_INFO_VAL];

    MPI_Info_get_nkeys(info, &nkeys);

    for (i=0; i<nkeys; i++) {
	MPI_Info_get_nthkey(info, i, key);
	MPI_Info_get(info, key, MPI_MAX_INFO_VAL-1, value, &flag);
	printf("key = %s, value = %s\n", key, value);
    }
    return;
}
#endif
