/*
 * $HEADER$
 */

#include <sys/param.h>
#include <stdlib.h>
#include <stdbool.h>

/* Define the structures underlying the Open MPI universe system */

struct ompi_universe_t {
    char *name;
    pid_t pid;
    char *session_file;
    bool persistence;
    bool web_server;
    bool console_connected;
};
typedef struct ompi_universe_t ompi_universe_t;

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

#define OMPI_RIDICULOUS_NAMELEN 1024

