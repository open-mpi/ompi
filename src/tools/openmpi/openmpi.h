/*
 * $HEADER$
 */

#include <sys/param.h>
#include <stdlib.h>

#ifndef OMPI_UNIVERSE_H
#define OMPI_UNIVERSE_H

/* Define the structures underlying the Open MPI universe system */

struct ompi_universe_t {
    char *name;
    char *host;
    char *uid;
    bool persistence;
    char *scope;
    bool silent_mode;
    bool script_mode;
    bool web_server;
    char *socket_contact_info;
    char *oob_contact_info;
    bool console_connected;
};
typedef struct ompi_universe_t ompi_universe_t;

extern ompi_universe_t ompi_universe;

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

#define OMPI_RIDICULOUS_NAMELEN 1024

#endif
