/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <plpa.h>


int main(int argc, char *argv[]) 
{
    int i;
    int ret = 0;
    int need_help = 0;
    int show_topo = 0;
    int have_topo, max_socket, max_core;
    PLPA_NAME(api_type_t) api_probe;

    for (i = 1; i < argc; ++i) {
        if (0 == strcmp("--version", argv[i])) {
            printf("PLPA version %s\n", PACKAGE_VERSION);
            exit(0);
        } else if (0 == strcmp("--help", argv[i])) {
            need_help = 1;
            ret = 0;
            break;
        } else if (0 == strcmp("--topo", argv[i])) {
            show_topo = 1;
        } else {
            printf("%s: unrecognized option: %s\n",
                   argv[0], argv[i]);
            need_help = 1;
            ret = 1;
        }
    }

    if (need_help) {
        printf("usage: %s [--version | --topo] [--help]\n", argv[0]);
        return ret;
    }

    /* Is affinity supported at all? */

    if (0 != PLPA_NAME(api_probe)(&api_probe)) {
        api_probe = PLPA_NAME_CAPS(PROBE_NOT_SUPPORTED);
    }
    printf("Kernel affinity support: ");
    switch (api_probe) {
    case PLPA_NAME_CAPS(PROBE_OK):
        printf("yes\n");
        break;
    case PLPA_NAME_CAPS(PROBE_NOT_SUPPORTED):
        printf("no\n");
        break;
    default:
        printf("unknonwn (no)\n");
        break;
    }

    /* What about topology? */

    if (0 != PLPA_NAME(have_topology_information)(&have_topo)) {
        have_topo = 0;
    }
    printf("Kernel topology support: %s\n", have_topo ? "yes" : "no");
    if (0 != PLPA_NAME(max_socket)(&max_socket)) {
        max_socket = -1;
    }
    printf("Number of processor sockets: %d\n", have_topo ? max_socket : -1);

    /* If asked, print the map */

    if (show_topo) {
        if (have_topo) {
            /* Remember that max_socket and max_core values are
               0-indexed */
            for (i = 0; i <= max_socket; ++i) {
                ret = PLPA_NAME(max_core)(i, &max_core);
                if (0 == ret) {
                    printf("Socket %d: %d core%s\n", i, max_core + 1,
                           (0 == max_core) ? "" : "s");
                } else {
                    printf("Socket %d: unknown cores\n", i);
                }
            }
        } else {
            printf("Kernel topology not supported -- cannot show topology information\n");
            exit(1);
        }
    }

    return 0;
}
