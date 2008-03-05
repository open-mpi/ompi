/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "plpa_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "plpa.h"


int main(int argc, char *argv[]) 
{
    int i;
    int ret = 0;
    int need_help = 0;
    int show_topo = 0;
    int have_topo, num_sockets, max_socket_num, num_cores, max_core_num;
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
    if (0 != PLPA_NAME(get_socket_info)(&num_sockets, &max_socket_num)) {
        num_sockets = max_socket_num = -1;
    }
    printf("Number of processor sockets: ");
    if (have_topo && num_sockets >= 0) {
        printf("%d\n", num_sockets);
    } else {
        printf("unknown\n");
    }

    /* If asked, print the map */

    if (show_topo) {
        if (have_topo) {
            /* Remember that max_socket and max_core values are
               0-indexed */
            for (i = 0; i <= max_socket_num; ++i) {
                ret = PLPA_NAME(get_core_info)(i, &num_cores, &max_core_num);
                if (0 == ret) {
                    printf("Socket %d: %d core%s (max core ID: %d)\n",
                           i, num_cores, (1 == num_cores) ? "" : "s",
                           max_core_num);
                }
            }
        } else {
            printf("Kernel topology not supported -- cannot show topology information\n");
            exit(1);
        }
    }

    return 0;
}
