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
    int have_topo, num_sockets, max_socket_num, num_cores, max_core_id;
    int num_processors_online, max_processor_id_online;
    int num_processors_offline, max_processor_id_offline;
    int num_processors_total, max_processor_id_total;
    int processor_id;
    int socket_id, exists, online, num_offline;
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
            /* Go through all the processors and count how many are
               offline; we have no topology information for offline
               processors */
            if (0 != PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ALL),
                                                   &num_processors_total,
                                                   &max_processor_id_total) ||
                0 != PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                                   &num_processors_online,
                                                   &max_processor_id_online) ||
                0 != PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_OFFLINE),
                                                   &num_processors_offline,
                                                   &max_processor_id_offline)) {
                fprintf(stderr, "plpa_get_processor_info failed\n");
                exit(1);
            }
            /* This is a little overkill; this information should
               never mismatch.  But what the heck. */
            if (num_processors_online + num_processors_offline !=
                num_processors_total) {
                fprintf(stderr, "Number of online and offline processors do not seem to add up (online: %d, offline: %d, total: %d)\n",
                        num_processors_online,
                        num_processors_offline,
                        num_processors_total);
                exit(1);
            }

            printf("Number of processors online: %d\n", num_processors_online);
            printf("Number of processors offline: %d (no topology information available)\n", 
                   num_processors_offline);

            /* Another "over the top" check -- these should never
               disagree.  But what the heck; it's a good test of
               PLPA. */
            for (num_offline = i = 0; i < num_processors_total; ++i) {
                if (0 != PLPA_NAME(get_processor_id)(i,
                                                     PLPA_NAME_CAPS(COUNT_ALL),
                                                      &processor_id)) {
                    fprintf(stderr, "pla_get_processor_id failed\n");
                    break;
                }
                if (0 != PLPA_NAME(get_processor_flags)(processor_id,
                                                        &exists, 
                                                        &online)) {
                    fprintf(stderr, "plpa_get_processor_flags failed\n");
                    break;
                }
                if (exists && !online) {
                    ++num_offline;
                }
            }
            if (num_offline != num_processors_offline) {
                fprintf(stderr, "Number of online and offline processors do not seem to add up (1)\n");
                exit(1);
            }

            /* Go through all the sockets */
            for (i = 0; i < num_sockets; ++i) {
                /* Turn the socket number into a Linux socket ID */
                if (0 != PLPA_NAME(get_socket_id)(i, &socket_id)) {
                    fprintf(stderr, "plpa_get_socket_id failed\n");
                    break;
                }
                /* Find out about the cores on that socket */
                if (0 != PLPA_NAME(get_core_info)(socket_id,
                                                  &num_cores, &max_core_id)) {
                    fprintf(stderr, "plpa_get_core_info failed\n");
                    break;
                }

                printf("Socket %d (ID %d): %d core%s (max core ID: %d)\n",
                       i, socket_id, num_cores, (1 == num_cores) ? "" : "s",
                       max_core_id);
            }
        } else {
            printf("Kernel topology not supported -- cannot show topology information\n");
            exit(1);
        }
    }

    return 0;
}
