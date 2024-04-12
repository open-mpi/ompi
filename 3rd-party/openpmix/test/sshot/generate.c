/*
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <getopt.h>
#include <jansson.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define DEFAULT_NUM_SWITCHES  8
#define DEFAULT_NUM_FABPORTS  10
#define DEFAULT_NUM_EDGEPORTS 10
#define DEFAULT_NUM_NODES     8
#define DEFAULT_NUM_NICS      8
#define DEFAULT_NUM_GROUPS    2

static int iseven(int num)
{
    return !(num & 1);
}

static int help = 0;
typedef struct {
    int nswitches;
    int *members;
} grptrk_t;

int main(int argc, char **argv)
{
    json_t *root, *switches, *fabricports, *edgeports;
    json_t *sw, *port, *cport, *links;
    json_t *nodes, *node, *nics;
    char *s;
    int idnum = 10;
    int jnum = 0;
    int pnum = 0;
    int n, nn;
    int nsw, nswitches = DEFAULT_NUM_SWITCHES;
    int numfabports = DEFAULT_NUM_FABPORTS;
    int numedgeports = DEFAULT_NUM_EDGEPORTS;
    int nnics = DEFAULT_NUM_NICS;
    int nnodes = DEFAULT_NUM_NODES;
    int ngrps = DEFAULT_NUM_GROUPS;
    char *tmp;
    int opt;
    grptrk_t map[100];
    char *outfile = NULL;
    static struct option myoptions[] = {{"file", required_argument, NULL, 'f'},
                                        {"nswitches", required_argument, NULL, 's'},
                                        {"nfabports", required_argument, NULL, 'p'},
                                        {"nedgeports", required_argument, NULL, 'e'},
                                        {"nnodes", required_argument, NULL, 'c'},
                                        {"nnics", required_argument, NULL, 'n'},
                                        {"ngrps", required_argument, NULL, 'g'},
                                        {"help", no_argument, &help, 1}};
    int option_index;

    /* check for options */
    while ((opt = getopt_long(argc, argv, "f:p:e:c:n", myoptions, &option_index)) != -1) {
        switch (opt) {
        case 'f':
            outfile = optarg;
            break;
        case 's':
            nswitches = strtol(optarg, NULL, 10);
            break;
        case 'p':
            numfabports = strtol(optarg, NULL, 10);
            break;
        case 'e':
            numedgeports = strtol(optarg, NULL, 10);
            break;
        case 'c':
            nnodes = strtol(optarg, NULL, 10);
            break;
        case 'n':
            nnics = strtol(optarg, NULL, 10);
            break;
        case 'g':
            ngrps = strtol(optarg, NULL, 10);
            break;
        default:
            fprintf(stderr,
                    "Usage: %s\n    Options:\n"
                    "        [-f] [output file - will add .json extension]\n"
                    "        [-s] [number of switches]\n"
                    "        [-g] [number of groups to organize the switches into (<100)\n"
                    "        [-p] [number of fabricports per switch]\n"
                    "        [-e] [number of edge ports per switch]\n"
                    "        [-c] [number of nodes]\n"
                    "        [-n] [number of NICs per node]\n",
                    argv[0]);
            exit(1);
        }
    }
    if (help) {
        fprintf(stdout,
                "Usage: %s\n    Options:\n"
                "        [-f] [output file - will add .json extension]\n"
                "        [-s] [number of switches]\n"
                "        [-p] [number of fabricports per switch]\n"
                "        [-e] [number of edge ports per switch]\n"
                "        [-c] [number of nodes]\n"
                "        [-n] [number of NICs per node]\n",
                argv[0]);
        exit(0);
    }

    /* create a JSON object */
    root = json_object();

    /* we will be defining an array of switches */
    switches = json_array();
    json_object_set_new(root, "switches", switches);

    for (n = 0; n < ngrps; n++) {
        map[n].nswitches = 0;
        map[n].members = (int *) malloc(nswitches * sizeof(int));
    }

    for (nsw = 0; nsw < nswitches; nsw++) {
        /* each switch contains an object that starts with its identifiers */
        sw = json_object();
        (void) asprintf(&tmp, "0x30000c0r21b%d", nsw);
        json_object_set_new(sw, "IP", json_string(tmp));
        free(tmp);
        nn = nsw % ngrps;
        json_object_set_new(sw, "grpID", json_integer(nn));
        map[nn].members[map[nn].nswitches] = nsw;
        map[nn].nswitches++;
        json_object_set_new(sw, "swcNum", json_integer(nsw));
        /* each switch contains an array of fabric ports */
        fabricports = json_array();
        for (n = 0; n < numfabports; n++) {
            port = json_object();
            /* each fabric port has an ID */
            (void) asprintf(&tmp, "0x30000c0r21a01%d", idnum);
            json_object_set_new(port, "id", json_string(tmp));
            free(tmp);
            ++idnum;
            /* each fabric port has a "meta" object that contains the connection port */
            cport = json_object();
            (void) asprintf(&tmp, "0x30000c0r21j%dp%d", jnum, pnum);
            json_object_set_new(cport, "conn_port", json_string(tmp));
            json_object_set_new(port, "meta", cport);
            free(tmp);
            if (iseven(n)) {
                pnum = 1;
            } else {
                pnum = 0;
                ++jnum;
            }
            /* add the port to the array of fabricports */
            json_array_append(fabricports, port);
        }
        /* add the fabricports to the switch */
        json_object_set_new(sw, "fabricports", fabricports);

        /* each switch also contains an array of edge ports */
        edgeports = json_array();
        for (n = 0; n < numedgeports; n++) {
            port = json_object();
            /* each edge port has an ID */
            (void) asprintf(&tmp, "0x30000c0r21a01%d", idnum);
            json_object_set_new(port, "id", json_string(tmp));
            free(tmp);
            ++idnum;
            /* each edge port has a "meta" object that contains the connection port */
            cport = json_object();
            (void) asprintf(&tmp, "0x30000c0r21j%dp%d", jnum, pnum);
            json_object_set_new(cport, "conn_port", json_string(tmp));
            json_object_set_new(port, "meta", cport);
            free(tmp);
            if (iseven(n)) {
                pnum = 1;
            } else {
                pnum = 0;
                ++jnum;
            }
            /* add the port to the array of edgeports */
            json_array_append(edgeports, port);
        }
        /* add the edgeports to the switch */
        json_object_set_new(sw, "edgeports", edgeports);

        /* add the switch to the array */
        json_array_append(switches, sw);
    }

    /* add the nmber of groups */
    json_object_set_new(root, "numGroups", json_integer(ngrps));
    /* set the max number of switches in a group */
    nsw = (nswitches / ngrps) + (nswitches % ngrps);
    json_object_set_new(root, "maxNumLocalSwitches", json_integer(nsw));

    /* define an array of links */
    links = json_array();
    json_object_set_new(root, "links", links);
    /* put some arbitrary endpoint info in it */
    for (n = 0; n < numfabports; n++) {
        port = json_object();
        (void) asprintf(&tmp, "0x30000c0r21a01%d", idnum);
        json_object_set_new(port, "endpoint1", json_string(tmp));
        free(tmp);
        ++idnum;
        (void) asprintf(&tmp, "0x30000c0r21a01%d", idnum);
        json_object_set_new(port, "endpoint2", json_string(tmp));
        free(tmp);
        ++idnum;
        /* add the port to the array of links */
        json_array_append(links, port);
    }

    /* define an array of nodes */
    nodes = json_array();
    json_object_set_new(root, "nodes", nodes);
    nsw = 0;

    for (nn = 0; nn < nnodes; nn++) {
        /* each node contains an object that starts with its name */
        node = json_object();
        (void) asprintf(&tmp, "node%03d", nn);
        json_object_set_new(node, "name", json_string(tmp));
        free(tmp);
        /* each node contains a set of NICs */
        nics = json_array();
        for (n = 0; n < nnics; n++) {
            port = json_object();
            /* each NIC has an ID */
            (void) asprintf(&tmp, "0x30000c0r21a01%d", idnum);
            json_object_set_new(port, "id", json_string(tmp));
            free(tmp);
            ++idnum;
            /* define a group for this node */
            opt = nn % ngrps;
            /* and a connection - each NIC needs to point to a
             * switch in the same group */
            nsw = n % map[opt].nswitches;
            json_object_set_new(port, "switch", json_integer(map[opt].members[nsw]));
            /* add the port to the NIC array */
            json_array_append(nics, port);
        }
        /* add the NICs to the node */
        json_object_set_new(node, "nics", nics);
        /* add the node to the array */
        json_array_append(nodes, node);
    }

    if (NULL == outfile) {
        s = json_dumps(root, JSON_INDENT(4));
        puts(s);
    } else {
        (void) asprintf(&tmp, "%s.json", outfile);
        json_dump_file(root, tmp, JSON_INDENT(4));
        free(tmp);
    }
    json_decref(root);

    return 0;
}
