/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "src/util/argv.h"
#include "src/util/pmix_environ.h"
#include "src/util/output.h"
#include "src/server/pmix_server_ops.h"

#include "server_callbacks.h"
#include "utils.h"

#define TEST_NODES "odin001,odin002,odin003,odin010,odin011,odin075"
#define TEST_PROCS "1,2,3,4;5-8;9,11-12;17-20;21-24;100"

int main(int argc, char **argv)
{
    char *regex;
    char **nodes, **procs;
    pmix_status_t rc;
    
    /* smoke test */
    if (PMIX_SUCCESS != 0) {
        TEST_ERROR(("ERROR IN COMPUTING CONSTANTS: PMIX_SUCCESS = %d", PMIX_SUCCESS));
        exit(1);
    }

    TEST_VERBOSE(("Testing version %s", PMIx_Get_version()));

    TEST_VERBOSE(("Start PMIx regex smoke test"));

    fprintf(stderr, "NODES: %s\n", TEST_NODES);
    PMIx_generate_regex(TEST_NODES, &regex);
    fprintf(stderr, "REGEX: %s\n\n", regex);
    /* test reverse parsing */
    rc = pmix_regex_parse_nodes(regex, &nodes);
    free(regex);
    if (PMIX_SUCCESS == rc) {
        regex = pmix_argv_join(nodes, ',');
        pmix_argv_free(nodes);
        fprintf(stderr, "NODES: %s\n", TEST_NODES);
        fprintf(stderr, "RSULT: %s\n\n\n", regex);
        free(regex);
    } else {
        fprintf(stderr, "Node reverse failed: %d\n\n\n", rc);
    }
    
    fprintf(stderr, "PROCS: %s\n", TEST_PROCS);
    PMIx_generate_ppn(TEST_PROCS, &regex);
    fprintf(stderr, "PPN: %s\n\n", regex);
    /* test reverse parsing */
    rc = pmix_regex_parse_procs(regex, &procs);
    free(regex);
    if (PMIX_SUCCESS == rc) {
        regex = pmix_argv_join(procs, ';');
        pmix_argv_free(procs);
        fprintf(stderr, "PROCS: %s\n", TEST_PROCS);
        fprintf(stderr, "RSULT: %s\n", regex);
        free(regex);
    } else {
        fprintf(stderr, "PPN reverse failed: %d\n", rc);
    }

    return 0;
}

