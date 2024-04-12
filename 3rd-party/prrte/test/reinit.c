/*
 * Copyright (c) 2020      Triad National Security, LLC.
 *                         All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Test of rank position identification inside group */

#include "pmix.h"
#include <stdio.h>
#include <stdlib.h>
/*
#include <assert.h>
#include <sys/time.h>
#include <stdarg.h>
#include <unistd.h>
*/

pmix_proc_t this_proc;

int main(int argc, char *argv[]) {

    pmix_value_t *val;
    size_t ninfo = 0;
    pmix_proc_t job_proc;
    int count = 0;

    /* initialization */
try_again:
    fprintf(stderr, "initializing pmix\n");
    PMIx_Init(&this_proc, NULL, ninfo);
    fprintf(stderr, "initialized pmix\n");

    fprintf(stderr, "calling pmix fence\n");
    PMIx_Fence(NULL, 0, NULL, 0);
    fprintf(stderr, "called pmix fence\n");

    /* Handles everything that needs to happen after PMIx_Init() */

    job_proc = this_proc;
    job_proc.rank = PMIX_RANK_WILDCARD; // Note that PMIX_RANK_WILDCARD == -2
    fprintf(stderr, "getting job size\n");
    PMIx_Get(&job_proc, PMIX_JOB_SIZE, NULL, 0, &val);
    /* After using PMIx_Get to get a value, we need to compare it our validation parameters
       we've passed as an argument; this is the main purpose of pmixt_validate_predefined(). */


    fprintf(stderr, "getting universesize\n");
    PMIx_Get(&job_proc, PMIX_UNIV_SIZE, NULL, 0, &val);

    fprintf(stderr, "getting local size\n");
    PMIx_Get(&job_proc, PMIX_LOCAL_SIZE, NULL, 0, &val);
    fprintf(stderr, "getting local rank\n");
    PMIx_Get(&this_proc, PMIX_LOCAL_RANK, NULL, 0, &val);

    fprintf(stderr, "getting local nodid\n");
    PMIx_Get(&this_proc, PMIX_NODEID, NULL, 0, &val);

    fprintf(stderr, "getting local peers\n");
    PMIx_Get(&job_proc, PMIX_LOCAL_PEERS, NULL, 0, &val);

    fprintf(stderr, "getting hostname\n");
    PMIx_Get(&this_proc, PMIX_HOSTNAME, NULL, 0, &val);

    fprintf(stderr, "getting my rank\n");
    PMIx_Get(&job_proc, PMIX_RANK, NULL, 0, &val);


    /* finalize */
    fprintf(stderr, "finalizing pmix\n");
    PMIx_Finalize(NULL, 0);
    fprintf(stderr, "finalized pmix\n");
    count++;
    if (count < 2) {
       goto try_again;
    }

   exit(0);

}

