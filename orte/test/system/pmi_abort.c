/*
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "pmi.h"

/* NOTES
 *
 * useful debug environment variables:
 * PMI_DEBUG
 */

int main(int argc, char **argv, char **envp)
{
    int pmi_rank = -1;
    int pmi_process_group_size = -1;
    int rc = EXIT_SUCCESS;
    char *err = NULL;
    PMI_BOOL pmi_initialized = PMI_FALSE;
    int i;
    double pi;
    int spawned;

    if (1 < argc) {
        rc = strtol(argv[1], NULL, 10);
    } else {
        rc = 3;
    }

    /* sanity */
    if (PMI_SUCCESS != PMI_Initialized(&pmi_initialized) ||
        PMI_TRUE == pmi_initialized) {
        fprintf(stderr, "=== ERROR: PMI sanity failure\n");
        return EXIT_FAILURE;
    }
    if (PMI_SUCCESS != PMI_Init(&spawned)) {
        err = "PMI_Init failure!";
        goto done;
    }
    if (PMI_SUCCESS != PMI_Get_size(&pmi_process_group_size)) {
        err = "PMI_Get_size failure!";
        goto done;
    }
    if (PMI_SUCCESS != PMI_Get_rank(&pmi_rank)) {
        err = "PMI_Get_rank failure!";
        goto done;
    }
 
    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 10000) i = 0;
        if ((pmi_rank == 3 || 
             (pmi_process_group_size <= 3 && pmi_rank == 0))
            && i == 9995) {
            asprintf(&err, "RANK%d CALLED ABORT", pmi_rank);
            fprintf(stderr, "%s\n", err);
            fflush(stderr);
            PMI_Abort(rc, err);
        }
    }

 done:
    if (NULL != err) {
        fprintf(stderr, "=== ERROR [rank:%d] %s\n", pmi_rank, err);
        rc = EXIT_FAILURE;
    }
    return rc;
}
