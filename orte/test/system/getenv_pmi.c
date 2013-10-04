/*
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
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
    int num_local_procs = 0;
    int *local_rank_ids = NULL;
    int spawned = PMI_FALSE;
    int rc = EXIT_SUCCESS;
    char *err = NULL;
    PMI_BOOL pmi_initialized = PMI_FALSE;
    int pmi_vallen_max, max_length;
    char *pmi_kvs_name;

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
    if (PMI_SUCCESS != PMI_Get_clique_size(&num_local_procs)) {
        err = "PMI_Get_clique_size failure!";
        goto done;
    }
    if (PMI_SUCCESS != PMI_KVS_Get_value_length_max(&pmi_vallen_max)) {
        err = "PMI_KVS_Get_value_length_max failure!";
        goto done;
    }
    if (PMI_SUCCESS != PMI_KVS_Get_name_length_max(&max_length)) {
        err = "PMI_KVS_Get_name_length_max failure!";
        goto done;
    }
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        err = "malloc failure!";
        goto done;
    }
    if (PMI_SUCCESS != PMI_KVS_Get_my_name(pmi_kvs_name,max_length)) {
        err = "PMI_KVS_Get_my_name failure!";
        goto done;
    }

    if (NULL == (local_rank_ids = calloc(num_local_procs, sizeof(int)))) {
        err = "out of resources";
        goto done;
    }
    if (PMI_SUCCESS != PMI_Get_clique_ranks(local_rank_ids, num_local_procs)) {
        err = "PMI_Get_clique_size failure!";
        goto done;
    }
    /* lowest local rank will print env info and tag its output*/
    //    if (pmi_rank == local_rank_ids[0]) {
    //  for (; NULL != envp && NULL != *envp; ++envp) {
    //      printf("===[%d]: %s\n",  pmi_rank, *envp);
    //  }
    //}

done:
    if (PMI_TRUE == pmi_initialized) {
        if (PMI_SUCCESS != PMI_Finalize()) {
            err = "PMI_Finalize failure!";
        }
    }
    if (NULL != err) {
        fprintf(stderr, "=== ERROR [rank:%d] %s\n", pmi_rank, err);
        rc = EXIT_FAILURE;
    }
    return rc;
}
