
/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <slurm/pmi.h>
#include "pmi2_pmap_parser.h"
#include "pmi2_utils.h"

static int my_node;
static char *kvs_name;


static int kvslen_max = 0;
static int keylen_max = 0;
static int vallen_max = 0;


void pmi_init(int *rank, int *size)
{
    int spawned, appnum;
    int rc;

    *size = -1;
    *rank = -1;
    appnum = -1;

    if (PMI_SUCCESS != (rc = PMI_Init(&spawned))) {
        fprintf(stderr, "pmi1: PMI_Init: error rc = %d\n", rc);
        abort();
    }

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&kvslen_max))) {
        fprintf(stderr, "pmi1: PMI_KVS_Get_name_length_max: error rc = %d\n", rc);
        abort();
    }

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&keylen_max))) {
        fprintf(stderr, "pmi1: PMI_KVS_Get_key_length_max: error rc = %d\n", rc);
        abort();
    }

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_value_length_max(&vallen_max))) {
        fprintf(stderr, "pmi1: PMI_KVS_Get_value_length_max: error rc = %d\n", rc);
        abort();
    }

    if( PMI_SUCCESS != (rc = PMI_Get_rank(rank)) ) {
        fprintf(stderr, "pmi1: PMI_Get_rank: error rc = %d\n", rc);
        abort();
    }

    kvs_name = (char*)malloc(kvslen_max);
    if (kvs_name == NULL) {
        fprintf(stderr, "pmi1: kvs_name = (char*)malloc(kvslen_max) failed\n");
        abort();
    }

    rc = PMI_KVS_Get_my_name(kvs_name, kvslen_max);
    if (PMI_SUCCESS != rc) {
        fprintf(stderr, "pmi1: PMI_KVS_Get_my_name: error rc = %d\n", rc);
        abort();
    }

    if (PMI_SUCCESS != (rc = PMI_Get_universe_size(size))) {
        fprintf(stderr, "pmi1: PMI_Get_universe_size: error rc = %d\n", rc);
        abort();
    }
}

void pmi_get_local_ranks(int **local_ranks, int *local_cnt)
{
    int rc, found;
    int *lranks = NULL, nlranks;

    /* get our local proc info to find our local rank */
    if (PMI_SUCCESS != (rc = PMI_Get_clique_size(&nlranks))) {
        fprintf(stderr, "pmi1: PMI_Get_clique_size: error rc = %d\n", rc);
        abort();
    }

    lranks = (int*)calloc(nlranks, sizeof(int));
    if (NULL == lranks) {
        fprintf(stderr, "pmi1: lranks = (int*)calloc(nlranks, sizeof(int)) failed\n");
        abort();
    }
    if (PMI_SUCCESS != (rc = PMI_Get_clique_ranks(lranks, nlranks))) {
        fprintf(stderr, "pmi1: PMI_Get_clique_ranks: error rc = %d\n", rc);
        abort();
    }
    *local_ranks = lranks;
    *local_cnt = nlranks;
}

void pmi_get_shmem_size(char *is_avail, size_t *size)
{
    *is_avail = 0;
}

void pmi_put_key_rem(char *key, int *key_val, int key_size)
{
    int rc;
    char *encoded = pmi_encode(key_val, key_size * sizeof(int));
    if( NULL == encoded ){
        fprintf(stderr, "pmi_encode: error on key: %s\n", key);
        abort();
    }
    if( PMI_SUCCESS != (rc = PMI_KVS_Put(kvs_name, key, encoded))) {
        fprintf(stderr, "pmi1: PMI_KVS_Put: error rc = %d\n", rc);
        abort();
    }
}

void pmi_put_key_loc(char *key, int *key_val, int key_size)
{
    /* PMI1 doesn't support key locality */
    pmi_put_key_rem(key, key_val, key_size);
}

void pmi_put_double(char *key, double val)
{
    char buf[128];
    int rc;

    sprintf(buf, "%lf", val);
    if( PMI_SUCCESS != (rc = PMI_KVS_Put(kvs_name, key, buf))) {
        fprintf(stderr, "pmi1: PMI_KVS_Put: error rc = %d\n", rc);
        abort();
    }
}


void pmi_commit()
{
    int rc;
    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(kvs_name))) {
        fprintf(stderr, "pmi1: PMI_KVS_Commit: error rc = %d\n", rc);
        abort();
    }
}

void pmi_fence(int collect)
{
    int rc;

    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        fprintf(stderr, "pmi1: PMI_Barrier: error rc = %d\n", rc);
        abort();
    }
}

void pmi_fini()
{
    PMI_Finalize();
}

void pmi_get_key_rem(int rank, char *key_name, int **key_val, int *key_size)
{
    int rc;
    size_t tmp_size;
    char *tmp = calloc(vallen_max, sizeof(char));

    rc = PMI_KVS_Get(kvs_name, key_name, tmp, vallen_max);
    if( PMI_SUCCESS != rc ){
        fprintf(stderr, "pmi1: PMI_KVS_Get: error rc = %d\n", rc);
        abort();
    }

    *key_val = (int*)pmi_decode(tmp, &tmp_size);
    *key_size = tmp_size / sizeof(int);

    if( NULL == *key_val ){
        fprintf(stderr,"pmi1: pmi_decode: cannot decode key '%s'\n", key_name);
        abort();
    }
    free(tmp);
}

void pmi_get_key_loc(int rank, char *key_name, int **key_val, int *key_size)
{
    /* PMI1 doesn't support local ranks */
    pmi_get_key_rem(rank, key_name, key_val, key_size);
}

double pmi_get_double(int rank, char *key)
{
    int rc;
    char *tmp = calloc(vallen_max, sizeof(char));
    double v;

    rc = PMI_KVS_Get(kvs_name, key, tmp, vallen_max);
    if( PMI_SUCCESS != rc ){
        fprintf(stderr, "pmi1: PMI_KVS_Get: error rc = %d\n", rc);
        abort();
    }
    sscanf(tmp, "%lf", &v);
    free(tmp);
    return v;
}
