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
#include <slurm/pmi2.h>
#include "pmi2_pmap_parser.h"
#include "pmi2_utils.h"

static int my_rank, my_node;
static char * kvs_name;

void pmi_init(int *rank, int *size)
{
    int spawned, appnum;
    int rc;

    *size = -1;
    *rank = -1;
    appnum = -1;

    if (PMI2_SUCCESS != (rc = PMI2_Init(&spawned, size, rank, &appnum))) {
        fprintf(stderr, "pmi2: PMI2_Init: error rc = %d\n", rc);
        abort();
    }
    if( *size < 0 || *rank < 0 ){
        fprintf(stderr, "pmi2: PMI2_Init: bad size=%d or rank=%d values\n",
               *size, *rank);
        abort();
    }
    my_rank = *rank;

    kvs_name = (char*)malloc(PMI2_MAX_VALLEN);
    if( kvs_name == NULL ){
        fprintf(stderr, "pmi_init (pmi2): cannot malloc kvs name buffer\n");
        abort();
    }
    rc = PMI2_Job_GetId(kvs_name, PMI2_MAX_VALLEN);
    if( PMI2_SUCCESS != rc ){
        fprintf(stderr, "pmi_init (pmi2): cannot get kvs name: rc = %d\n", rc);
        abort();
    }
}

void pmi_get_local_ranks(int **local_ranks, int *local_cnt)
{
    int rc, found;

    char *pmapping = (char*)malloc(PMI2_MAX_VALLEN);
    if( pmapping == NULL ){
        fprintf(stderr,"pmi_get_local_ranks (pmi2): cannot mallic for pmappig\n");
        abort();
    }


    rc = PMI2_Info_GetJobAttr("PMI_process_mapping", pmapping, PMI2_MAX_VALLEN, &found);
    if( !found || PMI2_SUCCESS != rc ) {
        fprintf(stderr,"pmi_get_local_ranks (pmi2): PMI2_Info_GetJobAttr rc = %d\n", rc);
        abort();
    }

    *local_ranks = NULL;
    *local_ranks = mca_common_pmi2_parse_pmap(pmapping, my_rank, &my_node, local_cnt);
    if (NULL == *local_ranks) {
        fprintf(stderr,"mca_common_pmi2_parse_pmap: error\n");
        abort();
    }

    free(pmapping);
}

void pmi_get_shmem_size(char *is_avail, size_t *size)
{
    *is_avail = 0;
}

void pmi_put_key_loc(char *key, int *key_val, int key_size)
{
    char *encoded = pmi_encode(key_val, key_size * sizeof(int));
    if( NULL == encoded ){
        fprintf(stderr, "pmi_encode: error on key: %s\n", key);
        abort();
    }
    PMI2_Info_PutNodeAttr(key, encoded);
}

void pmi_put_key_rem(char *key, int *key_val, int key_size)
{
    char *encoded = pmi_encode(key_val, key_size * sizeof(int));
    if( NULL == encoded ){
        fprintf(stderr, "pmi_encode: error on key: %s\n", key);
        abort();
    }

    PMI2_KVS_Put(key, encoded);
}

void pmi_put_double(char *key, double val)
{
    char buf[128];
    sprintf(buf, "%lf", val);
    PMI2_KVS_Put(key, buf);
}


void pmi_commit()
{

}

void pmi_fence(int collect)
{
    int rc;
    /* now call fence */
    if (PMI2_SUCCESS != (rc = PMI2_KVS_Fence()) ) {
        fprintf(stderr, "PMI2_KVS_Fence: error %d\n", rc);
        abort();
    }
}

void pmi_fini()
{
    PMI2_Finalize();
}

void pmi_get_key_loc(int rank, char *key_name, int **key_val, int *key_size)
{
    int found, rc;
    size_t tmp_size;
    char *tmp = calloc(PMI2_MAX_VALLEN, sizeof(char));
    if( (rc = PMI2_Info_GetNodeAttr(key_name, tmp, PMI2_MAX_VALLEN, &found, 1) ) ){
        fprintf(stderr,"PMI2_Info_GetNodeAttr: error rc = %d\n", rc);
        abort();
    }
    if( !found ){
        fprintf(stderr,"pmi_get_key_loc: key %s not found\n", key_name);
        abort();
    }

    *key_val = (int*)pmi_decode(tmp, &tmp_size);
    *key_size = tmp_size / sizeof(int);

    if( NULL == *key_val ){
        fprintf(stderr,"pmi_decode: cannot decode key %s\n", key_name);
        abort();
    }
    free(tmp);
}

void pmi_get_key_rem(int rank, char *key_name, int **key_val, int *key_size)
{
    int len, rc;
    size_t tmp_size;

    char *tmp = calloc(PMI2_MAX_VALLEN, sizeof(char));
    if( (rc = PMI2_KVS_Get(kvs_name, PMI2_ID_NULL, key_name, tmp, PMI2_MAX_VALLEN, &len) ) ){
        fprintf(stderr,"PMI2_Info_GetNodeAttr: error rc = %d\n", rc);
        abort();
    }

    *key_val = (int*)pmi_decode(tmp, &tmp_size);
    *key_size = tmp_size / sizeof(int);

    if( NULL == *key_val ){
        fprintf(stderr,"pmi_decode: cannot decode key %s\n", key_name);
        abort();
    }
    free(tmp);
}

double pmi_get_double(int rank, char *key)
{
    int len, rc;
    size_t tmp_size;
    char *tmp = calloc(PMI2_MAX_VALLEN, sizeof(char));
    double v;

    if( (rc = PMI2_KVS_Get(kvs_name, PMI2_ID_NULL, key, tmp, PMI2_MAX_VALLEN, &len) ) ){
        fprintf(stderr,"PMI2_Info_GetNodeAttr: error rc = %d\n", rc);
        abort();
    }
    sscanf(tmp, "%lf", &v);
    free(tmp);
    return v;
}
