/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2016      Intel, Inc.  All rights reserved.
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
#include <time.h>
#include <getopt.h>
#include <limits.h>
#include <string.h>
#include <pmix.h>

pmix_proc_t this_proc;
static int _int_size = 0;

void pmi_init(int *rank, int *size)
{
    pmix_value_t value, *val = &value;
    pmix_proc_t job_proc;
    int rc;

    /* init us */
#if (PMIX_VERSION_MAJOR == 1 )
    if (PMIX_SUCCESS != (rc = PMIx_Init(&this_proc)))
#else
    if (PMIX_SUCCESS != (rc = PMIx_Init(&this_proc, NULL, 0)))
#endif
    {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }

    job_proc = this_proc;
#if (PMIX_VERSION_MAJOR > 1 )
    job_proc.rank = PMIX_RANK_WILDCARD;
#endif
    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&job_proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
    _int_size = *size = val->data.uint32;
    *rank = this_proc.rank;
    PMIX_VALUE_RELEASE(val);
}

void pmi_get_local_ranks(int **local_ranks, int *local_cnt)
{
    pmix_value_t value, *val = &value;
    char *ptr;
    int i, rc;
    pmix_proc_t job_proc = this_proc;
#if (PMIX_VERSION_MAJOR > 1 )
    job_proc.rank = PMIX_RANK_WILDCARD;
#endif

    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&job_proc, PMIX_LOCAL_SIZE, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get PMIX_LOCAL_SIZE failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
    *local_cnt = val->data.uint32;
    PMIX_VALUE_RELEASE(val);

    *local_ranks = calloc(*local_cnt, sizeof(int));
    /* get our job size */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&job_proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get PMIX_LOCAL_PEERS failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
    ptr = val->data.string;
    for(i=0; NULL != ptr && i < *local_cnt; i++ ){
         char *loc_rank = strsep(&ptr, ",");
         (*local_ranks)[i] = atoi(loc_rank);
    }
    if( i != *local_cnt || NULL != ptr ){
        fprintf(stderr, "Client ns %s rank %d: number of local peers doesn't match",
                this_proc.nspace, this_proc.rank);
        abort();
    }
}

/* WARNING: should match one in
 * src/mca/gds/ds12/gds_dstore.h
 */
typedef struct {
    size_t rank;
    size_t offset;
    size_t count;
} rank_meta_info;

#define DSTORE_INIT_SEG "initial-pmix_shared-segment"
#define DSTORE_META_SEG "smseg"
#define DSTORE_DATA_SEG "smdataseg"

typedef enum { other_seg, init_seg, meta_seg, data_seg } seg_type_t;

struct {
    char *sign;
    seg_type_t type;
} segments[] = {
    {DSTORE_INIT_SEG, init_seg},
    {DSTORE_META_SEG, meta_seg},
    {DSTORE_DATA_SEG, data_seg},
};

static char *_get_maps_file(char *line)
{
    char *token = NULL;
    char *saveptr = NULL;
    int i = 0;

    token = strtok_r(line, "-\n", &saveptr);
    if(NULL == token) {
        return NULL;
    }
    return (char*)strtoul(token, &saveptr, 16);
}

static seg_type_t
is_dstor_region(char *line, char **base)
{
    int i;
    seg_type_t type = other_seg;
    for(i=0; i<3; i++){
        if( strstr(line, segments[i].sign) ){
            if(!(*base = _get_maps_file(line)) ){
                return other_seg;
            }
            return segments[i].type;
        }
    }
    return other_seg;
}

void pmi_get_shmem_size(char *is_avail, size_t *cum_size)
{
    char maps_path[PATH_MAX] = "/proc/self/maps";
    FILE *maps_fp = NULL;
    char *line = NULL;
    size_t size = 0, meta_size = 0, data_size = 0;
    char *base;

    if (NULL == (maps_fp = fopen(maps_path, "r"))) {
        abort();
    }

    while ((size = getline(&line, &size, maps_fp)) != -1) {
        seg_type_t type = is_dstor_region(line, &base);
        switch(type) {
        case other_seg:
        case init_seg:
        case meta_seg:
            break;
        case data_seg:{
            data_size += *((size_t*)base);
            break;
        }
        }
    }
    free(line);
    fclose(maps_fp);
    *is_avail = 0;
    *cum_size = data_size;
    if( *cum_size > 0 ) {
        *is_avail = 1;
        *cum_size += sizeof(rank_meta_info) * _int_size;
    }
}

static void _put_key(char *key, int *key_val, int key_size, pmix_scope_t scope)
{
    pmix_value_t value;
    int rc;

    value.type = PMIX_BYTE_OBJECT;
    value.data.bo.size = key_size * sizeof(int);
    value.data.bo.bytes = (char*)key_val;
    if (PMIX_SUCCESS != (rc = PMIx_Put(scope, key, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
}

void pmi_put_key_loc(char *key, int *key_val, int key_size)
{
    _put_key(key, key_val, key_size, PMIX_LOCAL);
}

void pmi_put_key_rem(char *key, int *key_val, int key_size)
{
    _put_key(key, key_val, key_size, PMIX_REMOTE);
}

void pmi_put_double(char *key, double v)
{
    pmix_value_t value;
    int rc;

    value.type = PMIX_DOUBLE;
    value.data.dval = v;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, key, &value))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Put internal failed: %d", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
}

void pmi_commit()
{
    int rc;

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Commit failed: %d",
                this_proc.nspace, this_proc.rank, rc);
        abort();
    }
}


void pmi_fence(int collect)
{
    pmix_info_t *info = NULL;
    pmix_proc_t proc;
    bool value = 1;
    int ninfo = 0;
    int rc;

    if( collect ){
        PMIX_INFO_CREATE(info, 1);
        (void)strncpy(info->key, PMIX_COLLECT_DATA, PMIX_MAX_KEYLEN);
        pmix_value_load(&info->value, &value, PMIX_BOOL);
        ninfo = 1;
    }

    /* call fence to ensure the data is received */
    PMIX_PROC_CONSTRUCT(&proc);
    (void)strncpy(proc.nspace, this_proc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    if (PMIX_SUCCESS != (rc = PMIx_Fence(&proc, 1, info, ninfo))) {
        fprintf(stderr,  "Client ns %s rank %d: PMIx_Fence failed: %d",
                this_proc.nspace, this_proc.rank, rc);
        abort();
    }

    if( collect ){
        PMIX_INFO_FREE(info, ninfo);
    }
}

void _get_key(int rank, char *key_name, int **key_val, int *key_size)
{
    pmix_proc_t proc;
    pmix_value_t value, *val = &value;
    int rc;

    (void)strncpy(proc.nspace, this_proc.nspace, PMIX_MAX_NSLEN);
    proc.rank = rank;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, key_name, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d",
                this_proc.nspace, this_proc.rank, key_name, rc);
        abort();
    }
    if (PMIX_BYTE_OBJECT != val->type) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d",
                this_proc.nspace, this_proc.rank, key_name, val->type);
        PMIX_VALUE_RELEASE(val);
        abort();
    }
    *key_val = (int*)val->data.bo.bytes;
    *key_size = val->data.bo.size / sizeof(int);
    val->data.bo.bytes = NULL;
    PMIX_VALUE_RELEASE(val);
}

void pmi_get_key_loc(int rank, char *key_name, int **key_val, int *key_size)
{
    _get_key(rank, key_name, key_val, key_size);
}

void pmi_get_key_rem(int rank, char *key_name, int **key_val, int *key_size)
{
    _get_key(rank, key_name, key_val, key_size);
}

double pmi_get_double(int rank, char *key_name)
{
    pmix_proc_t proc;
    pmix_value_t value, *val = &value;
    int rc;
    double v;

    (void)strncpy(proc.nspace, this_proc.nspace, PMIX_MAX_NSLEN);
    proc.rank = rank;
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, key_name, NULL, 0, &val))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s failed: %d",
                this_proc.nspace, this_proc.rank, key_name, rc);
        abort();
    }
    if (PMIX_DOUBLE != val->type) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get %s returned wrong type: %d",
                this_proc.nspace, this_proc.rank, key_name, val->type);
        PMIX_VALUE_RELEASE(val);
        abort();
    }
    v = val->data.dval;
    PMIX_VALUE_RELEASE(val);
    return v;
}

pmi_fini()
{
    int rc;
#if (PMIX_VERSION_MAJOR == 1 )
    if (PMIX_SUCCESS != (rc = PMIx_Finalize()))
#else
    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0)))
#endif
    {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", this_proc.nspace, this_proc.rank, rc);
        abort();
    }
}
