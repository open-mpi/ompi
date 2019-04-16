/*
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "test_fence.h"

static void get_cb(pmix_status_t status, pmix_value_t *kv, void *cbdata)
{
    get_cbdata *cb = (get_cbdata*)cbdata;
    PMIX_ACQUIRE_OBJECT(cb);
    if (PMIX_SUCCESS == status) {
        pmix_value_xfer(cb->kv, kv);
    }
    cb->status = status;
    PMIX_POST_OBJECT(cb);
    cb->in_progress = 0;
}

static void add_noise(char *noise_param, char *my_nspace, pmix_rank_t my_rank)
{
    bool participate = false;
    participant_t *p;

    parse_noise(noise_param, 1);
    if (NULL != noise_range) {
        PMIX_LIST_FOREACH(p, noise_range, participant_t) {
            if (0 == strncmp(my_nspace, p->proc.nspace, strlen(my_nspace)) &&
                (my_rank == p->proc.rank || PMIX_RANK_WILDCARD == p->proc.rank)) {
                participate = true;
                break;
            }
        }
        if (participate) {
            sleep(2);
            TEST_VERBOSE(("I'm %s:%d sleeping\n", my_nspace, my_rank));
        }
        PMIX_LIST_RELEASE(noise_range);
        noise_range = NULL;
    }
}

static void release_cb(pmix_status_t status, void *cbdata)
{
    int *ptr = (int*)cbdata;
    *ptr = 0;
}

int test_fence(test_params params, char *my_nspace, pmix_rank_t my_rank)
{
    int len;
    int rc;
    size_t i, npcs;
    fence_desc_t *desc;
    participant_t *p, *next;
    pmix_proc_t *pcs;
    bool participate;
    int fence_num = 0;
    char sval[500];
    int put_ind;

    if (NULL != params.noise) {
        add_noise(params.noise, my_nspace, my_rank);
    }

    PMIX_CONSTRUCT(&test_fences, pmix_list_t);
    parse_fence(params.fences, 1);

    TEST_VERBOSE(("fences %s\n", params.fences));

    /* cycle thru all the test fence descriptors to find
     * those that include my nspace/rank */
    PMIX_LIST_FOREACH(desc, &test_fences, fence_desc_t) {
        char tmp[256] = {0};
        len = sprintf(tmp, "fence %d: block = %d de = %d ", fence_num, desc->blocking, desc->data_exchange);
        participate = false;
        /* search the participants */
        PMIX_LIST_FOREACH(p, desc->participants, participant_t) {
            if (0 == strncmp(my_nspace, p->proc.nspace, strlen(my_nspace)) &&
                (my_rank == p->proc.rank || PMIX_RANK_WILDCARD == p->proc.rank)) {
                participate = true;
            }
            if (PMIX_RANK_WILDCARD == p->proc.rank) {
                len += sprintf(tmp+len, "all; ");
            } else {
                len += sprintf(tmp+len, "%d,", p->proc.rank);
            }
        }
        TEST_VERBOSE(("%s\n", tmp));
        if (participate) {
            /*run fence test on this range */
            /* first put value (my_ns, my_rank) with key based on fence_num to split results of different fences*/
            put_ind = 0;
            (void)snprintf(sval, 500, "%d:%s:%d", fence_num, my_nspace, my_rank);
            PUT(string, sval, PMIX_GLOBAL, fence_num, put_ind++, params.use_same_keys);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            PUT(int, fence_num+my_rank, PMIX_GLOBAL, fence_num, put_ind++, params.use_same_keys);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            PUT(float, fence_num+1.1, PMIX_GLOBAL, fence_num, put_ind++, params.use_same_keys);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            PUT(uint32_t, fence_num+14, PMIX_GLOBAL, fence_num, put_ind++, params.use_same_keys);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            PUT(uint16_t, fence_num+15, PMIX_GLOBAL, fence_num, put_ind++, params.use_same_keys);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            /* Submit the data */
            if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
                TEST_ERROR(("%s:%d: PMIx_Commit failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                return rc;
            }

            /* setup the fence */
            npcs = pmix_list_get_size(desc->participants);
            PMIX_PROC_CREATE(pcs, npcs);
            i = 0;
            PMIX_LIST_FOREACH(p, desc->participants, participant_t) {
                (void)strncpy(pcs[i].nspace, p->proc.nspace, PMIX_MAX_NSLEN);
                pcs[i].rank = p->proc.rank;
                i++;
            }

            /* perform fence */
            FENCE(desc->blocking, desc->data_exchange, pcs, npcs);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Fence failed: %d", my_nspace, my_rank, rc));
                PMIX_LIST_DESTRUCT(&test_fences);
                PMIX_PROC_FREE(pcs, npcs);
                return rc;
            }

            /* replace all items in the list with PMIX_RANK_WILDCARD rank by real ranks to get their data. */
            pmix_proc_t *ranks;
            size_t nranks;
            PMIX_LIST_FOREACH_SAFE(p, next, desc->participants, participant_t) {
                if (PMIX_RANK_WILDCARD == p->proc.rank) {
                    rc = get_all_ranks_from_namespace(params, p->proc.nspace, &ranks, &nranks);
                    if (PMIX_SUCCESS != rc) {
                        TEST_ERROR(("%s:%d: Can't parse --ns-dist value in order to get ranks for namespace %s", my_nspace, my_rank, p->proc.nspace));
                        PMIX_LIST_DESTRUCT(&test_fences);
                        return PMIX_ERROR;
                    }
                    pmix_list_remove_item(desc->participants, (pmix_list_item_t*)p);
                    for (i = 0; i < nranks; i++) {
                        participant_t *prt;
                        prt = PMIX_NEW(participant_t);
                        strncpy(prt->proc.nspace, ranks[i].nspace, strlen(ranks[i].nspace)+1);
                        prt->proc.rank = ranks[i].rank;
                        pmix_list_append(desc->participants, &prt->super);
                    }
                    PMIX_PROC_FREE(ranks, nranks);
                }
            }

            /* get data from all participating in this fence clients */
            PMIX_LIST_FOREACH(p, desc->participants, participant_t) {
                put_ind = 0;
                snprintf(sval, 500, "%d:%s:%d", fence_num, p->proc.nspace, p->proc.rank);
                GET(string, sval, p->proc.nspace, p->proc.rank, fence_num, put_ind++, params.use_same_keys, 1, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d) from %s:%d", my_nspace, my_rank, rc, p->proc.nspace, p->proc.rank));
                    PMIX_PROC_FREE(pcs, npcs);
                    PMIX_LIST_DESTRUCT(&test_fences);
                    return rc;
                }
                GET(int, (int)(fence_num+p->proc.rank), p->proc.nspace, p->proc.rank, fence_num, put_ind++, params.use_same_keys, 0, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d) from %s:%d", my_nspace, my_rank, rc, p->proc.nspace, p->proc.rank));
                    PMIX_PROC_FREE(pcs, npcs);
                    PMIX_LIST_DESTRUCT(&test_fences);
                    return rc;
                }
                GET(float, fence_num+1.1, p->proc.nspace, p->proc.rank, fence_num, put_ind++, params.use_same_keys, 1, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d) from %s:%d", my_nspace, my_rank, rc, p->proc.nspace, p->proc.rank));
                    PMIX_PROC_FREE(pcs, npcs);
                    PMIX_LIST_DESTRUCT(&test_fences);
                    return rc;
                }
                GET(uint32_t, (uint32_t)fence_num+14, p->proc.nspace, p->proc.rank, fence_num, put_ind++, params.use_same_keys, 0, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d) from %s:%d", my_nspace, my_rank, rc, p->proc.nspace, p->proc.rank));
                    PMIX_PROC_FREE(pcs, npcs);
                    PMIX_LIST_DESTRUCT(&test_fences);
                    return rc;
                }
                GET(uint16_t, fence_num+15, p->proc.nspace, p->proc.rank, fence_num, put_ind++, params.use_same_keys, 1, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d) from %s:%d", my_nspace, my_rank, rc, p->proc.nspace, p->proc.rank));
                    PMIX_PROC_FREE(pcs, npcs);
                    PMIX_LIST_DESTRUCT(&test_fences);
                    return rc;
                }
            }
            /* barrier across participating processes to prevent putting new values with the same key
             * before finishing data exchange with other processes. */
            FENCE(1, 0, pcs, npcs);
            PMIX_PROC_FREE(pcs, npcs);
        }
        fence_num++;
    }

    PMIX_LIST_DESTRUCT(&test_fences);
    return PMIX_SUCCESS;
}

static int get_local_peers(char *my_nspace, int my_rank, pmix_rank_t **_peers, pmix_rank_t *count)
{
    pmix_value_t *val;
    pmix_rank_t *peers = NULL;
    char *sptr, *token, *eptr, *str;
    pmix_rank_t npeers;
    int rc;
    pmix_proc_t proc;

    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    /* get number of neighbours on this node */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_SIZE, NULL, 0, &val))) {
        TEST_ERROR(("%s:%d: PMIx_Get local peer # failed: %d", my_nspace, my_rank, rc));
        return rc;
    }
    if (NULL == val) {
        TEST_ERROR(("%s:%d: PMIx_Get local peer # returned NULL value", my_nspace, my_rank));
        return PMIX_ERROR;
    }

    if (val->type != PMIX_UINT32  ) {
        TEST_ERROR(("%s:%d: local peer # attribute value type mismatch,"
                " want %d get %d(%d)",
                my_nspace, my_rank, PMIX_UINT32, val->type));
        return PMIX_ERROR;
    }
    npeers = val->data.uint32;
    peers = malloc(sizeof(pmix_rank_t) * npeers);

    /* get ranks of neighbours on this node */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
        TEST_ERROR(("%s:%d: PMIx_Get local peers failed: %d", my_nspace, my_rank, rc));
        free(peers);
        return rc;
    }
    if (NULL == val) {
        TEST_ERROR(("%s:%d: PMIx_Get local peers returned NULL value", my_nspace, my_rank));
        free(peers);
        return PMIX_ERROR;
    }

    if (val->type != PMIX_STRING  ) {
        TEST_ERROR(("%s:%d: local peers attribute value type mismatch,"
                " want %d get %d(%d)",
                my_nspace, my_rank, PMIX_UINT32, val->type));
        free(peers);
        return PMIX_ERROR;
    }

    *count = 0;
    sptr = NULL;
    str = val->data.string;
    do{
        if( *count > npeers ){
            TEST_ERROR(("%s:%d: Bad peer ranks number: should be %d, actual %d (%s)",
                my_nspace, my_rank, npeers, *count, val->data.string));
            free(peers);
            return PMIX_ERROR;
        }
        token = strtok_r(str, ",", &sptr);
        str = NULL;
        if( NULL != token ){
            peers[(*count)++] = strtol(token,&eptr,10);
            if( *eptr != '\0' ){
                TEST_ERROR(("%s:%d: Bad peer ranks string", my_nspace, my_rank));
                free(peers);
                return PMIX_ERROR;
            }
        }

    } while( NULL != token );

    if( *count != npeers ){
        TEST_ERROR(("%s:%d: Bad peer ranks number: should be %d, actual %d (%s)",
                my_nspace, my_rank, npeers, *count, val->data.string));
        free(peers);
        return PMIX_ERROR;
    }
    *_peers = peers;
    return PMIX_SUCCESS;
}

int test_job_fence(test_params params, char *my_nspace, pmix_rank_t my_rank)
{
    int rc;
    int i, j;
    char sval[50];
    pmix_rank_t *peers, npeers;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_proc_t proc;

    (void)strncpy(proc.nspace, my_nspace, PMIX_MAX_NSLEN);
    proc.rank = my_rank;

    for (i=0; i < 3; i++) {
        PUT(int, 12340 + i, PMIX_LOCAL, 100, i, 0);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
            return rc;
        }

        (void)snprintf(sval, 50, "%s:%d", my_nspace, my_rank);
        PUT(string, sval, PMIX_REMOTE, 101, i, 0);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
            return PMIX_ERROR;
        }

        PUT(float, (float)12.15 + i, PMIX_GLOBAL, 102, i, 0);
        if (PMIX_SUCCESS != rc) {
            TEST_ERROR(("%s:%d: PMIx_Put failed: %d", my_nspace, my_rank, rc));
            return PMIX_ERROR;
        }
    }

    /* Submit the data */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        TEST_ERROR(("%s:%d: PMIx_Commit failed: %d", my_nspace, my_rank, rc));
        return PMIX_ERROR;
    }

    /* Perform a fence if was requested */
    FENCE(!params.nonblocking, params.collect, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        TEST_ERROR(("%s:%d: PMIx_Fence failed: %d", my_nspace, my_rank, rc));
        return rc;
    }

    if (PMIX_SUCCESS != (rc = get_local_peers(my_nspace, my_rank, &peers, &npeers))) {
        return PMIX_ERROR;
    }

    /* Check the predefined output */
    for (i=0; i < (int)params.ns_size; i++) {

        for (j=0; j < 3; j++) {

            int local = 0;
            pmix_rank_t k;
            for(k=0; k<npeers; k++){
                if( peers[k] == i+params.base_rank){
                    local = 1;
                }
            }
            if( local ){
                GET(int, (12340+j), my_nspace, i+params.base_rank, 100, j, 0, 0, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed: %s", my_nspace, my_rank, PMIx_Error_string(rc)));
                    return PMIX_ERROR;
                }

                snprintf(sval, 50, "%s:%d", my_nspace, i+params.base_rank);
                GET(string, sval, my_nspace, i+params.base_rank, 101, j, 0, 1, 1);
                if (PMIX_SUCCESS == rc && (i+params.base_rank) != my_rank ) {
                    TEST_ERROR(("%s:%d: PMIx_Get of remote key on local proc", my_nspace, my_rank));
                    return PMIX_ERROR;
                }
            } else {
                GET(int, (12340+j), my_nspace, i+params.base_rank, 100, j, 0, 0, 1);
                if (PMIX_SUCCESS == rc && (i+params.base_rank) != my_rank) {
                    TEST_ERROR(("%s:%d: PMIx_Get of local key on the remote proc", my_nspace, my_rank));
                    return PMIX_ERROR;
                }

                snprintf(sval, 50, "%s:%d", my_nspace, i+params.base_rank);
                GET(string, sval, my_nspace, i+params.base_rank, 101, j, 0, 1, 0);
                if (PMIX_SUCCESS != rc) {
                    TEST_ERROR(("%s:%d: PMIx_Get failed (%d)", my_nspace, my_rank, rc));
                    return PMIX_ERROR;
                }
            }

            GET(float, (float)12.15 + j, my_nspace, i+params.base_rank, 102, j, 0, 0, 0);
            if (PMIX_SUCCESS != rc) {
                TEST_ERROR(("%s:%d: PMIx_Get failed (%d)", my_nspace, my_rank, rc));
                return PMIX_ERROR;
            }
        }

        /* ask for a non-existent key */
        proc.rank = i+params.base_rank;
        if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, "foobar", NULL, 0, &val))) {
            TEST_ERROR(("%s:%d: PMIx_Get returned success instead of failure",
                        my_nspace, my_rank));
            return PMIX_ERROR;
        }
        if (PMIX_ERR_NOT_FOUND != rc && PMIX_ERR_PROC_ENTRY_NOT_FOUND != rc) {
            TEST_ERROR(("%s:%d [ERROR]: PMIx_Get returned %s instead of not_found",
                        my_nspace, my_rank, PMIx_Error_string(rc)));
            return PMIX_ERROR;
        }
        if (NULL != val) {
            TEST_ERROR(("%s:%d [ERROR]: PMIx_Get did not return NULL value", my_nspace, my_rank));
            return PMIX_ERROR;
        }
        TEST_VERBOSE(("%s:%d: rank %d is OK", my_nspace, my_rank, i+params.base_rank));
    }
    return PMIX_SUCCESS;
}
