#include <pmix.h>
#include <sched.h>
#include <stdio.h>

static pmix_proc_t allproc = PMIX_PROC_STATIC_INIT;
static pmix_proc_t myproc = PMIX_PROC_STATIC_INIT;
static pmix_proc_t * all_procs = NULL;
static bool mywait = false;
static bool refresh = false;
static bool timeout = false;
static bool group = false;
static uint32_t nprocs;

static int pmi_set_string(const char *key, void *data, size_t size)
{
    int rc;
    pmix_value_t value;

    PMIX_VALUE_CONSTRUCT(&value);
    value.type = PMIX_BYTE_OBJECT;
    value.data.bo.bytes = data;
    value.data.bo.size = size;
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, key, &value))) {
        fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Put failed: %s\n", myproc.nspace, myproc.rank,
            PMIx_Error_string(rc));
    }

    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Commit failed: %s\n", myproc.nspace, myproc.rank,
            PMIx_Error_string(rc));
    }

    /* protect the data */
    value.data.bo.bytes = NULL;
    value.data.bo.size = 0;
    PMIX_VALUE_DESTRUCT(&value);
    fprintf(stdout, "%s:%d PMIx_Put on %s with value %s\n", myproc.nspace, myproc.rank, key, (char*) data);

    return 0;
}

static int pmi_get_string(uint32_t peer_rank, const char *key,
                          void **data_out, size_t *data_size_out, bool retry)
{
    int rc;
    pmix_proc_t proc;
    pmix_value_t *pvalue;
    pmix_info_t info;

    PMIX_LOAD_PROCID(&proc, myproc.nspace, peer_rank);
    if (refresh && retry) {
        PMIX_INFO_LOAD(&info, PMIX_GET_REFRESH_CACHE, &refresh, PMIX_BOOL);
        rc = PMIx_Get(&proc, key, &info, 1, &pvalue);
        PMIX_INFO_DESTRUCT(&info);
    } else if (timeout) {
        rc = 2;
        PMIX_INFO_LOAD(&info, PMIX_TIMEOUT, &rc, PMIX_INT);
        rc = PMIx_Get(&proc, key, &info, 1, &pvalue);
        PMIX_INFO_DESTRUCT(&info);
    } else {
        rc = PMIx_Get(&proc, key, NULL, 0, &pvalue);
    }
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Get on rank %u %s: %s\n", myproc.nspace, myproc.rank,
            peer_rank, key, PMIx_Error_string(rc));
        return rc;
    }
    if (pvalue->type != PMIX_BYTE_OBJECT) {
        fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Get %s: got wrong data type\n", myproc.nspace, myproc.rank,
            key);
    }
    *data_out = pvalue->data.bo.bytes;
    *data_size_out = pvalue->data.bo.size;

    /* protect the data */
    pvalue->data.bo.bytes = NULL;
    pvalue->data.bo.size = 0;
    PMIX_VALUE_RELEASE(pvalue);
    PMIX_PROC_DESTRUCT(&proc);

    fprintf(stdout, "%s:%d PMIx_get %s returned %zi bytes (value %s)\n", myproc.nspace, myproc.rank, key,
            data_size_out[0], (char*) *data_out);

    return 0;
}

static int pmix_exchange(bool flag)
{
    int rc;
    pmix_info_t info;

    fprintf(stderr, "Execute fence\n");
    PMIX_INFO_CONSTRUCT(&info);
    PMIX_INFO_LOAD(&info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
    if (all_procs) {
        /* Array with procs (group case)*/
        rc = PMIx_Fence(all_procs, nprocs, &info, 1);
    } else {
        /* Rank wildcard */
        rc = PMIx_Fence(&allproc, 1, &info, 1);
    }

    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Fence_nb failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exit(1);
    }
    PMIX_INFO_DESTRUCT(&info);

    return 0;
}

static int pmix_create_group(void) {

    pmix_status_t rc = PMIX_SUCCESS;
    bool assign_context_id = true;
    pmix_info_t * directives = NULL;
    pmix_info_t *results = NULL;
    size_t ndirs = 2;
    size_t nresults = 0;
    int group_timeout = 10;

    PMIX_PROC_CREATE(all_procs, nprocs);
    for (size_t i = 0; i < nprocs; i++) {
        PMIX_PROC_LOAD(&(all_procs[i]), myproc.nspace, i);
    }

    fprintf(stdout, "Client ns %s rank %d: Group construction...\n", myproc.nspace,
            myproc.rank);

    PMIX_INFO_CREATE(directives, ndirs);
    PMIX_INFO_LOAD(&(directives[0]), PMIX_GROUP_ASSIGN_CONTEXT_ID, &assign_context_id, PMIX_BOOL);
    PMIX_INFO_LOAD(&(directives[1]), PMIX_TIMEOUT, &group_timeout, PMIX_INT);
    if (PMIX_SUCCESS != (rc = PMIx_Group_construct("mygroup", all_procs, nprocs, directives, ndirs,
                                     &results, &nresults))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Group_construct failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        return rc;
    }

    for (size_t k = 0; k < nresults; k++) {
        fprintf(stdout, "%s-%d: %s\n", myproc.nspace, myproc.rank, PMIx_Info_string(&results[k]));
    }

    if (directives != NULL) {
        PMIX_INFO_FREE(directives, ndirs);
    }
    if (results != NULL) {
        PMIX_INFO_FREE(results, nresults);
    }
    return rc;
}

static int pmix_destruct_group(void) {
    pmix_status_t rc = PMIX_SUCCESS;
    size_t ndirs = 1;
    pmix_info_t * directives = NULL;
    PMIX_INFO_CREATE(directives, ndirs);
    PMIX_INFO_LOAD(&(directives[0]), PMIX_TIMEOUT, &timeout, PMIX_INT);
    if (PMIX_SUCCESS != (rc = PMIx_Group_destruct("mygroup", directives, ndirs))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Group_destruct failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
    }
    if (directives != NULL) {
        PMIX_INFO_FREE(directives, ndirs);
    }
    return rc;
}

int main(int argc, char *argv[])
{
    char data[256];
    char *data_out;
    size_t size_out;
    int rc;
    pmix_value_t *pvalue;

    /* check the args */
    if (1 < argc) {
        if (2 < argc || 0 == strncmp(argv[1], "-h", 2) || 0 == strncmp(argv[1], "--h", 3)) {
            fprintf(stderr, "Usage:\n");
            fprintf(stderr, "\t--wait       Test PMIX_GET_WAIT_FOR_KEY\n");
            fprintf(stderr, "\t--refresh    Test PMIX_GET_REFRESH_CACHE\n");
            fprintf(stderr, "\t--timeout    Test PMIX_GET_WAIT_FOR_KEY, but timeout\n");
            fprintf(stderr, "\t--group      Test PMIX_GET_REFRESH_CACHE and PMIx Process Group\n");
            exit(0);
        }
        if (0 == strncmp(argv[1], "--w", 3)) {
            mywait = true;
        } else if (0 == strncmp(argv[1], "--r", 3)) {
            refresh = true;
        } else if (0 == strncmp(argv[1], "--t", 3)) {
            timeout = true;
        } else if (0 == strncmp(argv[1], "--g", 3)) {
            group = true;
            refresh = true;
        } else {
            fprintf(stderr, "Invalid cmd line option: %s\n", argv[1]);
            fprintf(stderr, "Usage:\n");
            fprintf(stderr, "\t--wait       Test PMIX_GET_WAIT_FOR_KEY\n");
            fprintf(stderr, "\t--refresh    Test PMIX_GET_REFRESH_CACHE\n");
            fprintf(stderr, "\t--timeout    Test PMIX_GET_WAIT_FOR_KEY, but timeout\n");
            fprintf(stderr, "\t--group      Test PMIX_GET_REFRESH_CACHE and PMIx Process Group\n");
            exit(1);
        }
    }

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "ERROR: PMIx_Init failed");
        exit(1);
    }
    if (myproc.rank == 0) {
        printf("PMIx initialized\n");
    }
    memset(data, 0, 256);

    /* job-related info is found in our nspace, assigned to the
     * wildcard rank as it doesn't relate to a specific rank. Setup
     * a name to retrieve such values */
    PMIX_LOAD_PROCID(&allproc, myproc.nspace, PMIX_RANK_WILDCARD);

    /* get the number of procs in our job */
    if (PMIX_SUCCESS != (rc = PMIx_Get(&allproc, PMIX_JOB_SIZE, NULL, 0, &pvalue))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exit(1);
    }
    nprocs = pvalue->data.uint32;
    PMIX_VALUE_RELEASE(pvalue);


    /* Each rank puts a unique key-value pair in the KVS */
    sprintf(data, "ORIG rank %d", myproc.rank);
    if (0 == myproc.rank) {
        pmi_set_string("test-key-0", data, 256);
    } else {
        pmi_set_string("test-key-1", data, 256);
    }

    pmix_exchange(true);

    if (1 == myproc.rank) {
        if (timeout) {
            sleep(10);
        } else if (mywait) {
            sleep(2);
        }
    }

    /* Each rank gets the key-value pair that was put by the other rank */
    if (0 == myproc.rank) {
        rc = pmi_get_string(1, "test-key-1", (void **) &data_out, &size_out, false);
        if (0 != rc) {
            goto done;
        }
        fprintf(stdout, "%d: obtained data for test-key-1 \"%s\"\n", myproc.rank, data_out);
    } else {
        rc = pmi_get_string(0, "test-key-0", (void **) &data_out, &size_out, false);
        if (0 != rc) {
            goto done;
        }
        fprintf(stdout, "%d: obtained data for test-key-0 \"%s\"\n", myproc.rank, data_out);
    }

    /* Create a group */
    if (group) {
        rc = pmix_create_group();
        if (rc != PMIX_SUCCESS) {
            exit(1);
        }
    }

    /* Each rank attempts to overwrite the value that was previously put */
    sprintf(data, "OVERWRITE rank %d", myproc.rank);
    if (0 == myproc.rank) {
        pmi_set_string("test-key-0", data, 256);
    } else {
        pmi_set_string("test-key-1", data, 256);
    }

    pmix_exchange(true);

    if (1 == myproc.rank) {
        if (timeout) {
            sleep(10);
        } else if (mywait) {
            sleep(2);
        }
    }

    /* Each rank gets the value put by the other rank
     * If overwrite was successful, each rank should get the new value here */
    if (0 == myproc.rank) {
        rc = pmi_get_string(1, "test-key-1", (void **) &data_out, &size_out, true);
        if (0 != rc) {
            goto done;
        }
        fprintf(stdout, "%d: obtained data for test-key-1 \"%s\"\n", myproc.rank, data_out);
    } else {
        rc = pmi_get_string(0, "test-key-0", (void **) &data_out, &size_out, true);
        if (0 != rc) {
            goto done;
        }
        fprintf(stdout, "%d: obtained data for test-key-0 \"%s\"\n", myproc.rank, data_out);
    }

    /* Destroy group*/
    if (group) {
        rc = pmix_destruct_group();
        if (rc != PMIX_SUCCESS) {
            exit(1);
        }
    }

done:
    if (all_procs != NULL) {
        PMIX_PROC_FREE(all_procs, nprocs);
    }

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "ERROR: Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    }
    if (myproc.rank == 0)
        printf("PMIx finalized\n");

    exit(0);
}
