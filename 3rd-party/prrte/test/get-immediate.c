#include <pmix.h>
#include <sched.h>
#include <stdio.h>

static pmix_proc_t allproc = {};
static pmix_proc_t myproc = {};
static bool immediate = false;
static bool shouldfind = false;

int pmi_set_string(const char *key, void *data, size_t size)
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
    printf("%s:%d PMIx_Put on %s\n", myproc.nspace, myproc.rank, key);

    return 0;
}

int pmi_get_string(uint32_t peer_rank, const char *key, void **data_out, size_t *data_size_out)
{
    int rc;
    pmix_proc_t proc;
    pmix_value_t *pvalue;
    pmix_info_t info;

    PMIX_LOAD_PROCID(&proc, myproc.nspace, peer_rank);
    if (immediate) {
        PMIX_INFO_LOAD(&info, PMIX_IMMEDIATE, NULL, PMIX_BOOL);
    } else {
        PMIX_INFO_LOAD(&info, PMIX_OPTIONAL, NULL, PMIX_BOOL);
    }
    rc = PMIx_Get(&proc, key, &info, 1, &pvalue);
    PMIX_INFO_DESTRUCT(&info);
    if (immediate) {
        /* both ranks should find data */
        shouldfind = true;
    } else {
        /* if it is optional, then neither rank will find it */
        shouldfind = false;
    }

    if (shouldfind) {
        if (PMIX_SUCCESS != rc) {
            fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Get on rank %u did not find %s: %s\n", myproc.nspace, myproc.rank,
                peer_rank, key, PMIx_Error_string(rc));
            return rc;
        }
        if (pvalue->type != PMIX_BYTE_OBJECT) {
            fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Get %s: got wrong data type\n", myproc.nspace, myproc.rank,
                key);
            return rc;
        }
        *data_out = pvalue->data.bo.bytes;
        *data_size_out = pvalue->data.bo.size;

        /* protect the data */
        pvalue->data.bo.bytes = NULL;
        pvalue->data.bo.size = 0;
        PMIX_VALUE_RELEASE(pvalue);
        PMIX_PROC_DESTRUCT(&proc);

        printf("%s:%d PMIx_get %s returned %zi bytes\n", myproc.nspace, myproc.rank, key,
               data_size_out[0]);
    } else {
        if (PMIX_SUCCESS == rc) {
            fprintf(stderr, "ERROR: Client ns %s rank %d: PMIx_Get on rank %u found %s: %s\n", myproc.nspace, myproc.rank,
                    peer_rank, key, PMIx_Error_string(rc));
        }
        *data_out = strdup("NOT-FOUND");
        *data_size_out = strlen(*data_out);
    }

    return 0;
}

int main(int argc, char *argv[])
{
    char data[256] = {};
    char *data_out;
    size_t size_out;
    int rc;
    pmix_value_t *pvalue;

    /* check the args */
    if (1 < argc) {
        if (2 < argc || 0 == strncmp(argv[1], "-h", 2) || 0 == strncmp(argv[1], "--h", 3)) {
            fprintf(stderr, "Usage:\n");
            fprintf(stderr, "\t--immediate       Test PMIX_IMMEDIATE\n");
            exit(0);
        }
        if (0 == strncmp(argv[1], "--i", 3)) {
            immediate = true;
        } else {
            fprintf(stderr, "Invalid cmd line option: %s\n", argv[1]);
            fprintf(stderr, "Usage:\n");
            fprintf(stderr, "\t--immediate       Test PMIX_IMMEDIATE\n");
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
    uint32_t nprocs = pvalue->data.uint32;
    PMIX_VALUE_RELEASE(pvalue);

    /* each proc pushes some data */
    if (immediate || 0 == myproc.rank) {
        if (0 == myproc.rank) {
            pmi_set_string("test-key-1", data, 256);
        } else {
            pmi_set_string("test-key-2", data, 256);
        }
    }
    sleep(2);

    if (0 == myproc.rank) {
        rc = pmi_get_string(1, "test-key-2", (void **) &data_out, &size_out);
    } else {
        rc = pmi_get_string(0, "test-key-1", (void **) &data_out, &size_out);
    }
    if (0 == rc) {
        printf("%d: obtained data \"%s\"\n", myproc.rank, data_out);
    }

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "ERROR: Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace, myproc.rank, rc);
    }
    if (myproc.rank == 0)
        printf("PMIx finalized\n");

    exit(0);
}
