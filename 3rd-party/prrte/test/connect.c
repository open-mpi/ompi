/* -*- C -*-
 *
 * $HEADER$
 *
 * Test of connect
 */

#include <stdio.h>
#include "pmix.h"

int main(int argc, char* argv[])
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_proc_t wildcard;
    pmix_proc_t remote;
    pmix_value_t value;
    pmix_value_t *returnval;
    pmix_info_t info;

    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to init\n");
        exit(1);
    }

    printf("Hello from rank %u\n", myproc.rank);

    /* put some remote values */
    PMIX_VALUE_CONSTRUCT(&value);
    value.type = PMIX_STRING;
    value.data.string="12345";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, "spastic-global", &value))) {
        fprintf(stderr, "%u: Global put failed\n", myproc.rank);
        exit(1);
    }
    value.data.string = "67890";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_REMOTE, "spastic-remote", &value))) {
        fprintf(stderr, "%u: Remote put failed\n", myproc.rank);
        exit(1);
    }
    value.data.string = "abcdef";
    if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_LOCAL, "spastic-local", &value))) {
        fprintf(stderr, "%u: Local put failed\n", myproc.rank);
        exit(1);
    }

    /* commit them */
    rc = PMIx_Commit();
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "%u: Failed to commit\n", myproc.rank);
        exit(1);
    }

    printf("%u: Connecting\n", myproc.rank);
    PMIX_LOAD_PROCID(&wildcard, myproc.nspace, PMIX_RANK_WILDCARD);
    rc = PMIx_Connect(&wildcard, 1, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to connect\n");
        exit(1);
    }
    printf("%u: Connect succeeded!\n", myproc.rank);
    if (0 != myproc.rank && 1 != myproc.rank) {
        goto done;
    }

    /* try to get a remote value */
    PMIX_LOAD_NSPACE(remote.nspace, myproc.nspace);
    if (0 == myproc.rank) {
        remote.rank = 1;
    } else {
        remote.rank = 0;
    }
    printf("%u: Attempt to get global value\n", myproc.rank);
    PMIX_INFO_LOAD(&info, PMIX_IMMEDIATE, NULL, PMIX_BOOL);
    rc = PMIx_Get(&remote, "spastic-global", &info, 1, &returnval);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "%u: Unable to retrieve global data from %u\n", myproc.rank, remote.rank);
        goto done;
    }
    printf("%u: Global value for rank %u obtained\n", myproc.rank, remote.rank);

    printf("%u: Attempt to get remote value\n", myproc.rank);
    rc = PMIx_Get(&remote, "spastic-remote", &info, 1, &returnval);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "%u: Unable to retrieve remote data from %u\n", myproc.rank, remote.rank);
        goto done;
    }
    printf("%u: Remote value for rank %u obtained\n", myproc.rank, remote.rank);

    remote.rank = 0;
    rc = PMIx_Get(&remote, PMIX_GROUP_CONTEXT_ID, &info, 1, &returnval);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "%u: Unable to retrieve context ID from %u\n", myproc.rank, remote.rank);
        goto done;
    }
    printf("%u: Context ID %lu obtained\n", myproc.rank, (unsigned long)returnval->data.uint32);

done:
    PMIx_Finalize(NULL, 0);
    return 0;
}
