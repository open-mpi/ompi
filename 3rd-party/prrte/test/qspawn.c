#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>
#include <unistd.h>

#include <pmix.h>

int main(int argc, char *argv[])
{
    pmix_status_t rc;
    int size;
    pid_t pid;
    pmix_proc_t myproc;
    pmix_proc_t proc, parent;
    pmix_app_t app;
    pmix_proc_t peers[2];
    char hostname[1024];
    pmix_value_t *val = NULL;
    pmix_nspace_t nspace;

    pid = getpid();
    gethostname(hostname, 1024);

    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client: PMIx_Init failed: %s\n", PMIx_Error_string(rc));
        exit(1);
    }

    PMIX_LOAD_PROCID(&proc, myproc.nspace, PMIX_RANK_WILDCARD);
    rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Get job size failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_GET_NUMBER(rc, val, size, int);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: get size number failed: %s\n", myproc.nspace,
                myproc.rank, PMIx_Error_string(rc));
        goto done;
    }
    PMIX_VALUE_RELEASE(val);

    rc = PMIx_Get(&myproc, PMIX_PARENT_ID, NULL, 0, &val);
    /* If we don't find it, then we're the parent */
    if (PMIX_SUCCESS != rc) {
        if (0 == myproc.rank) {
            pid = getpid();
            PMIX_APP_CONSTRUCT(&app);
            app.cmd = strdup(argv[0]);
            PMIX_ARGV_APPEND(rc, app.argv, argv[0]);
            app.maxprocs = 3;
            rc = PMIx_Spawn(NULL, 0, &app, 1, nspace);
            if (PMIX_SUCCESS != rc) {
                printf("Child failed to spawn\n");
                return rc;
            }

            /* post a piece of information our children should get */

            PMIX_LOAD_PROCID(&peers[0], myproc.nspace, 0);
            PMIX_LOAD_PROCID(&peers[1], nspace, PMIX_RANK_WILDCARD);
            /* connect to the children */
            rc = PMIx_Connect(peers, 2, NULL, 0);
            if (PMIX_SUCCESS != rc) {
                printf("Connect to children failed!\n");
            }
            rc = PMIx_Disconnect(peers, 2, NULL, 0);
            if (PMIX_SUCCESS != rc) {
                printf("Disonnect from children failed!\n");
            }
        }
        PMIX_LOAD_PROCID(&peers[0], myproc.nspace, PMIX_RANK_WILDCARD);
        PMIx_Fence(&peers[0], 1, NULL, 0);
    }
        /* Otherwise, we're the child */
    else {
        PMIX_LOAD_PROCID(&peers[0], val->data.proc->nspace, 0);
        PMIX_LOAD_PROCID(&peers[1], myproc.nspace, PMIX_RANK_WILDCARD);
        PMIX_VALUE_RELEASE(val);
        /* connect to our parent */
        if (1 == myproc.rank) {
            sleep(1);
        }
        rc = PMIx_Connect(peers, 2, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            printf("%s.%u: Connect to parent failed!\n", myproc.nspace, myproc.rank);
        }

        if (2 == myproc.rank) {
            sleep(1);
        }
        rc = PMIx_Disconnect(peers, 2, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            printf("Disonnect from parent failed!\n");
        }
    }

done:
    PMIx_Finalize(NULL, 0);
    return 0;
}
