
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include <pmix.h>

int main(int argc, char* argv[])
{
    pmix_proc_t myproc;
    char hostname[1024];
    pid_t pid;
    pmix_value_t *val = NULL;
    pmix_app_t apps[2];
    pmix_nspace_t nspace;
    pmix_status_t rc;

    pid = getpid();
    gethostname(hostname, 1024);

    PMIx_Init(&myproc, NULL, 0);

    rc = PMIx_Get(&myproc, PMIX_PARENT_ID, NULL, 0, &val);
    /* If we don't find it, then we're the parent */
    if (PMIX_SUCCESS != rc) {
        printf("Parent [pid %ld] about to spawn!\n", (long)pid);
        PMIX_APP_CONSTRUCT(&apps[0]);
        apps[0].cmd = strdup(argv[0]);
        PMIX_ARGV_APPEND(rc, apps[0].argv, "This is job 1");
        apps[0].maxprocs = 2;
        PMIX_APP_CONSTRUCT(&apps[1]);
        apps[1].cmd = strdup(argv[0]);
        PMIX_ARGV_APPEND(rc, apps[1].argv, "This is job 2");
        apps[1].maxprocs = 2;

        rc = PMIx_Spawn(NULL, 0, apps, 2, nspace);
        if (PMIX_SUCCESS != rc) {
            printf("Child failed to spawn\n");
            return rc;
        }
        printf("Parent done with spawn\n");
    }
    /* Otherwise, we're the child */
    else {
        printf("Hello from the child %s.%u on host %s pid %ld argv[1] = %s\n",
               myproc.nspace, myproc.rank, hostname, (long)pid, argv[1]);
    }

    PMIx_Finalize(NULL, 0);
    return 0;
}
