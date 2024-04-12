#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <getopt.h>

#include <pmix.h>

static bool verbose = false;

int main( int argc, char **argv )
{
    pmix_status_t rc;
    pmix_proc_t myproc;
    pmix_proc_t procs[2];
    pmix_value_t *returnval;
    pmix_info_t info;
    static struct option myoptions[] = {{"verbose", no_argument, NULL, 'v'},
                                        {0, 0, 0, 0}};
    int option_index;
    int opt;

    while ((opt = getopt_long(argc, argv, "v", myoptions, &option_index)) != -1) {
        switch (opt) {
            case 'v':
                verbose = true;
                break;
        }
    }

    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Failed to init\n");
        exit(1);
    }
    PMIX_INFO_LOAD(&info, PMIX_IMMEDIATE, NULL, PMIX_BOOL);
    rc = PMIx_Get(&myproc, PMIX_PARENT_ID, &info, 1, &returnval);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s.%u]: Unable to retrieve parent\n", myproc.nspace, myproc.rank);
        goto done;
    }
    PMIX_XFER_PROCID(&procs[0], returnval->data.proc);
    PMIX_VALUE_RELEASE(returnval);

    PMIX_XFER_PROCID(&procs[1], &myproc);
    rc = PMIx_Connect(procs, 2, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "[%s.%u]: Failed to connect\n", myproc.nspace, myproc.rank);
        exit(1);
    }

    PMIx_Disconnect(procs, 2, NULL, 0);

done:
    PMIx_Finalize(NULL, 0);
    if (verbose) {
        printf("[%s.%u]: exiting\n", myproc.nspace, myproc.rank);
    }
    return 0;
}
