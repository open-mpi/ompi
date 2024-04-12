#include <pmix.h>
#include <stdbool.h>
#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <stdarg.h>

static void hide_unused_params(int x, ...)
{
    va_list ap;

    va_start(ap, x);
    va_end(ap);
}

int main(int argc, char **argv)
{
    int rc=0;
    pmix_value_t value;
    pmix_value_t *val = &value;
    pmix_value_t pvalue;
    pmix_proc_t myproc, rootproc;
    pmix_info_t info;
    hide_unused_params(rc, argc, argv);

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Init failed.\n");
        exit(1);
    }
    fprintf(stderr, "[%s:%u]: Running\n", myproc.nspace, myproc.rank);

    PMIX_VALUE_LOAD(&pvalue, "hello", PMIX_STRING);
    if (myproc.rank == 0) {
        if (PMIX_SUCCESS != (rc = PMIx_Put(PMIX_GLOBAL, "my.foo", &pvalue))) {
            fprintf(stderr, "Put of my.foo failed\n");
            exit(1);
        }
    }

    PMIx_Commit();

    bool tvalue = 1;
    strcpy(info.key, PMIX_COLLECT_DATA);
    PMIX_VALUE_LOAD(&(info.value), &tvalue, PMIX_BOOL);
    PMIx_Fence(NULL, 0, &info, 1);

    fprintf(stderr, "[%s:%u]: Fence complete\n", myproc.nspace, myproc.rank);

    rootproc = myproc;
    rootproc.rank = 0;

    if (PMIX_SUCCESS != (rc = PMIx_Get(&rootproc, "my.foo", NULL, 0, &val))) {
        fprintf(stderr, "Get of root's my.foo failed\n");
        exit(1);
    }
    fprintf(stderr, "[%s:%u]: Get complete and successful\n", myproc.nspace, myproc.rank);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d:PMIx_Finalize failed: %d\n", myproc.nspace,
                myproc.rank, rc);
        exit(1);
    }
    return (rc);
}
