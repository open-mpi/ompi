
#include "include/pmix.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <ctype.h>

static void hide_unused_params(int x, ...)
{
    va_list ap;

    va_start(ap, x);
    va_end(ap);
}

int main(int argc, char **argv)
{
    pmix_proc_t myproc;
    pmix_status_t rc=0;
    int rank;
    hide_unused_params(rc, argc, argv);

    rc = PMIx_Init(&myproc, NULL, 0);
    assert(PMIX_SUCCESS == rc);

    {
        pmix_value_t *value;
        rc = PMIx_Get(&myproc, PMIX_RANK, NULL, 0, &value);
        assert(PMIX_SUCCESS == rc);
        printf("%d\n", value->type);
        assert(value->type == PMIX_INT);
        rank = value->data.uint32;
        PMIX_VALUE_RELEASE(value);
    }

    if (rank == 0) {
        pmix_info_t *info;
        PMIX_INFO_CREATE(info, 1);
        snprintf(info[0].key, PMIX_MAX_KEYLEN, "magic-found");
        info[0].value.type = PMIX_STRING;
        info[0].value.data.string = "yes";
        rc = PMIx_Publish(info, 1);
        assert(PMIX_SUCCESS == rc);
    }

    printf("I am rank %d\n", rank);

    {
        bool flag;
        pmix_info_t *info;
        PMIX_INFO_CREATE(info, 1);
        flag = true;
        PMIX_INFO_LOAD(info, PMIX_COLLECT_DATA, &flag, PMIX_BOOL);
        rc = PMIx_Fence(&myproc, 1, info, 1);
        assert(PMIX_SUCCESS == rc);
        PMIX_INFO_FREE(info, 1);
    }

    if (rank == 1) {
        int i;
        pmix_pdata_t *pdata;
        PMIX_PDATA_CREATE(pdata, 2);
        snprintf(pdata[0].key, PMIX_MAX_KEYLEN, "magic-found");
        snprintf(pdata[1].key, PMIX_MAX_KEYLEN, "magic-not-found");
        rc = PMIx_Lookup(&pdata[0], 2, NULL, 0);
        assert((PMIX_SUCCESS == rc) || (PMIX_ERR_NOT_FOUND == rc));
        for (i = 0; i < 2; i++)
            if (pdata[i].value.type == PMIX_STRING)
                printf("Found[%d] %d %s\n", i, pdata[i].value.type, pdata[i].value.data.string);
            else
                printf("Found[%d] %d\n", i, pdata[i].value.type);
        PMIX_PDATA_FREE(pdata, 1);
    }

    rc = PMIx_Finalize(NULL, 0);
    assert(PMIX_SUCCESS == rc);
}
