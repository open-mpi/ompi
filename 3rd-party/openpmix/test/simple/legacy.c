#include "src/include/pmix_config.h"

#include <assert.h>
#include <stdio.h>
#include <pmix.h>
#include <string.h>

#include "src/include/pmix_globals.h"

int
main(int argc, char *argv[])
{
pmix_value_t *val = NULL;
pmix_status_t rc = PMIX_SUCCESS;
pmix_proc_t global_proc, proc;
PMIX_HIDE_UNUSED_PARAMS(argc, argv);

rc = PMIx_Init(&global_proc, NULL, 0);
assert(PMIX_SUCCESS == rc);

rc = PMIx_Get(&global_proc, PMIX_LOCAL_RANK, NULL, 0, &val);
assert(PMIX_SUCCESS == rc);

int local_rank = val->data.uint16;
PMIX_VALUE_RELEASE(val);

char key_p[32];
pmix_value_t value_p;

sprintf(key_p, "%s-%d", "foo", local_rank);
value_p.type = PMIX_UINT32;
value_p.data.uint32 = local_rank + 10;

rc = PMIx_Put(PMIX_GLOBAL, key_p, &value_p);
assert(PMIX_SUCCESS == rc);

if (1 == global_proc.rank) {
    sleep(1);
}
rc = PMIx_Commit();
assert(PMIX_SUCCESS == rc);

PMIX_PROC_CONSTRUCT(&proc);
proc.rank = PMIX_RANK_UNDEF;
strcpy(proc.nspace, global_proc.nspace);

for (int i = 0; i < 2; i++) {
    char key_g[32];
    pmix_value_t *value_g;

    sprintf(key_g, "%s-%d", "foo", i);
    rc = PMIx_Get(&proc, key_g, NULL, 0, &value_g);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Rank %u: PMIx_Get got %d\n", global_proc.rank, rc);
    } else {
    fprintf(stderr, "Rank %u: Got %d\n", global_proc.rank, value_g->data.uint32);
    }
}

PMIx_Finalize(NULL, 0);

return 0;

}
