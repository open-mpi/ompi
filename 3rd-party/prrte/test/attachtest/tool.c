#include <pmix_tool.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    pmix_proc_t proc;
    pmix_status_t rc = PMIX_ERROR;

    rc = PMIx_tool_init(&proc, NULL, 0);
    if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "PMIx_tool_init failed: %s\n", PMIx_Error_string(rc));
        return EXIT_FAILURE;
    }

    rc = PMIx_tool_finalize();
    if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "PMIx_tool_finalize failed: %s\n", PMIx_Error_string(rc));
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
