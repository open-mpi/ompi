#include <pmix.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    int pause = 0;
    if (argc > 1) {
        pause = atoi(argv[1]);
    }

    pmix_proc_t proc;
    pmix_status_t rc = PMIX_ERROR;

    rc = PMIx_Init(&proc, NULL, 0);
    if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "PMIx_Init failed: %s\n", PMIx_Error_string(rc));
        return EXIT_FAILURE;
    }

    printf("Hello\n");

    sleep(pause);

    printf("Bye\n");

    rc = PMIx_Finalize(NULL, 0);
    if (rc != PMIX_SUCCESS) {
        fprintf(stderr, "PMIx_Finalize failed: %s\n", PMIx_Error_string(rc));
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
