#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <pmix.h>

int main(int argc, char **argv)
{
    char *tmp = PMIx_Argv_join(argv, ' ');
    fprintf(stderr, "CMDLINE: %s\n", tmp);
    return 0;
}
