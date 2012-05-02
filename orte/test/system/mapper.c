/* -*- C -*-
 */

#include <stdio.h>

#include "orte/constants.h"

#include "opal/util/argv.h"

#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#define LINE_LENGTH 10000

int main(int argc, char* argv[])
{
    char text[LINE_LENGTH];
    char **invals=NULL;
    int i, j;

    if (1 < argc) {
        if (ORTE_SUCCESS != orte_init(&argc, &argv, ORTE_PROC_NON_MPI)) {
            fprintf(stderr, "Failed orte_init\n");
            exit(1);
        }
    }

    memset(text, 0, sizeof(text));
    while (fgets(text, sizeof(text), stdin)) {
        /* remove trailing newline */
        if ('\n' == text[strlen(text)-1]) {
            text[strlen(text)-1] = '\0';
        }
        /* break the line on white space */
        for (i=0, j=0; i < LINE_LENGTH && '\0' != text[i]; i++) {
            if (isspace(text[i])) {
                if (j < i) {
                    text[i] = '\0';
                    opal_argv_append_nosize(&invals, &text[j]);
                }
                j = i+1;
            }
        }
        if (i < LINE_LENGTH && j < i+1) {
            opal_argv_append_nosize(&invals, &text[j]);
        }
        if (NULL == invals) {
            fprintf(stderr, "stdin complete (text strlen: %lu)\n", strlen(text));
            break;
        }
        for (i=0; NULL != invals[i]; i++) {
            fprintf(stdout, "%s\t1\n", invals[i]);
        }
        if (NULL != invals) {
            opal_argv_free(invals);
            invals = NULL;
        }
        memset(text, 0, sizeof(text));
    }

    if (1 < argc) {
        fprintf(stderr, "%s: FINALIZING\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        fflush(stderr);

        if (ORTE_SUCCESS != orte_finalize()) {
            fprintf(stderr, "Failed orte_finalize\n");
            exit(1);
        }
    }

    return 0;
}
