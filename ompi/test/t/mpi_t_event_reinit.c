/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Repeated MPI_T_init_thread / MPI_T_finalize must leave the event registry
 * stable: the same counts and the same name->index mapping (registration is
 * idempotent by name and one-shot).
 */

#include <mpi.h>
#include <stdio.h>
#include <string.h>

static int failures = 0;
static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

static int snapshot(int *num_events, int *num_sources, char *first_name, int first_name_cap)
{
    int provided, rc;
    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        return 0;
    }
    (void) MPI_T_event_get_num(num_events);
    (void) MPI_T_source_get_num(num_sources);
    first_name[0] = '\0';
    if (*num_events > 0) {
        int nl = first_name_cap, ne = 0;
        rc = MPI_T_event_get_info(0, first_name, &nl, NULL, NULL, NULL, &ne, NULL, NULL, NULL,
                                  NULL, NULL);
        (void) rc;
    }
    MPI_T_finalize();
    return 1;
}

int main(void)
{
    int n1 = 0, s1 = 0, n2 = 0, s2 = 0;
    char name1[256], name2[256];

    if (!snapshot(&n1, &s1, name1, sizeof(name1)) || !snapshot(&n2, &s2, name2, sizeof(name2))) {
        printf("FAIL: MPI_T_init_thread\nRESULT: FAIL\n");
        return 1;
    }

    expect("event count stable across init/finalize", n1 == n2);
    expect("source count stable across init/finalize", s1 == s2);
    expect("event index 0 maps to the same name", 0 == strcmp(name1, name2));

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
