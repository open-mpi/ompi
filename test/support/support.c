/*
 * $HEADER$
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "support.h"

/**
 * A testing support library to provide uniform reporting output
 */

static int lam_n_tests;
static int lam_n_success;
static int lam_n_failures;
static char *lam_description;

void test_init(char *a)
{
    /* local variables */
    int len;

    /* save the descriptive string */
    len=strlen(a);
    lam_description=(char *)malloc(len+1);
    assert(lam_description);

    strcpy(lam_description,a);

    /* initialize counters */
    lam_n_tests=0;
    lam_n_success=0;
    lam_n_failures=0;

    return;

}

