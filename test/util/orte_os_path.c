/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "util/sys_info.h"
#include "util/os_path.h"
#include "support.h"

static bool test1(void);   /* trivial answer test */
static bool test2(void);   /* relative path test */
static bool test3(void);   /* absolute path test */
static bool test4(void);   /* missing path separator test */
static bool test5(void);   /* very long path name test */


int main(int argc, char* argv[])
{

    test_init("orte_os_path_t");
    /* setup the system info structure */
    orte_sys_info();
    
    if (test1()) {
        test_success();
    }
    else {
      test_failure("orte_os_path_t test1 failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("orte_os_path_t test2 failed");
    }

    if (test3()) {
        test_success();
    }
    else {
      test_failure("orte_os_path_t test3 failed");
    }

    if (test4()) {
        test_success();
    }
    else {
      test_failure("orte_os_path_t test4 failed");
    }

    if (test5()) {
        test_success();
    }
    else {
      test_failure("orte_os_path_t test5 failed");
    }

    test_finalize();
    return 0;
}


static bool test1(void)
{
    char *out, *answer;

    /* Test trivial functionality. Program should return ".[separator]" when called in relative
     * mode, and the separator character when called in absolute mode. */
    if (NULL != (out = orte_os_path(true,NULL))) {
        answer = strdup(".");
        strcat(answer, orte_system_info.path_sep);
        if (0 != strcmp(answer, out))
            return(false);
    }
    if (NULL != (out = orte_os_path(false,NULL))) {
        if (0 != strcmp(orte_system_info.path_sep, out))
            return(false);
    }

    return true;
}


static bool test2(void)
{
    char *out;
    char *a[] = { "aaa", "bbb", "ccc", NULL };
 
    if (NULL == orte_system_info.path_sep) {
        printf("test2 cannot be run\n");
        return(false);
    }

    /* Construct a relative path name and see if it comes back correctly. Check multiple depths. */
    out = strdup(".");
    out = strcat(out, orte_system_info.path_sep);
    out = strcat(out, a[0]);
    if (0 != strcmp(out, orte_os_path(true, a[0], NULL)))
        return(false);

    out = strcat(out, orte_system_info.path_sep);
    out = strcat(out, a[1]);
    if (0 != strcmp(out, orte_os_path(true, a[0], a[1], NULL)))
        return(false);

    out = strcat(out, orte_system_info.path_sep);
    out = strcat(out, a[2]);
    if (0 != strcmp(out, orte_os_path(true, a[0], a[1], a[2], NULL)))
        return(false);

    return true;
}


static bool test3(void)
{
    char *out;
    char *a[] = { "aaa", "bbb", "ccc", NULL };

    if (NULL == orte_system_info.path_sep) {
        printf("test3 cannot be run\n");
        return(false);
    }

    /* Same as prior test, only with absolute path name */
    out = strdup(orte_system_info.path_sep);
    out = strcat(out, a[0]);
    if (0 != strcmp(out, orte_os_path(false, a[0], NULL)))
        return(false);

    out = strcat(out, orte_system_info.path_sep);
    out = strcat(out, a[1]);
    if (0 != strcmp(out, orte_os_path(false, a[0], a[1], NULL)))
        return(false);

    out = strcat(out, orte_system_info.path_sep);
    out = strcat(out, a[2]);
    if (0 != strcmp(out, orte_os_path(false, a[0], a[1], a[2], NULL)))
        return(false);

    return true;
}

static bool test4(void)
{
    char a[MAXPATHLEN + 10];
    int i;

    if (NULL == orte_system_info.path_sep) {
        printf("test4 cannot be run\n");
        return(false);
    }

    for (i=0; i< MAXPATHLEN+5; i++) {
        a[i] = 'a';
    }
    if (NULL != orte_os_path(false, a, NULL)) {
        return(false);
    }
    return (true);
}

static bool test5(void)
{
    /* test to ensure the program doesn't bomb when no separator is found.
     * Program should try to find one, then return NULL if it can't */

    if (NULL != orte_system_info.path_sep) {
        free(orte_system_info.path_sep);
        orte_system_info.path_sep = NULL;
    }
    return(test1());
}
