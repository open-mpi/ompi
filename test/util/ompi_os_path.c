/*
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/param.h>

#include "ompi_config.h"
#include "util/sys_info.h"
#include "util/os_path.h"

static bool test1(void);   /* trivial answer test */
static bool test2(void);   /* relative path test */
static bool test3(void);   /* absolute path test */
static bool test4(void);   /* missing path separator test */
static bool test5(void);   /* very long path name test */


int main(int argc, char* argv[])
{
    bool test1f, test2f, test3f, test4f, test5f;

    test1f = test2f = test3f = test4f = test5f = false;

    /* All done */

    if (test1()) {
        printf("test1 passed\n");
        test1f = true;
    }

    if (test2()) {
        printf("test2 passed\n");
        test2f = true;
    }

    if (test3()) {
        printf("test3 passed\n");
        test3f = true;
    }

    if (test4()) {
        printf("test4 passed\n");
        test4f = true;
    }

    if (test5()) {
        printf("test5 passed\n");
        test5f = true;
    }
 
    if (test1f && test2f && test3f && test4f && test5f) {
        printf("test succeeded\n");
        return 0;
    }

    printf("test failed\n");
    return -1;
}


static bool test1(void)
{
    char *out;

    /* Test trivial functionality. Program should return "." when called in relative
     * mode, and the separator character when called in absolute mode. */

    if (NULL != (out = ompi_os_path(true,NULL))) {
        if (0 != strcmp(".", out))
            return(false);
    }
    if (NULL != (out = ompi_os_path(false,NULL))) {
        if (0 != strcmp(ompi_system_info.path_sep, out))
            return(false);
    }

    return true;
}


static bool test2(void)
{
    char *out;
    char *a[] = { "aaa", "bbb", "ccc", NULL };
 
    if (NULL == ompi_system_info.path_sep) {
        printf("test2 cannot be run\n");
        return(false);
    }

    /* Construct a relative path name and see if it comes back correctly. Check multiple depths. */
    out = strdup(".");
    out = strcat(out, ompi_system_info.path_sep);
    out = strcat(out, a[0]);
    if (0 != strcmp(out, ompi_os_path(true, a[0], NULL)))
        return(false);

    out = strcat(out, ompi_system_info.path_sep);
    out = strcat(out, a[1]);
    if (0 != strcmp(out, ompi_os_path(true, a[0], a[1], NULL)))
        return(false);

    out = strcat(out, ompi_system_info.path_sep);
    out = strcat(out, a[2]);
    if (0 != strcmp(out, ompi_os_path(true, a[0], a[1], a[2], NULL)))
        return(false);

    return true;
}


static bool test3(void)
{
    char *out;
    char *a[] = { "aaa", "bbb", "ccc", NULL };

    if (NULL == ompi_system_info.path_sep) {
        printf("test3 cannot be run\n");
        return(false);
    }

    /* Same as prior test, only with absolute path name */
    out = strdup(ompi_system_info.path_sep);
    out = strcat(out, a[0]);
    if (0 != strcmp(out, ompi_os_path(false, a[0], NULL)))
        return(false);

    out = strcat(out, ompi_system_info.path_sep);
    out = strcat(out, a[1]);
    if (0 != strcmp(out, ompi_os_path(false, a[0], a[1], NULL)))
        return(false);

    out = strcat(out, ompi_system_info.path_sep);
    out = strcat(out, a[2]);
    if (0 != strcmp(out, ompi_os_path(false, a[0], a[1], a[2], NULL)))
        return(false);

    return true;
}

static bool test4(void)
{
    char a[MAXPATHLEN + 10];
    int i;

    if (NULL == ompi_system_info.path_sep) {
        printf("test4 cannot be run\n");
        return(false);
    }

    for (i=0; i< MAXPATHLEN+5; i++) {
        a[i] = 'a';
    }
    if (NULL != ompi_os_path(false, a, NULL)) {
        return(false);
    }
    return (true);
}

static bool test5(void)
{
    /* test to ensure the program doesn't bomb when no separator is found.
     * Program should try to find one, then return NULL if it can't */

    if (NULL != ompi_system_info.path_sep) {
        free(ompi_system_info.path_sep);
        ompi_system_info.path_sep = NULL;
    }
    return(test1());
}


