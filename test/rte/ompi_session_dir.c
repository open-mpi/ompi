/*
 * $HEADER$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <libgen.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "lam_config.h"
#include "include/constants.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "rte/universe/os_session_dir.h"

static bool test1(void);   /* given prefix, both one that works and one that fails */
static bool test2(void);   /* no prefix given, LAM_PREFIX_ENV set, one good and one bad */
static bool test3(void);   /* no prefix given, TMPDIR set, one good and one bad */
static bool test4(void);   /* no prefix given, TMP set, one good and one bad */
static bool test5(void);   /* no prefix given, HOME set, one good and one bad */
static bool test6(void);   /* no prefix given, nothing set, one good and one bad */


int main(int argc, char* argv[])
{
    bool test1f, test2f, test3f, test4f, test5f, test6f;

    test1f = test2f = test3f = test4f = test5f = test6f = false;
    ompi_sys_info(); /* initialize system */
 
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

    if (test6()) {
        printf("test6 passed\n");
        test6f = true;
    }

    if (test1f && test2f && test3f && test4f && test5f && test6f) {
        printf("test succeeded\n");
        return 0;
    }

    printf("test failed\n");
    return -1;
}


static bool test1(void)
{
    /* Test proper action when given a prefix */

    char *prefix, *tmp, *tmp2;

    /* see if we can create a specified path */

    prefix = ompi_os_path(false, "tmp", NULL);
    if (LAM_ERROR == ompi_session_dir_init(prefix, "test-universe")) {
        free(prefix);
        return(false);
    }
    /* see if it can access an existing path */

    if (LAM_ERROR == ompi_session_dir_init(prefix, "test-universe")) {
        free(prefix);
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);
    free(ompi_system_info.session_dir);
    free(prefix);
    free(tmp);

    /* check what happens when given prefix that won't allow access */
    tmp2 = ompi_os_path(false, "test", NULL); /* assume we don't have root privileges */
    if (LAM_SUCCESS == ompi_session_dir_init(tmp2, "test-universe")) {
        printf("created temp directory in %s - shouldn't have been able to do so\n", tmp2);
	rmdir(ompi_system_info.session_dir);
	tmp = strdup(dirname(ompi_system_info.session_dir));
	rmdir(tmp);
	free(tmp);
        free(tmp2);
        return(false);
    }

    return true;
}


static bool test2(void)
{
    char *tmp;

    /* use the LAM_PREFIX_ENV variable */

    setenv("LAM_PREFIX_ENV", "/tmp/trythis", 1);

    if (LAM_ERROR == ompi_session_dir_init(NULL, "test-universe")) {
	unsetenv("LAM_PREFIX_ENV");
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("LAM_PREFIX_ENV");

    return(true);

}


static bool test3(void)
{
    /* use the TMPDIR enviro variable */
    char *tmp;

    setenv("TMPDIR", "/tmp/trythis", 1);

    if (LAM_ERROR == ompi_session_dir_init(NULL, "test-universe")) {
	unsetenv("TMPDIR");
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("TMPDIR");

    return(true);
}


static bool test4(void)
{
    /* use the TMP enviro variable */
    char *tmp;

    setenv("TMP", "/tmp/trythis", 1);

    if (LAM_ERROR == ompi_session_dir_init(NULL, "test-universe")) {
	unsetenv("TMP");
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("TMP");

    return(true);
}


static bool test5(void)
{
    /* use the HOME enviro variable */
    char *tmp;

    setenv("HOME", "/tmp/trythis", 1);

    if (LAM_ERROR == ompi_session_dir_init(NULL, "test-universe")) {
	unsetenv("HOME");
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("HOME");

    return(true);
}


static bool test6(void)
{
    char *tmp;

    /* no enviro variables set, no prefix given 
    * Program should turn to default of /tmp (where "/" is whatever
    * top-level directory is appropriate for given system)
    */
    if (LAM_ERROR == ompi_session_dir_init(NULL, "test-universe")) {
        return(false);
    }

    rmdir(ompi_system_info.session_dir);
    tmp = strdup(dirname(ompi_system_info.session_dir));
    rmdir(tmp);

    return(true);
}
