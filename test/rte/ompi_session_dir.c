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

#include "ompi_config.h"
#include "include/constants.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "util/session_dir.h"

static bool test1(void);   /* given prefix, both one that works and one that fails */
static bool test2(void);   /* no prefix given, OMPI_PREFIX_ENV set, one good and one bad */
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
    if (OMPI_ERROR == ompi_session_dir(true, prefix, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
        free(prefix);
        return(false);
    }
    /* see if it can access an existing path */

    if (OMPI_ERROR == ompi_session_dir(false, prefix, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
        free(prefix);
        return(false);
    }

    /* Currently, to remove the session, we need to call
     * ompi_session_dir_finalize.  
     *
     * NOTE: this will have to change at a future revision, since the
     *       plan is to leave the session dir around.  We will have to
     *       call a TBD function that will remove the session.
     */
    ompi_session_dir_finalize ();
    free(prefix);
    free(tmp);

    /* check what happens when given prefix that won't allow access */
    tmp2 = ompi_os_path(false, "test", NULL); /* assume we don't have root privileges */
    if (OMPI_SUCCESS == ompi_session_dir(true, tmp2, "test-universe", 
					 ompi_system_info.user, NULL, NULL, NULL, NULL)) {
        printf("created temp directory in %s - shouldn't have been able to do so\n", tmp2);
	ompi_session_dir_finalize();
	free(tmp);
        free(tmp2);
        return(false);
    }

    return true;
}


static bool test2(void)
{
    char *tmp;

    /* use the OMPI_PREFIX_ENV variable */

    setenv("OMPI_PREFIX_ENV", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
	unsetenv("OMPI_PREFIX_ENV");
        return(false);
    }

    ompi_session_dir_finalize ();
    free(tmp);

    unsetenv("OMPI_PREFIX_ENV");

    return(true);

}


static bool test3(void)
{
    /* use the TMPDIR enviro variable */
    char *tmp;

    setenv("TMPDIR", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
	unsetenv("TMPDIR");
        return(false);
    }

    ompi_session_dir_finalize ();
    free(tmp);

    unsetenv("TMPDIR");

    return(true);
}


static bool test4(void)
{
    /* use the TMP enviro variable */
    char *tmp;

    setenv("TMP", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
	unsetenv("TMP");
        return(false);
    }

    ompi_session_dir_finalize ();
    free(tmp);

    unsetenv("TMP");

    return(true);
}


static bool test5(void)
{
    /* use the HOME enviro variable */
    char *tmp;

    setenv("HOME", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, "test-universe", ompi_system_info.user, NULL, NULL, NULL, NULL)) {
	unsetenv("HOME");
        return(false);
    }

    ompi_session_dir_finalize ();
    free(tmp);

    unsetenv("HOME");

    return(true);
}


static bool test6(void)
{

    /* no enviro variables set, no prefix given 
    * Program should turn to default of /tmp (where "/" is whatever
    * top-level directory is appropriate for given system)
    */
    if (OMPI_ERROR == ompi_session_dir(true, NULL, "test-universe", 
				       ompi_system_info.user, NULL, NULL, NULL, NULL)) {
        return(false);
    }

    ompi_session_dir_finalize();
    return(true);
}
