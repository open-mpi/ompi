/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>

#include "support.h"
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
    ompi_sys_info(); /* initialize system */
 
    /* Perform overall test initialization */
    test_init("ompi_bitmap_t");

    if (test1()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test1 failed");
    }

    if (test2()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test2 failed");
    }

    if (test3()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test3 failed");
    }

    if (test4()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test4 failed");
    }

    if (test5()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test5 failed");
    }

    if (test6()) {
      test_success();
    }
    else {
      test_failure("ompi_session_dir_t test6 failed");
    }

    test_finalize();
    return 0;
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
