/*
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

#include "include/constants.h"
#include "util/sys_info.h"
#include "util/os_path.h"
#include "util/os_create_dirpath.h"
#include "util/session_dir.h"
#include "util/proc_info.h"
#include "support.h"


static bool test1(void);   /* given prefix, both one that works and one that fails */
static bool test2(void);   /* no prefix given, OMPI_PREFIX_ENV set, one good and one bad */
static bool test3(void);   /* no prefix given, TMPDIR set, one good and one bad */
static bool test4(void);   /* no prefix given, TMP set, one good and one bad */
static bool test5(void);   /* no prefix given, HOME set, one good and one bad */
static bool test6(void);   /* no prefix given, nothing set, one good and one bad */
static bool test7(void);   /* remove session directory tree */
static bool test8(void);   /* attempt to remove tree when subdirs present */

void clear_proc_info(void);

static FILE *test_out=NULL;

int main(int argc, char* argv[])
{
    ompi_sys_info(); /* initialize system */

    test_init("ompi_session_dir_t");
    test_out = fopen( "test_session_dir_out", "w+" );
    if( test_out == NULL ) {
      test_failure("test_session_dir couldn't open test file failed");
      test_finalize();
      exit(1);
    }


    fprintf(test_out, "running test1\n");
    if (test1()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test1 failed");
    }

    fprintf(test_out, "running test2\n");
    if (test2()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test2 failed");
    }

    fprintf(test_out, "running test3\n");
    if (test3()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test3 failed");
    }

    fprintf(test_out, "running test4\n");
    if (test4()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test4 failed");
    }

    fprintf(test_out, "running test5\n");
    if (test5()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test5 failed");
    }

    fprintf(test_out, "running test6\n");
    if (test6()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test6 failed");
    }

    fprintf(test_out, "running test7\n");
    if (test7()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test7 failed");
    }

    fprintf(test_out, "running test8\n");
    if (test8()) {
        test_success();
    }
    else {
      test_failure("ompi_session_dir_t test8 failed");
    }

    fprintf(test_out, "completed all tests\n");

    fclose(test_out);
    test_finalize();
    return 0;
}


static bool test1(void)
{
    /* Test proper action when given a prefix */

    char *prefix, *tmp;

    /* see if we can create a specified path */

    clear_proc_info();

    prefix = ompi_os_path(false, "tmp", NULL);
    if (OMPI_ERROR == ompi_session_dir(true, prefix, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	fprintf(test_out, "test1 - couldn't create specified path\n");
        free(prefix);
        return(false);
    }
    /* see if it can access an existing path */

    if (OMPI_ERROR == ompi_session_dir(false, prefix, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	fprintf(test_out, "test1 - couldn't access existing path\n");
        free(prefix);
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);
    free(ompi_process_info.universe_session_dir);
    free(prefix);
    free(tmp);

    return true;
}


static bool test2(void)
{
    char *tmp;

    clear_proc_info();

    /* use the OMPI_PREFIX_ENV variable */

    setenv("OMPI_PREFIX_ENV", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("OMPI_PREFIX_ENV");
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("OMPI_PREFIX_ENV");

    return(true);

}


static bool test3(void)
{
    /* use the TMPDIR enviro variable */
    char *tmp;

    clear_proc_info();

    setenv("TMPDIR", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("TMPDIR");
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("TMPDIR");

    return(true);
}


static bool test4(void)
{
    /* use the TMP enviro variable */
    char *tmp;

    clear_proc_info();

    setenv("TMP", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("TMP");
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("TMP");

    return(true);
}


static bool test5(void)
{
    /* use the HOME enviro variable */
    char *tmp;

    clear_proc_info();

    setenv("HOME", "/tmp/trythis", 1);

    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("HOME");
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);
    free(tmp);

    unsetenv("HOME");

    return(true);
}


static bool test6(void)
{
    char *tmp;

    clear_proc_info();

    /* no enviro variables set, no prefix given 
    * Program should turn to default of /tmp (where "/" is whatever
    * top-level directory is appropriate for given system)
    */
    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, NULL, NULL, "test-universe", NULL, NULL)) {
        return(false);
    }

    rmdir(ompi_process_info.universe_session_dir);
    tmp = strdup(dirname(ompi_process_info.universe_session_dir));
    rmdir(tmp);

    return(true);
}

static bool test7(void)
{
    char *filenm;
    FILE *fp;

    clear_proc_info();

    /* create test proc session directory tree */
    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, "localhost", NULL, "test-universe", "test-job", "test-proc")) {
	return(false);
    }

    fprintf(test_out, "removing directories: %s\n\t%s\n\t%s\n",
	    ompi_process_info.proc_session_dir,
	    ompi_process_info.job_session_dir,
	    ompi_process_info.universe_session_dir);

    /* create some files */

    filenm = ompi_os_path(false, ompi_process_info.proc_session_dir, "dum1", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm = ompi_os_path(false, ompi_process_info.job_session_dir, "dum2", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm = ompi_os_path(false, ompi_process_info.universe_session_dir, "dum3", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);

    if (OMPI_ERROR == ompi_session_dir_finalize()) {
	return(false);
    }

    return true;
}

static bool test8(void)
{
    char *filenm;
    FILE *fp;

    clear_proc_info();

    /* create test proc session directory tree */
    if (OMPI_ERROR == ompi_session_dir(true, NULL, ompi_system_info.user, "localhost", NULL, "test-universe2", "test-job", "test-proc")) {
	return(false);
    }

    fprintf(test_out, "removing directories: %s\n\t%s\n\t%s\n",
	    ompi_process_info.proc_session_dir,
	    ompi_process_info.job_session_dir,
	    ompi_process_info.universe_session_dir);

    /* create some files */

    filenm = ompi_os_path(false, ompi_process_info.proc_session_dir, "dum1", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm = ompi_os_path(false, ompi_process_info.job_session_dir, "dum2", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm = ompi_os_path(false, ompi_process_info.universe_session_dir, "dum3", NULL);
    fp = fopen(filenm, "w");
    fprintf(fp, "ss");
    fclose(fp);


    filenm = ompi_os_path( false, ompi_process_info.job_session_dir, "dir1", NULL);
    ompi_os_create_dirpath(filenm, 0777);

    if (OMPI_ERROR == ompi_session_dir_finalize()) {
	return(false);
    }

    return true;
}

void clear_proc_info(void)
{
    ompi_process_info.tmpdir_base = NULL;
    ompi_process_info.top_session_dir = NULL;
    ompi_process_info.universe_session_dir = NULL;
    ompi_process_info.job_session_dir = NULL;
    ompi_process_info.proc_session_dir = NULL;

}
