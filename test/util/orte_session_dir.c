/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif  /* HAVE_SYS_PARAM_H */
#include <sys/stat.h>

#include "support.h"
#include "orte/constants.h"
#include "orte/util/proc_info.h"
#include "opal/util/os_path.h"
#include "orte/util/session_dir.h"
#include "orte/util/proc_info.h"


static bool test1(void);   /* given prefix, both one that works and one that fails */
static bool test2(void);   /* no prefix given, ORTE_PREFIX_ENV set, one good and one bad */
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
    orte_proc_info(); /* initialize proc info structure */
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->cellid = 0;
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;
    
    test_init("orte_session_dir_t");
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
      test_failure("orte_session_dir_t test1 failed");
    }

    fprintf(test_out, "running test2\n");
    if (test2()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test2 failed");
    }

    fprintf(test_out, "running test3\n");
    if (test3()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test3 failed");
    }

    fprintf(test_out, "running test4\n");
    if (test4()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test4 failed");
    }

    fprintf(test_out, "running test5\n");
    if (test5()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test5 failed");
    }

    fprintf(test_out, "running test6\n");
    if (test6()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test6 failed");
    }

    fprintf(test_out, "running test7\n");
    if (test7()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test7 failed");
    }

    fprintf(test_out, "running test8\n");
    if (test8()) {
        test_success();
    }
    else {
      test_failure("orte_session_dir_t test8 failed");
    }

    fprintf(test_out, "completed all tests\n");

    fclose(test_out);
    
    /* clean up */
    orte_proc_info_finalize();
    
    test_finalize();
    return 0;
}


static bool test1(void)
{
    /* Test proper action when given a prefix */

    char *prefix;

    /* see if we can create a specified path */

    clear_proc_info();

    prefix = opal_os_path(false, "tmp", NULL);
    if (ORTE_SUCCESS != orte_session_dir(true, prefix, NULL, NULL, "test-universe", NULL, NULL)) {
	fprintf(test_out, "test1 - couldn't create specified path\n");
        free(prefix);
        return(false);
    }
    /* see if it can access an existing path */

    if (ORTE_SUCCESS != orte_session_dir(false, prefix, NULL, NULL, "test-universe", NULL, NULL)) {
	fprintf(test_out, "test1 - couldn't access existing path\n");
        free(prefix);
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);
    free(orte_process_info.universe_session_dir);
    free(prefix);

    return true;
}


static bool test2(void)
{
    clear_proc_info();

    /* use the ORTE_PREFIX_ENV variable */

    setenv("OMPI_PREFIX_ENV", "/tmp/trythis", 1);

    if (ORTE_SUCCESS != orte_session_dir(true, NULL, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("OMPI_PREFIX_ENV");
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);

    unsetenv("OMPI_PREFIX_ENV");

    return(true);

}


static bool test3(void)
{
    /* use the TMPDIR enviro variable */
    clear_proc_info();

    setenv("TMPDIR", "/tmp/trythis", 1);

    if (ORTE_SUCCESS != orte_session_dir(true, NULL, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("TMPDIR");
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);

    unsetenv("TMPDIR");

    return(true);
}


static bool test4(void)
{
    /* use the TMP enviro variable */

    clear_proc_info();

    setenv("TMP", "/tmp/trythis", 1);

    if (ORTE_SUCCESS != orte_session_dir(true, NULL, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("TMP");
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);

    unsetenv("TMP");

    return(true);
}


static bool test5(void)
{
    /* use the HOME enviro variable */

    clear_proc_info();

    setenv("HOME", "/tmp/trythis", 1);

    if (ORTE_SUCCESS != orte_session_dir(true, NULL, NULL, NULL, "test-universe", NULL, NULL)) {
	unsetenv("HOME");
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);

    unsetenv("HOME");

    return(true);
}


static bool test6(void)
{

    clear_proc_info();

    /* no enviro variables set, no prefix given 
    * Program should turn to default of /tmp (where "/" is whatever
    * top-level directory is appropriate for given system)
    */
    if (ORTE_SUCCESS != orte_session_dir(true, NULL, NULL, NULL, "test-universe", NULL, NULL)) {
        return(false);
    }

    orte_session_dir_finalize(orte_process_info.my_name);

    return(true);
}

static bool test7(void)
{
    char *filenm[5];
    FILE *fp;
    int i;

    clear_proc_info();

    /* create test proc session directory tree */
    if (ORTE_SUCCESS != orte_session_dir(true, NULL, "localhost", NULL, "test-universe", "test-job", "test-proc")) {
	return(false);
    }

    fprintf(test_out, "removing directories: %s\n\t%s\n\t%s\n",
	    orte_process_info.proc_session_dir,
	    orte_process_info.job_session_dir,
	    orte_process_info.universe_session_dir);

    /* create some files */

    filenm[0] = opal_os_path(false, orte_process_info.proc_session_dir, "dum1", NULL);
    fp = fopen(filenm[0], "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm[1] = opal_os_path(false, orte_process_info.job_session_dir, "dum2", NULL);
    fp = fopen(filenm[1], "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm[2] = opal_os_path(false, orte_process_info.universe_session_dir, "dum3", NULL);
    fp = fopen(filenm[2], "w");
    fprintf(fp, "ss");
    fclose(fp);

    if (ORTE_SUCCESS != orte_session_dir_finalize(orte_process_info.my_name)) {
	return(false);
    }

    for (i=0; i < 3; i++) unlink(filenm[i]);
    orte_session_dir_finalize(orte_process_info.my_name);

    return true;
}

static bool test8(void)
{
    char *filenm[5];
    FILE *fp;
    int i;

    clear_proc_info();

    /* create test proc session directory tree */
    if (ORTE_SUCCESS != orte_session_dir(true, NULL, "localhost", NULL, "test-universe2", "test-job2", "test-proc2")) {
	return(false);
    }

    fprintf(test_out, "removing directories: %s\n\t%s\n\t%s\n",
	    orte_process_info.proc_session_dir,
	    orte_process_info.job_session_dir,
	    orte_process_info.universe_session_dir);

    /* create some files */

    filenm[0] = opal_os_path(false, orte_process_info.proc_session_dir, "dum1", NULL);
    fp = fopen(filenm[0], "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm[1] = opal_os_path(false, orte_process_info.job_session_dir, "dum2", NULL);
    fp = fopen(filenm[1], "w");
    fprintf(fp, "ss");
    fclose(fp);

    filenm[2] = opal_os_path(false, orte_process_info.universe_session_dir, "dum3", NULL);
    fp = fopen(filenm[2], "w");
    fprintf(fp, "ss");
    fclose(fp);


    if (ORTE_SUCCESS != orte_session_dir_finalize(orte_process_info.my_name)) {
	   return(false);
    }

    for (i=0; i < 3; i++) unlink(filenm[i]);
    orte_session_dir_finalize(orte_process_info.my_name);
    
    return true;
}

void clear_proc_info(void)
{
    orte_process_info.tmpdir_base = NULL;
    orte_process_info.top_session_dir = NULL;
    orte_process_info.universe_session_dir = NULL;
    orte_process_info.job_session_dir = NULL;
    orte_process_info.proc_session_dir = NULL;

}
