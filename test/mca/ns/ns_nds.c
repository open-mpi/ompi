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

#include "orte_config.h"
#include "../src/include/orte_constants.h"
#include "../src/include/orte_types.h"
#include "../src/include/orte_names.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "include/constants.h"

#include "support.h"
#include "../src/runtime/runtime.h"
#include "../src/mca/base/base.h"
#include "../src/plsnds/plsnds.h"
#include "../src/mca/ns/ns.h"
#include "../src/mca/ns/base/base.h"

static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify int16 */
static bool test3(void);        /* verify int32 */
static bool test4(void);        /* verify string */
static bool test5(void);        /* verify name */
static bool test6(void);        /* verify BOOL */
static bool test7(void);        /* verify OBJECT */
static bool test8(void);        /* verify composite (multiple types and element counts) */
static bool test9(void);        /* verify GPR_KEYVAL */
static bool test10(void);        /* verify GPR_VALUE */
static bool test11(void);        /* verify APP_INFO */
static bool test12(void);        /* verify APP_CONTEXT */

FILE *test_out;

int main (int argc, char* argv[])
{

    test_init("orte_plsnds");
    test_out = stderr;
    
    /* open up the mca so we can get parameters */
    ompi_init(argc, argv);

    /* startup the MCA */
    if (OMPI_SUCCESS != mca_base_open()) {
        fprintf(stderr, "can't open mca\n");
        exit (1);
    }
    
    /* startup the name services */
    if (OMPI_SUCCESS != orte_ns_base_open()) {
        fprintf(stderr, "can't open name services\n");
        exit (1);
    }
    
    /* setup the plsnds */
    orte_plsnds_open();
    
    fprintf(test_out, "executing test1\n");
    if (test1()) {
        test_success();
    }
    else {
      test_failure("orte_dps test1 failed");
    }

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        test_success();
    }
    else {
      test_failure("orte_dps test2 failed");
    }
    
    fprintf(test_out, "executing test3\n");
    if (test3()) {
        test_success();
    }
    else {
      test_failure("orte_dps test3 failed");
    }

    fprintf(test_out, "executing test4\n");
    if (test4()) {
        test_success();
    }
    else {
      test_failure("orte_dps test4 failed");
    }

    fprintf(test_out, "executing test5\n");
    if (test5()) {
        test_success();
    }
    else {
      test_failure("orte_dps test5 failed");
    }

    fprintf(test_out, "executing test6\n");
    if (test6()) {
        test_success();
    }
    else {
      test_failure("orte_dps test6 failed");
    }

    fprintf(test_out, "executing test7\n");
    if (test7()) {
        test_success();
    }
    else {
      test_failure("orte_dps test7 failed");
    }

    fprintf(test_out, "executing test8\n");
    if (test8()) {
        test_success();
    }
    else {
      test_failure("orte_dps test8 failed");
    }

    fprintf(test_out, "executing test9\n");
    if (test9()) {
        test_success();
    }
    else {
      test_failure("orte_dps test9 failed");
    }

    fprintf(test_out, "executing test10\n");
    if (test10()) {
        test_success();
    }
    else {
      test_failure("orte_dps test10 failed");
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        test_success();
    }
    else {
      test_failure("orte_dps test11 failed");
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        test_success();
    }
    else {
      test_failure("orte_dps test12 failed");
    }

    test_finalize();
    fclose(test_out);
    return (0);
}


static bool test1(void)        /* check seed/singleton name discovery */
{
    orte_process_name_t seed={0,0,0};
    int rc;
    
    orte_process_info.seed = true;
    
    if (ORTE_SUCCESS != (rc = orte_ns.set_my_name())) {
        test_comment("set_my_name failed for seed/singleton case");
        fprintf(test_out, "set_my_name failed for seed/singleton case with return %s\n",
                    ORTE_ERROR_NAME(rc));
        return false;
    }
    
    if (NULL == orte_process_info.my_name) {
        test_comment("name_discovery failed for seed/singleton case - NULL name");
        fprintf(test_out, "name_discovery failed for seed/singleton case - NULL name\n");
        return false;
    }
    
    if (0 != orte_ns.compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &seed)) {
        test_comment("name_discovery failed for seed/singleton case - name mismatch");
        fprintf(test_out, "name_discovery failed for seed/singleton case - name mismatch\n");
        return false;
    }
    
    return (true);
}

/*
 * check environment name discovery
 */
static bool test2(void) 
{
    int rc;
    orte_process_name_t dummy={2,5,21456};
    
    if (NULL != orte_process_info.my_name) {  /* cleanup from prior test */
        free(orte_process_info.my_name);
        orte_process_info.my_name = NULL;
    }
    
    orte_process_info.seed = false;
    orte_ns.copy_process_name(&orte_process_info.ns_replica, &dummy);
    setenv("OMPI_MCA_CELLID", "2", 1);
    setenv("OMPI_MCA_JOBID", "5", 1);
    setenv("OMPI_MCA_PROCID", "21456", 1);
    setenv("OMPI_MCA_VPID_START", "0", 1);
    setenv("OMPI_MCA_NUM_PROCS", "100000", 1);
    
    if (ORTE_SUCCESS != (rc = orte_ns.set_my_name())) {
        test_comment("set_my_name failed for env case");
        fprintf(test_out, "set_my_name failed for env case with return %s\n",
                    ORTE_ERROR_NAME(rc));
        return false;
    }
    
    if (NULL == orte_process_info.my_name) {
        test_comment("name_discovery failed for env case - NULL name");
        fprintf(test_out, "name_discovery failed for env case - NULL name\n");
        return false;
    }
    
    if (0 != orte_ns.compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &dummy)) {
        test_comment("name_discovery failed for env case - name mismatch");
        fprintf(test_out, "name_discovery failed for env case - name mismatch\n");
        return false;
    }

    if (0 != orte_process_info.vpid_start) {
        test_comment("name_discovery failed for env case - wrong vpid_start");
        fprintf(test_out, "name_discovery failed for env case - wrong vpid_start\n");
        return false;
    }

    if (100000 != orte_process_info.num_procs) {
        test_comment("name_discovery failed for env case - wrong num_procs");
        fprintf(test_out, "name_discovery failed for env case - wrong num_procs\n");
        return false;
    }

    return (true);
}

/*
 * check RSH
 */
static bool test3(void)
{
    int rc;
    orte_process_name_t dummy={10,0,160000};
    
    if (NULL != orte_process_info.my_name) {  /* cleanup from prior test */
        free(orte_process_info.my_name);
        orte_process_info.my_name = NULL;
    }
    
    orte_process_info.seed = false;
    orte_ns.copy_process_name(&orte_process_info.ns_replica, &dummy);
    setenv("OMPI_MCA_CELLID", "10", 1);
    setenv("OMPI_MCA_JOBID", "0", 1);
    setenv("OMPI_MCA_PROCID", "160000", 1);
    setenv("OMPI_MCA_VPID_START", "15", 1);
    setenv("OMPI_MCA_NUM_PROCS", "250000", 1);
    
    setenv("OMPI_MCA_PLS_LAUNCHER", "RSH", 1);
    
    if (ORTE_SUCCESS != (rc = orte_ns.set_my_name())) {
        test_comment("set_my_name failed for env case");
        fprintf(test_out, "set_my_name failed for env case with return %s\n",
                    ORTE_ERROR_NAME(rc));
        return false;
    }
    
    if (NULL == orte_process_info.my_name) {
        test_comment("name_discovery failed for env case - NULL name");
        fprintf(test_out, "name_discovery failed for env case - NULL name\n");
        return false;
    }
    
    if (0 != orte_ns.compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &dummy)) {
        test_comment("name_discovery failed for env case - name mismatch");
        fprintf(test_out, "name_discovery failed for env case - name mismatch\n");
        return false;
    }

    if (15 != orte_process_info.vpid_start) {
        test_comment("name_discovery failed for env case - wrong vpid_start");
        fprintf(test_out, "name_discovery failed for env case - wrong vpid_start\n");
        return false;
    }

    if (250000 != orte_process_info.num_procs) {
        test_comment("name_discovery failed for env case - wrong num_procs");
        fprintf(test_out, "name_discovery failed for env case - wrong num_procs\n");
        return false;
    }

    return (true);
}

/*
 * 
 */

static bool test4(void)
{
    return (true);
}


/**
 *  
 */

static bool test5(void)
{
    return(true);
}

/**
 * 
 */

static bool test6(void)
{
    return (true);
}

/**
 * 
 */

static bool test7(void) 
{
    return (true);
}

/**
 * 
 */

static bool test8(void) 
{
    return (true);
}

/*  */
static bool test9(void)
{
    return (true);
}


/*  */
static bool test10(void)
{
    return (true);
}


/*  */
static bool test11(void)
{
    return (true);
}


/*  */
static bool test12(void)
{
    return (true);
}
