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
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "support.h"
#include "components.h"

#include "include/constants.h"
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/malloc.h"
#include "opal/util/output.h"
#include "mca/base/base.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"

static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify int16 */

static FILE *test_out;
static mca_ns_base_module_t *ns_module = NULL;


int main (int argc, char* argv[])
{
    test_component_handle_t ns_handle;
    mca_ns_base_component_t *ns_component = NULL;
    int priority;

    test_init("orte_ns_nds");
    test_out = stderr;
    
    /* Open up the output streams */
    if (!opal_output_init()) {
        return OMPI_ERROR;
    }

    /* 
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    opal_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    ompi_malloc_init();

    /* set seed to true to force replica selection */
    orte_process_info.seed = true;

    /* ensure the replica is isolated */
    setenv("OMPI_MCA_ns_replica_isolate", "1", 1);

    /* init the proc info structure */
    orte_proc_info();
    
    /* startup the MCA */
    if (OMPI_SUCCESS != mca_base_open()) {
        fprintf(stderr, "can't open mca\n");
        exit (1);
    }
    
    /* Open the ns replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("ns", "replica", &ns_handle, 
                            (mca_base_component_t**) &ns_component) ||
        NULL == ns_component) {
        test_fail_stop("Could not open ns replica\n", 1);
    }
    ns_module = ns_component->ns_init(&priority);
    if (NULL == ns_module) {
        test_fail_stop("NS replica component did not return a module\n", 1);
    }

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

    /* finalize and see if memory cleared */
    if (NULL != ns_component->ns_finalize) {
        ns_component->ns_finalize();
    }
    test_component_close(&ns_handle);

    orte_proc_info_finalize();
    mca_base_close();
    ompi_malloc_finalize();
    opal_output_finalize();

    fclose( test_out );
    test_finalize();
    return (0);
}


static bool test1(void)        /* check seed/singleton name discovery */
{
    orte_process_name_t seed={0,0,0};
    int rc;
    
    orte_process_info.seed = true;
    
    if (ORTE_SUCCESS != (rc = ns_module->set_my_name())) {
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
    
    if (0 != ns_module->compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &seed)) {
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
    if (ORTE_SUCCESS != (rc = ns_module->copy_process_name(&orte_process_info.ns_replica, &dummy))) {
        test_comment("unable to copy name");
        fprintf(test_out, "unable to copy name with return %s\n",
                    ORTE_ERROR_NAME(rc));
        return false;
    }
    
    setenv("OMPI_MCA_ns_nds", "env", 1);
    setenv("OMPI_MCA_ns_nds_cellid", "2", 1);
    setenv("OMPI_MCA_ns_nds_jobid", "5", 1);
    setenv("OMPI_MCA_ns_nds_vpid", "21456", 1);
    setenv("OMPI_MCA_ns_nds_vpid_start", "0", 1);
    setenv("OMPI_MCA_ns_nds_num_procs", "100000", 1);
    
    if (ORTE_SUCCESS != (rc = ns_module->set_my_name())) {
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
    
    if (0 != ns_module->compare(ORTE_NS_CMP_ALL, orte_process_info.my_name, &dummy)) {
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
