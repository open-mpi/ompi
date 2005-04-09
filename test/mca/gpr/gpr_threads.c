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
#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "include/orte_schema.h"

#include <stdio.h>
#include <string.h>

#include "support.h"

#include "runtime/runtime.h"
#include "threads/thread.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/malloc.h"
#include "mca/errmgr/errmgr.h"
#include "mca/ns/ns_types.h"
#include "mca/gpr/gpr.h"
#include "dps/dps.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

/* function for exercising the registry */
void *thread_fn(ompi_object_t *object);

#if !OMPI_HAVE_THREAD_SUPPORT

/* If we don't have thread support, there's no point in running this
   test */

int main(int argc, char *argv[])
{
    printf("OMPI was compiled without thread support -- skipping this test\n");
    return 77;
}

#else

/* Only have the body of this test if we have thread support */


int main(int argc, char **argv)
{
    int i, rc, num_threads;
    ompi_thread_t **threads;
    union {
        int ivalue;
        void *vvalue;
    } value;

    /* protect against sizeof mismatches */
    if (sizeof(i) > sizeof (void*)) {
        fprintf(stderr, "cannot run this test on this machine\n");
        exit(77);
    }
    
    test_init("test_gpr_threads");

    if (getenv("TEST_WRITE_TO_FILE") != NULL) {
        test_out = fopen( "test_gpr_threads", "w+" );
    } else {
        test_out = stderr;
    }
    if( test_out == NULL ) {
      test_failure("gpr_threads couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    if (1 < argc) {  /* number of threads to run provided on cmd line */
        num_threads = strtol(argv[1], NULL, 0);
    } else {  /* default to 2 */
        num_threads = 2;
    }

    /* setup the runtime environment */
    if (ORTE_SUCCESS != (rc = orte_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the threads */
    threads = (ompi_thread_t**)malloc(num_threads * sizeof(ompi_thread_t*));
    if (NULL == threads) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < num_threads; i++) {
        threads[i] = OBJ_NEW(ompi_thread_t);
        if (NULL == threads[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        threads[i]->t_run = thread_fn;
        value.ivalue = i;
        threads[i]->t_arg = value.vvalue;
    }
    
    /* run the threads */
    for (i=0; i < num_threads; i++) {
        if (OMPI_SUCCESS != (rc = ompi_thread_start(threads[i]))) {
            ORTE_ERROR_LOG(rc);
            exit(rc);
        }
    }

    /* finalize things */
    orte_finalize();
            
    fclose( test_out );

    test_finalize();

    return(0);
}

void *thread_fn(ompi_object_t *obj)
{
    return NULL;
}

#endif /* OMPI_HAVE_THREAD_SUPPORT */
