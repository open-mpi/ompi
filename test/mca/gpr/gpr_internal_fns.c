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
/** @file:
 *
 * The Open MPI general purpose registry - unit test
 *
 */

/*
 * includes
 */

#include "orte_config.h"
#include <stdio.h>
#include <string.h>

#include "include/orte_constants.h"
#include "include/orte_schema.h"

#include "support.h"

#include "class/orte_pointer_array.h"
#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

int main(int argc, char **argv)
{
    int rc, num_names, num_found;
    int32_t i, j, cnt;
    char *tmp=NULL, *tmp2=NULL, *names[15], *keys[5];
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t itag[10], itag2, *itaglist;
    orte_gpr_replica_container_t *cptr=NULL, **cptrs=NULL;
    orte_gpr_keyval_t *kptr=NULL, **kvals;
    orte_gpr_replica_itagval_t **ivals=NULL, *iptr;
    orte_gpr_value_t **values, *val;
    orte_process_name_t seed={0,0,0};
    bool found;
    
    test_init("test_gpr_replica");

   /*  test_out = fopen( "test_gpr_replica_out", "w+" ); */
    test_out = stderr;
    if( test_out == NULL ) {
      test_failure("gpr_test couldn't open test file failed");
      test_finalize();
      exit(1);
    } 

    /* ENSURE THE REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);
    
    /* Open up the output streams */
    if (!ompi_output_init()) {
        return OMPI_ERROR;
    }
                                                                                                                   
    /* 
     * If threads are supported - assume that we are using threads - and reset otherwise. 
     */
    ompi_set_using_threads(OMPI_HAVE_THREADS);
                                                                                                                   
    /* For malloc debugging */
    ompi_malloc_init();

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (rc = orte_sys_info())) {
        return rc;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (rc = orte_proc_info())) {
        return rc;
    }
    

    orte_process_info.seed = true;
    orte_process_info.my_name = &seed;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(test_out, "GPR started\n");
    } else {
        fprintf(test_out, "GPR could not start\n");
        exit (1);
    }
    
    if (ORTE_SUCCESS == orte_gpr_base_select()) {
        fprintf(test_out, "GPR replica selected\n");
    } else {
        fprintf(test_out, "GPR replica could not be selected\n");
        exit (1);
    }
                  
    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }
    
    fprintf(stderr, "going to find seg\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, "test-segment"))) {
        fprintf(test_out, "gpr_test: find_seg failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: find_seg failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: find_seg passed\n");
    }
    
    fprintf(stderr, "creating tags\n");
    for (i=0; i<10; i++) {
        asprintf(&tmp, "test-tag-%d", i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&itag[i], seg, tmp))) {
            fprintf(test_out, "gpr_test: create_itag failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: create_itag failed");
            test_finalize();
            return rc;
        } else {
            fprintf(test_out, "gpr_test: create_itag passed\n");
        }
        free(tmp);
    }
    
    fprintf(stderr, "lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%d", i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_lookup(&itag2, seg, tmp)) ||
             itag2 != itag[i]) {
            fprintf(test_out, "gpr_test: lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: lookup failed");
            test_finalize();
            return rc;
        } else {
            fprintf(test_out, "gpr_test: lookup passed\n");
        }
        free(tmp);
    }
    
    
    fprintf(stderr, "reverse lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp2, "test-tag-%d", i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_reverse_lookup(&tmp, seg, itag[i])) ||
             0 != strcmp(tmp2, tmp)) {
            fprintf(test_out, "gpr_test: reverse lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: reverse lookup failed");
            test_finalize();
            return rc;
        } else {
            fprintf(test_out, "gpr_test: reverse lookup passed\n");
        }
        free(tmp);
    }
    
    
    fprintf(stderr, "delete tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%d", i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_delete_itag(seg, tmp))) {
            fprintf(test_out, "gpr_test: delete tag failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: delete tag failed");
            test_finalize();
            return rc;
        } else {
            fprintf(test_out, "gpr_test: delete tag passed\n");
        }
        free(tmp);
    }
    
    fprintf(stderr, "get itag list\n");
    for (i=0; i < 14; i++) {
        asprintf(&names[i], "dummy%d", i);
    }
    names[14] = NULL;
    num_names = 0;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itaglist, seg,
                                                names, &num_names))) {
        fprintf(test_out, "gpr_test: get itag list failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: get itag list failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get itag list passed\n");
    }
    
    fprintf(test_out, "number of names found %d\n", num_names);
    for (i=0; i < num_names; i++) {
        fprintf(test_out, "\tname %s itag %d\n", names[i], itaglist[i]);
    }
    
    
    fprintf(stderr, "creating container\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_container(&cptr, seg,
                                3, itaglist))) {
        fprintf(test_out, "gpr_test: create_container failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: create_container failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: create_container passed\n");
    }
    
    fprintf(test_out, "itags for container\n");
    for (i=0; i < cptr->num_itags; i++) {
        fprintf(test_out, "\tindex %d itag %d\n", i, cptr->itags[i]);
    }

    fprintf(stderr, "add keyval\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_INT16;
    kptr->value.i16 = 21;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr, kptr))) {
        fprintf(test_out, "gpr_test: add keyval failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: add keyval failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: add keyval passed\n");
    }
    OBJ_RELEASE(kptr);
    
    ivals = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
    if (NULL != ivals[0]) {
        fprintf(stderr, "ival[0] %d %d %d\n", ivals[0]->itag,
                    ivals[0]->type, ivals[0]->value.i16);
    }
    
    fprintf(stderr, "search container for single entry\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_STRING;
    kptr->value.strptr = strdup("try-string-value");
    orte_gpr_replica_create_itag(&itag2, seg, kptr->key);
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_search_container(&num_found, ORTE_GPR_REPLICA_OR,
                                        &itag2, 1, cptr) ||
        0 >= num_found)) {
        fprintf(test_out, "gpr_test: search container for single entry failed - returned %s for itag %d\n",
                            ORTE_ERROR_NAME(rc), itag2);
        test_failure("gpr_test: search container for single entry failed");
        test_finalize();
        return -1;
    } else {
        fprintf(test_out, "gpr_test: search container for single entry passed\n");
    }
    OBJ_RELEASE(kptr);
    
    fprintf(stderr, "update single keyval\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_STRING;
    kptr->value.strptr = strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_keyval(seg, cptr, kptr))) {
        fprintf(test_out, "gpr_test: update single keyval failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update single keyval failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: update single keyval passed\n");
    }
    
    ivals = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
    for (i=0; i < (cptr->itagvals)->size; i++) {
        if (NULL != ivals[i]) {
            if (ivals[i]->type == ORTE_INT16) {
                fprintf(stderr, "ival[%d] %d %d %d\n", i, ivals[i]->itag,
                    ivals[i]->type, ivals[i]->value.i16);
            } else if (ivals[i]->type == ORTE_STRING) {
                fprintf(stderr, "ival[%d] %d %d %s\n", i, ivals[i]->itag,
                    ivals[i]->type, ivals[i]->value.strptr);
            }
        }
    }


    fprintf(stderr, "update multiple keyvals in a container\n");
    if(ORTE_SUCCESS != orte_gpr_replica_find_seg(&seg, false, "test-put-segment")) {
        return -1;
    }
    
    cptrs = (orte_gpr_replica_container_t**)((seg->containers)->addr);
    for (i=0; i < (seg->containers)->size; i++) {
        if (NULL != cptrs[i]) {
            cptr = cptrs[i];
        }
    }

    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("really-stupid-value");
    kptr->type = ORTE_INT32;
    kptr->value.i32 = 123456;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_keyval(seg, cptr, kptr))) {
        fprintf(test_out, "gpr_test: update multiple keyvals failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update multiple keyvals failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: update multiple keyvals passed\n");
    }
    
    orte_gpr.dump_all(0);
    
    fprintf(stderr, "check itag list\n");
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 0, NULL, 15, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial NULL case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial NULL case failed\n");
    }
    if (!orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 5, itaglist, 15, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial mismatched xand case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial mismatched xand case failed\n");
    }
    if (!orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_AND, 15, itaglist, 5, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial mismatched and case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial mismatched and case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 10, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial xand case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial xand case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_AND, 5, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial and case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial and case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_OR, 5, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial or case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XOR, 10, itaglist, 5, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial or case failed\n");
    }


    fprintf(stderr, "\nreleasing segment\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_release_segment(&seg)) ||
        NULL != seg) {
        fprintf(test_out, "gpr_test: release segment failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: release segment failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: release segment passed\n");
    }
    
    fclose( test_out );
/*    result = system( cmd_str );
    if( result == 0 ) {
        test_success();
    }
    else {
      test_failure( "test_gpr_replica failed");
    }
*/
    test_finalize();

    return(0);
}
