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
#include "mca/schema/schema.h"
#include "mca/schema/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/soh/base/base.h"
#include "mca/rmgr/base/base.h"

#include "support.h"
#include "components.h"

#include "class/orte_pointer_array.h"
#include "dps/dps.h"
#include "runtime/runtime.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "util/malloc.h"
#include "util/output.h"

#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/communications/gpr_replica_comm.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

/* output files needed by the test */
static FILE *test_out=NULL;

static char *cmd_str="diff ./test_gpr_replica_out ./test_gpr_replica_out_std";

/*
 * Local functions
 */
static void find(const char *name, test_component_handle_t *handle, 
                 test_component_fn_t *ptr);


int main(int argc, char **argv)
{
    int rc;
    size_t num_names, num_found;
    size_t i;
    char *tmp=NULL, *tmp2=NULL, *names[15], *keys[5];
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t itag[10], itag2, *itaglist;
    orte_gpr_replica_container_t *cptr=NULL;
    orte_gpr_keyval_t *kptr=NULL;
    orte_gpr_replica_itagval_t **ivals=NULL, *iptr;
    bool allow, have;
    int priority;

    test_component_handle_t handle;
    mca_gpr_base_component_t *gpr_component = NULL;
    orte_gpr_base_module_t *gpr_module = NULL;
    orte_gpr_replica_find_seg_fn_t find_seg_fn;
    orte_gpr_replica_create_itag_fn_t create_itag_fn;
    orte_gpr_replica_dict_lookup_fn_t dict_lookup_fn;
    orte_gpr_replica_dict_reverse_lookup_fn_t dict_reverse_lookup_fn;
    orte_gpr_replica_delete_itag_fn_t delete_itag_fn;
    orte_gpr_replica_get_itag_list_fn_t get_itag_list_fn;
    orte_gpr_replica_create_container_fn_t create_container_fn;
    orte_gpr_replica_add_keyval_fn_t add_keyval_fn;
    orte_gpr_replica_search_container_fn_t search_container_fn;
    orte_gpr_replica_update_keyval_fn_t update_keyval_fn;
    orte_gpr_replica_check_itag_list_fn_t check_itag_list_fn;
    orte_gpr_replica_release_segment_fn_t release_segment_fn;
    
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
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    ompi_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

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
    
    /* initialize the pointer variables */
    for (i=0; i < 15; i++) names[i]=NULL;
    for (i=0; i < 5; i++) keys[i] = NULL;
    
    /* initialize the system variables */
    orte_process_info.seed = true;
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->cellid = 0;
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    /* startup the dps */
    if (OMPI_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* startup the name services */
    if (OMPI_SUCCESS == orte_ns_base_open()) {
        fprintf(test_out, "NS started\n");
    } else {
        fprintf(test_out, "NS could not start\n");
        exit (1);
    }

    /* startup the soh */
    if (OMPI_SUCCESS == orte_soh_base_open()) {
        fprintf(test_out, "SOH started\n");
    } else {
        fprintf(test_out, "SOH could not start\n");
        exit (1);
    }

    /* startup the rmgr */
    if (OMPI_SUCCESS == orte_rmgr_base_open()) {
        fprintf(test_out, "RMGR started\n");
    } else {
        fprintf(test_out, "RMGR could not start\n");
        exit (1);
    }

    /* startup the schema */
    if (OMPI_SUCCESS == orte_schema_base_open()) {
        fprintf(test_out, "SCHEMA started\n");
    } else {
        fprintf(test_out, "SCHEMA could not start\n");
        exit (1);
    }

    /* Open the gpr replica component and initialize a module */
    if (OMPI_SUCCESS != 
        test_component_open("gpr", "replica", &handle, 
                            (mca_base_component_t**) &gpr_component) ||
        NULL == gpr_component) {
        fprintf(test_out, "Could not open replica\n");
        exit(1);
    }
    gpr_module = gpr_component->gpr_init(&allow, &have, &priority);
    if (NULL == gpr_module) {
        fprintf(test_out, "replica component did not return a module\n");
        exit(1);
    }

    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* Find a bunch of function pointers */

    fprintf(stderr, "finding relevant function pointers in component\n");
    find("orte_gpr_replica_find_seg", &handle, 
         (test_component_fn_t*) &find_seg_fn);
    find("orte_gpr_replica_create_itag", &handle, 
         (test_component_fn_t*) &create_itag_fn);
    find("orte_gpr_replica_dict_lookup", &handle, 
         (test_component_fn_t*) &dict_lookup_fn);
    find("orte_gpr_replica_dict_reverse_lookup", &handle, 
         (test_component_fn_t*) &dict_reverse_lookup_fn);
    find("orte_gpr_replica_delete_itag", &handle, 
         (test_component_fn_t*) &delete_itag_fn);
    find("orte_gpr_replica_get_itag_list", &handle, 
         (test_component_fn_t*) &get_itag_list_fn);
    find("orte_gpr_replica_check_itag_list", &handle, 
         (test_component_fn_t*) &check_itag_list_fn);
    find("orte_gpr_replica_create_container", &handle, 
         (test_component_fn_t*) &create_container_fn);
    find("orte_gpr_replica_add_keyval", &handle, 
         (test_component_fn_t*) &add_keyval_fn);
    find("orte_gpr_replica_search_container", &handle, 
         (test_component_fn_t*) &search_container_fn);
    find("orte_gpr_replica_update_keyval", &handle, 
         (test_component_fn_t*) &update_keyval_fn);
    find("orte_gpr_replica_release_segment", &handle, 
         (test_component_fn_t*) &release_segment_fn);

    /* Now do the tests */
    
    fprintf(stderr, "going to find seg\n");
    if (ORTE_SUCCESS != (rc = find_seg_fn(&seg, true, "test-segment"))) {
        fprintf(test_out, "gpr_test: find_seg failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: find_seg failed");
        test_finalize();
        return rc;
    } else {
        test_success();
        fprintf(test_out, "gpr_test: find_seg passed\n");
    }

    gpr_module->dump_all(0);

    fprintf(stderr, "creating tags\n");
    for (i=0; i<10; i++) {
        asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = create_itag_fn(&itag[i], seg, tmp))) {
            fprintf(test_out, "gpr_test: create_itag failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: create_itag failed");
            test_finalize();
            return rc;
        } else {
            test_success();
            fprintf(test_out, "gpr_test: create_itag passed\n");
        }
        free(tmp);
    }
    
    fprintf(stderr, "lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = dict_lookup_fn(&itag2, seg, tmp)) ||
             itag2 != itag[i]) {
            fprintf(test_out, "gpr_test: lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: lookup failed");
            test_finalize();
            return rc;
        } else {
            test_success();
            fprintf(test_out, "gpr_test: lookup passed\n");
        }
        free(tmp);
    }
    
    fprintf(stderr, "reverse lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp2, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = dict_reverse_lookup_fn(&tmp, seg, itag[i])) ||
             0 != strcmp(tmp2, tmp)) {
            fprintf(test_out, "gpr_test: reverse lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: reverse lookup failed");
            test_finalize();
            return rc;
        } else {
            test_success();
            fprintf(test_out, "gpr_test: reverse lookup passed\n");
        }
        free(tmp);
        free(tmp2);
    }
    
    fprintf(stderr, "delete tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = delete_itag_fn(seg, tmp))) {
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
        asprintf(&names[i], "dummy%lu", (unsigned long) i);
    }
    names[14] = NULL;
    num_names = 0;
    if (ORTE_SUCCESS != (rc = get_itag_list_fn(&itaglist, seg,
                                                names, &num_names))) {
        fprintf(test_out, "gpr_test: get itag list failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: get itag list failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: get itag list passed\n");
    }
    
    fprintf(test_out, "number of names found %lu\n", 
            (unsigned long) num_names);
    for (i=0; i < num_names; i++) {
        fprintf(test_out, "\tname %s itag %lu\n", names[i], 
                (unsigned long) itaglist[i]);
    }
    
    fprintf(stderr, "creating container\n");
    if (ORTE_SUCCESS != (rc = create_container_fn(&cptr, seg,
                                3, itaglist))) {
        fprintf(test_out, "gpr_test: create_container failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: create_container failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: create_container passed\n");
    }

    gpr_module->dump_all(0);
    
    fprintf(test_out, "itags for container\n");
    for (i=0; i < cptr->num_itags; i++) {
        fprintf(test_out, "\tindex %lu itag %lu\n", (unsigned long) i, 
                (unsigned long) cptr->itags[i]);
    }

    fprintf(stderr, "add keyval\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_INT16;
    kptr->value.i16 = 21;
    if (ORTE_SUCCESS != (rc = add_keyval_fn(&iptr, seg, cptr, kptr))) {
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
        fprintf(stderr, "ival[0] %lu %d %d\n", (unsigned long) ivals[0]->itag,
                ivals[0]->type, (int) ivals[0]->value.i16);
    }
    
    fprintf(stderr, "search container for single entry\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_STRING;
    kptr->value.strptr = strdup("try-string-value");
    create_itag_fn(&itag2, seg, kptr->key);
    if (ORTE_SUCCESS != (rc = search_container_fn(&num_found, ORTE_GPR_REPLICA_OR,
                                        &itag2, 1, cptr) ||
        0 >= num_found)) {
        fprintf(test_out, "gpr_test: search container for single entry failed - returned %s for itag %lu\n",
                            ORTE_ERROR_NAME(rc), (unsigned long) itag2);
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
    if (ORTE_SUCCESS != (rc = update_keyval_fn(seg, cptr, kptr))) {
        fprintf(test_out, "gpr_test: update single keyval failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update single keyval failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: update single keyval passed\n");
    }
    OBJ_RELEASE(kptr);
    
    ivals = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
    for (i=0; i < (cptr->itagvals)->size; i++) {
        if (NULL != ivals[i]) {
            if (ivals[i]->type == ORTE_INT16) {
                fprintf(stderr, "ival[%lu] %lu %d %d\n", (unsigned long) i, 
                        (unsigned long) ivals[i]->itag,
                        ivals[i]->type, (int) ivals[i]->value.i16);
            } else if (ivals[i]->type == ORTE_STRING) {
                fprintf(stderr, "ival[%lu] %lu %d %s\n", (unsigned long) i,
                        (unsigned long) ivals[i]->itag,
                        ivals[i]->type, ivals[i]->value.strptr);
            }
        }
    }

    fprintf(stderr, "add multiple keyvals to a container\n");
    for (i=0; i < 10; i++) {
        kptr = OBJ_NEW(orte_gpr_keyval_t);
        kptr->key = strdup("stupid-value");
        kptr->type = ORTE_INT16;
        kptr->value.i16 = i * 100;
        if (ORTE_SUCCESS != (rc = add_keyval_fn(&iptr, seg, cptr, kptr))) {
            fprintf(test_out, "gpr_test: add keyval failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            test_failure("gpr_test: add keyval failed");
            test_finalize();
            return rc;
        } else {
            fprintf(test_out, "gpr_test: add keyval passed\n");
        }
        OBJ_RELEASE(kptr);
    }
    
    fprintf(stderr, "update multiple keyvals in a container\n");
    if(ORTE_SUCCESS != find_seg_fn(&seg, false, "test-segment")) {
        fprintf(test_out, "Failure in orte_gpr_replica_find_seg\n");
        return -1;
    }
    
    gpr_module->dump_all(0);

    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->type = ORTE_INT32;
    kptr->value.i32 = 123456;
    if (ORTE_SUCCESS != (rc = create_itag_fn(&itag2, seg, kptr->key))) {
        fprintf(test_out, "gpr_internal_fns: update multiple keyvals - failed to get itag with error %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update multiple keyvals failed");
        test_finalize();
        return rc;
    }
    if (ORTE_SUCCESS != (rc = search_container_fn(&num_found, ORTE_GPR_REPLICA_OR,
                                                &itag2, 1, cptr))) {
        fprintf(test_out, "gpr_internal_fns: update multiple keyvals - failed to find itag with error %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update multiple keyvals failed");
        test_finalize();
        return rc;
    }
    if (ORTE_SUCCESS != (rc = update_keyval_fn(seg, cptr, kptr))) {
        fprintf(test_out, "gpr_test: update multiple keyvals failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: update multiple keyvals failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: update multiple keyvals passed\n");
    }
    OBJ_RELEASE(kptr);
    
    gpr_module->dump_all(0);
    
    fprintf(stderr, "check itag list\n");
    if (check_itag_list_fn(ORTE_GPR_REPLICA_XAND, 0, NULL, 15, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial NULL case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial NULL case failed\n");
    }
    if (!check_itag_list_fn(ORTE_GPR_REPLICA_XAND, 5, itaglist, 15, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial mismatched xand case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial mismatched xand case failed\n");
    }
    if (!check_itag_list_fn(ORTE_GPR_REPLICA_AND, 15, itaglist, 5, itaglist)) {
        fprintf(test_out, "check_itag_list: trivial mismatched and case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: trivial mismatched and case failed\n");
    }
    if (check_itag_list_fn(ORTE_GPR_REPLICA_XAND, 10, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial xand case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial xand case failed\n");
    }
    if (check_itag_list_fn(ORTE_GPR_REPLICA_AND, 5, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial and case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial and case failed\n");
    }
    if (check_itag_list_fn(ORTE_GPR_REPLICA_OR, 5, itaglist, 10, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial or case failed\n");
    }
    if (check_itag_list_fn(ORTE_GPR_REPLICA_XOR, 10, itaglist, 5, itaglist)) {
        fprintf(test_out, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(test_out, "check_itag_list: non-trivial or case failed\n");
    }

    fprintf(stderr, "\nreleasing segment\n");
    if (ORTE_SUCCESS != (rc = release_segment_fn(&seg)) ||
        NULL != seg) {
        fprintf(test_out, "gpr_test: release segment failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        test_failure("gpr_test: release segment failed");
        test_finalize();
        return rc;
    } else {
        fprintf(test_out, "gpr_test: release segment passed\n");
    }

    fprintf(stderr, "now finalize and see if all memory cleared\n");
    for (i=0; i < 15; i++) {
        if (NULL != names[i]) free(names[i]);
    }
    for (i=0; i < 5; i++) {
        if (NULL != keys[i]) free(keys[i]);
    }
    free(itaglist);

    /* finalize the gpr component */
    if (NULL != gpr_component->gpr_finalize) {
        gpr_component->gpr_finalize();
    }
    test_component_close(&handle);
    orte_dps_close();

    orte_sys_info_finalize();
    orte_proc_info_finalize();
    mca_base_close();
    ompi_malloc_finalize();
    ompi_output_finalize();
    ompi_class_finalize();

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

    return 0;
}


static void find(const char *name, test_component_handle_t *handle, 
                 test_component_fn_t *ptr)
{
    test_component_sym_t sym;

    if (OMPI_SUCCESS != 
        test_component_find_symbol(name, handle, &sym)) {
        fprintf(test_out, "Unable to find symbol: %s\n", name);
        exit(1);
    }
    fprintf(test_out, "Found symbol: %s\n", name);
    *ptr = sym.tcs_function;
}
