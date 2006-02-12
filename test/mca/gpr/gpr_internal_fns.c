/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "opal/runtime/opal.h"
#include "orte/orte_constants.h"

#include "opal/util/malloc.h"
#include "opal/util/output.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/gpr/replica/api_layer/gpr_replica_api.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "orte/mca/gpr/replica/communications/gpr_replica_comm.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"


int main(int argc, char **argv)
{
    int rc;
    size_t num_names;
    size_t i;
    char *tmp=NULL, *tmp2=NULL, *names[15], *keys[5];
    orte_gpr_replica_segment_t *seg=NULL;
    orte_gpr_replica_itag_t itag[10], itag2, *itaglist;
    orte_gpr_replica_container_t *cptr=NULL;
    orte_gpr_keyval_t *kptr=NULL;
    orte_gpr_replica_itagval_t *iptr;
    int16_t i16;
    int32_t i32;

    /* ENSURE THE REPLICA IS ISOLATED */
    setenv("OMPI_MCA_gpr_replica_isolate", "1", 1);

    opal_init();

    /* register handler for errnum -> string converstion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (rc = orte_proc_info())) {
        return rc;
    }

    orte_process_info.seed = true;
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->cellid = 0;
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(stderr, "MCA started\n");
    } else {
        fprintf(stderr, "MCA could not start\n");
        exit (1);
    }

    /* open the dss */
    if (ORTE_SUCCESS == orte_dss_open()) {
        fprintf(stderr, "DSS started\n");
    } else {
        fprintf(stderr, "DSS could not start\n");
        exit (1);
    }

    /* startup the gpr to register data types */
    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(stderr, "GPR opened\n");
    } else {
        fprintf(stderr, "GPR could not open\n");
        exit (1);
    }

    /* do a select on the registry components */
    if (OMPI_SUCCESS == orte_gpr_base_select()) {
        fprintf(stderr, "GPR selected\n");
    } else {
        fprintf(stderr, "GPR could not select\n");
        exit (1);
    }

    /* initialize the pointer variables */
    for (i=0; i < 15; i++) names[i]=NULL;
    for (i=0; i < 5; i++) keys[i] = NULL;


    /* Now do the tests */

    fprintf(stderr, "going to find seg\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_find_seg(&seg, true, "test-segment"))) {
        fprintf(stderr, "gpr_test: find_seg failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: find_seg passed\n");
    }

    orte_gpr.dump_all(0);

    fprintf(stderr, "creating tags\n");
    for (i=0; i<10; i++) {
        asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&itag[i], seg, tmp))) {
            fprintf(stderr, "gpr_test: create_itag failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            return rc;
        } else {
            fprintf(stderr, "gpr_test: create_itag passed\n");
        }
        free(tmp);
    }

    fprintf(stderr, "lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_lookup(&itag2, seg, tmp)) ||
             itag2 != itag[i]) {
            fprintf(stderr, "gpr_test: lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            return rc;
        } else {
            fprintf(stderr, "gpr_test: lookup passed\n");
        }
        free(tmp);
    }

    fprintf(stderr, "reverse lookup tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp2, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_reverse_lookup(&tmp, seg, itag[i])) ||
             0 != strcmp(tmp2, tmp)) {
            fprintf(stderr, "gpr_test: reverse lookup failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            return rc;
        } else {
            fprintf(stderr, "gpr_test: reverse lookup passed\n");
        }
        free(tmp);
        free(tmp2);
    }

    fprintf(stderr, "delete tags\n");
    for (i=0; i<10; i++) {
         asprintf(&tmp, "test-tag-%lu", (unsigned long) i);
         if (ORTE_SUCCESS != (rc = orte_gpr_replica_delete_itag(seg, tmp))) {
            fprintf(stderr, "gpr_test: delete tag failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            return rc;
        } else {
            fprintf(stderr, "gpr_test: delete tag passed\n");
        }
        free(tmp);
    }

    fprintf(stderr, "get itag list\n");
    for (i=0; i < 14; i++) {
        asprintf(&names[i], "dummy%lu", (unsigned long) i);
    }
    names[14] = NULL;
    num_names = 0;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_get_itag_list(&itaglist, seg,
                                                names, &num_names))) {
        fprintf(stderr, "gpr_test: get itag list failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
       return rc;
    } else {
        fprintf(stderr, "gpr_test: get itag list passed\n");
    }

    fprintf(stderr, "number of names found %lu\n",
            (unsigned long) num_names);
    for (i=0; i < num_names; i++) {
        fprintf(stderr, "\tname %s itag %lu\n", names[i],
                (unsigned long) itaglist[i]);
    }

    fprintf(stderr, "creating container\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_container(&cptr, seg,
                                3, itaglist))) {
        fprintf(stderr, "gpr_test: create_container failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: create_container passed\n");
    }

    orte_gpr.dump_all(0);

    fprintf(stderr, "itags for container\n");
    for (i=0; i < cptr->num_itags; i++) {
        fprintf(stderr, "\tindex %lu itag %lu\n", (unsigned long) i,
                (unsigned long) cptr->itags[i]);
    }

    fprintf(stderr, "add keyval\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->value = OBJ_NEW(orte_data_value_t);
    kptr->value->type = ORTE_INT16;
    i16 = 21;
    orte_dss.copy(&(kptr->value->data), &i16, ORTE_INT16);
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr, kptr))) {
        fprintf(stderr, "gpr_test: add keyval failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: add keyval passed\n");
    }
    OBJ_RELEASE(kptr);

    orte_gpr.dump_all(0);

    fprintf(stderr, "search container for single entry\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->value = OBJ_NEW(orte_data_value_t);
    kptr->value->type = ORTE_STRING;
    kptr->value->data = strdup("try-string-value");
    orte_gpr_replica_create_itag(&itag2, seg, kptr->key);
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_search_container(ORTE_GPR_REPLICA_OR,
                                        &itag2, 1, cptr) ||
        0 >= orte_gpr_replica_globals.num_srch_ival)) {
        fprintf(stderr, "gpr_test: search container for single entry failed - returned %s for itag %lu\n",
                            ORTE_ERROR_NAME(rc), (unsigned long) itag2);
        return -1;
    } else {
        fprintf(stderr, "gpr_test: search container for single entry passed\n");
    }
    OBJ_RELEASE(kptr);

    fprintf(stderr, "update single keyval\n");
    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->value = OBJ_NEW(orte_data_value_t);
    kptr->value->type = ORTE_STRING;
    kptr->value->data =strdup("try-string-value");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_keyval(&iptr, seg, cptr, kptr))) {
        fprintf(stderr, "gpr_test: update single keyval failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: update single keyval passed\n");
    }
    OBJ_RELEASE(kptr);

    orte_gpr.dump_all(0);

    fprintf(stderr, "add multiple keyvals to a container\n");
    for (i=0; i < 10; i++) {
        kptr = OBJ_NEW(orte_gpr_keyval_t);
        kptr->key = strdup("stupid-value");
        kptr->value = OBJ_NEW(orte_data_value_t);
        kptr->value->type = ORTE_INT16;
        i16 = i * 100;
        orte_dss.copy(&(kptr->value->data), &i16, ORTE_INT16);
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr, kptr))) {
            fprintf(stderr, "gpr_test: add keyval failed with error code %s\n",
                        ORTE_ERROR_NAME(rc));
            return rc;
        } else {
            fprintf(stderr, "gpr_test: add keyval passed\n");
        }
        OBJ_RELEASE(kptr);
    }

    orte_gpr.dump_all(0);

    kptr = OBJ_NEW(orte_gpr_keyval_t);
    kptr->key = strdup("stupid-value");
    kptr->value = OBJ_NEW(orte_data_value_t);
    kptr->value->type = ORTE_INT32;
    i32 = 123456;
    orte_dss.copy(&(kptr->value->data), &i32, ORTE_INT32);
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&itag2, seg, kptr->key))) {
        fprintf(stderr, "gpr_internal_fns: update multiple keyvals - failed to get itag with error %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_search_container(ORTE_GPR_REPLICA_OR,
                                                &itag2, 1, cptr))) {
        fprintf(stderr, "gpr_internal_fns: update multiple keyvals - failed to find itag with error %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_keyval(&iptr, seg, cptr, kptr))) {
        fprintf(stderr, "gpr_test: update multiple keyvals failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: update multiple keyvals passed\n");
    }
    OBJ_RELEASE(kptr);

    orte_gpr.dump_all(0);

    fprintf(stderr, "check itag list\n");
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 0, NULL, 10, itaglist)) {
        fprintf(stderr, "check_itag_list: trivial NULL case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: trivial NULL case failed\n");
    }
    if (!orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 5, itaglist, 10, itaglist)) {
        fprintf(stderr, "check_itag_list: trivial mismatched xand case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: trivial mismatched xand case failed\n");
    }
    if (!orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_AND, 10, itaglist, 5, itaglist)) {
        fprintf(stderr, "check_itag_list: trivial mismatched and case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: trivial mismatched and case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XAND, 10, itaglist, 10, itaglist)) {
        fprintf(stderr, "check_itag_list: non-trivial xand case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: non-trivial xand case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_AND, 5, itaglist, 10, itaglist)) {
        fprintf(stderr, "check_itag_list: non-trivial and case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: non-trivial and case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_OR, 5, itaglist, 10, itaglist)) {
        fprintf(stderr, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: non-trivial or case failed\n");
    }
    if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_XOR, 10, itaglist, 5, itaglist)) {
        fprintf(stderr, "check_itag_list: non-trivial or case passed\n");
    } else {
        fprintf(stderr, "check_itag_list: non-trivial or case failed\n");
    }

    fprintf(stderr, "\ncheck events prior to releasing segment to clear action records\n");
    orte_gpr_replica_check_events();

    fprintf(stderr, "\nreleasing segment\n");
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_release_segment(&seg)) ||
        NULL != seg) {
        fprintf(stderr, "gpr_test: release segment failed with error code %s\n",
                    ORTE_ERROR_NAME(rc));
        return rc;
    } else {
        fprintf(stderr, "gpr_test: release segment passed\n");
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
    orte_gpr_base_close();

    orte_dss_close();

    mca_base_close();
    opal_malloc_finalize();
    opal_output_finalize();
    opal_class_finalize();

    return 0;
}

