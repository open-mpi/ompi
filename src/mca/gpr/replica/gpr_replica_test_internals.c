/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"


ompi_list_t *mca_gpr_replica_test_internals(int level)
{
    ompi_list_t *test_results=NULL;
    ompi_registry_internal_test_results_t *result=NULL;
    char name[30], name2[30];
    char *name3[30];
    int i, j, num_keys;
    mca_gpr_replica_key_t segkey, key, *keys=NULL;
    mca_gpr_replica_segment_t *seg=NULL;
    mca_gpr_replica_keytable_t *dict_entry=NULL;
    bool success=false;
    mca_ns_base_jobid_t test_jobid=0;


    test_results = OBJ_NEW(ompi_list_t);

    if (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_base_pack_test_internals(mca_gpr_replica_compound_cmd, level);
	return NULL;
    }

    ompi_output(0, "testing define segment");
    /* create several test segments */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-define-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	if (NULL == mca_gpr_replica_define_segment(name, test_jobid)) {
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing get key for segment ");
    /* check ability to get key for a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-seg-key");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	key = mca_gpr_replica_get_key(NULL, name);
	if (MCA_GPR_REPLICA_KEY_MAX == key) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing define key");
    /* check that define key protects uniqueness */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-define-key-uniqueness");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	segkey = mca_gpr_replica_get_key(NULL, name);
	key = mca_gpr_replica_define_key(NULL, name);
	if (segkey != key) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing find segment");
    /* check the ability to find a segment */
    i = 2;
    sprintf(name, "test-def-seg%d", i);
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-find-seg");
    seg = mca_gpr_replica_find_seg(false, name, test_jobid);
    if (NULL == seg) {
	asprintf(&result->message, "test failed with NULL returned: %s", name);
    } else {  /* locate key and check it */
	segkey = mca_gpr_replica_get_key(NULL, name);
	if (segkey == seg->key) {
	    result->message = strdup("success");
	} else {
	    asprintf(&result->message, "test failed: key %d seg %d", segkey, seg->key);
	}
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing define key within segment");
    /* check ability to define key within a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-define-key-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	seg = mca_gpr_replica_find_seg(false, name, test_jobid);
	for (j=0; j<10 && success; j++) {
 	    sprintf(name2, "test-key%d", j);
	    key = mca_gpr_replica_define_key(seg, name2);
	    if (MCA_GPR_REPLICA_KEY_MAX == key) { /* got an error */
		success = false;
	    }
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);


    ompi_output(0, "testing get key within segment");
    /* check ability to retrieve key within a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-key-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	seg = mca_gpr_replica_find_seg(false, name, test_jobid);
	for (j=0; j<10 && success; j++) {
 	    sprintf(name2, "test-key%d", j);
	    key = mca_gpr_replica_get_key(seg, name2);
	    if (MCA_GPR_REPLICA_KEY_MAX == key) { /* got an error */
		success = false;
	    }
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);


    ompi_output(0, "testing get dict entry - global");
    /* check ability to get dictionary entries */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-dict-entry");
    /* first check ability to get segment values */
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	dict_entry = mca_gpr_replica_find_dict_entry(NULL, name);
	if (NULL == dict_entry) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing get dict entry - segment");
    if (success) { /* segment values checked out - move on to within a segment */
	result = OBJ_NEW(ompi_registry_internal_test_results_t);
	result->test = strdup("test-get-dict-entry-segment");
	for (i=0; i<5; i++) {
	    sprintf(name, "test-def-seg%d", i);
	    seg = mca_gpr_replica_find_seg(false, name, test_jobid);
	    for (j=0; j<10; j++) {
		sprintf(name2, "test-key%d", j);
		dict_entry = mca_gpr_replica_find_dict_entry(seg, name2);
		if (NULL == dict_entry) { /* got an error */
		    success = false;
		}
	    }
	}
	if (success) {
	    result->message = strdup("success");
	} else {
	    result->message = strdup("failed");
	}
	ompi_list_append(test_results, &result->item);
    }


    ompi_output(0, "testing get key list");
    /* check ability to get key list */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-keylist");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	seg = mca_gpr_replica_find_seg(false, name, test_jobid);
	for (j=0; j<10 && success; j++) {
 	    asprintf(&name3[j], "test-key%d", j);
	}
	name3[j] = NULL;
	keys = mca_gpr_replica_get_key_list(seg, name3, &num_keys);
	if (0 >= num_keys) { /* error condition */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    /* check ability to empty segment */


    return test_results;
}
