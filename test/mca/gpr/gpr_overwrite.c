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
#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "include/orte_schema.h"

#include <stdio.h>
#include <string.h>

#include "support.h"

#include "util/proc_info.h"
#include "util/sys_info.h"
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

/**
 * Struct for holding information 
 */
struct test_node_t {
    /** Base object */
    ompi_list_item_t super;
    /** String node name */
    char *node_name;
    /** String of the architecture for the node.  This is permitted to
        be NULL if it is not known. */
    char *node_arch;
    /** The cell ID of this node */
    orte_cellid_t node_cellid;
    /** State of this node; see include/orte_types.h */
    orte_node_state_t node_state;
    /** A "soft" limit on the number of slots available on the node.
        This will typically correspond to the number of physical CPUs
        that we have been allocated on this note and would be the
        "ideal" number of processes for us to launch. */
    size_t node_slots;
    /** How many processes have already been launched, used by one or
        more jobs on this node. */
    size_t node_slots_inuse;
    /** This represents the number of slots we (the allocator) are
        attempting to allocate to the current job - or the number of
        slots allocated to a specific job on a query for the jobs
        allocations */
    size_t node_slots_alloc;
    /** A "hard" limit (if set -- a value of 0 implies no hard limit)
        on the number of slots that can be allocated on a given
        node. This is for some environments (e.g. grid) there may be
        fixed limits on the number of slots that can be used.

        This value also could have been a boolean - but we may want to
        allow the hard limit be different than the soft limit - in
        other words allow the node to be oversubscribed up to a
        specified limit.  For example, if we have two processors, we
        may want to allow up to four processes but no more. */
    size_t node_slots_max;
};
/**
 * Convenience typedef
 */
typedef struct test_node_t test_node_t;

static void test_node_construct(test_node_t* node)
{
    node->node_name = NULL;
    node->node_arch = NULL;
    node->node_cellid = 0;
    node->node_state = ORTE_NODE_STATE_UNKNOWN;
    node->node_slots = 0;
    node->node_slots_alloc = 0;
    node->node_slots_inuse = 0;
    node->node_slots_max = 0;
}

static void test_node_destruct(test_node_t* node)
{
    if (NULL != node->node_name) {
        free(node->node_name);
    }
    if (NULL != node->node_arch) {
        free(node->node_arch);
    }
}


OBJ_CLASS_INSTANCE(
    test_node_t,
    ompi_list_item_t,
    test_node_construct,
    test_node_destruct);


static int test_overwrite(ompi_list_t* nodes);

int main(int argc, char **argv)
{
    ompi_list_t nodes;
    test_node_t *node;
    orte_process_name_t seed={0,0,0};
    int i, rc;

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
    
    /* setup a node list */
    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    for (i=0; i < 5; i++) {
        node = OBJ_NEW(test_node_t);
        asprintf(&(node->node_name), "node-%d", i);
        asprintf(&(node->node_arch), "arch-%d", i);
        node->node_cellid = 0;
        node->node_state = ORTE_NODE_STATE_UP;
        node->node_slots = i;
        node->node_slots_alloc = i%2;
        node->node_slots_inuse = i % 3;
        node->node_slots_max = i * 5;
        ompi_list_append(&nodes, &node->super);
    }

    fprintf(test_out, "putting initial set of values on registry\n");
    if (ORTE_SUCCESS != (rc = test_overwrite(&nodes))) {
        fprintf(test_out, "initial put of values failed with error %s\n",
            ORTE_ERROR_NAME(rc));
            return rc;
    } else {
        fprintf(test_out, "initial put of values successful\n");
    }
    
    orte_gpr.dump_all(0);
    
    fprintf(test_out, "changing values for overwrite test\n");
    /* change the arch, state, and slots_inuse values */
    for (i=0, node = (test_node_t*)ompi_list_get_first(&nodes);
         node != (test_node_t*)ompi_list_get_end(&nodes);
         node = (test_node_t*)ompi_list_get_next(node), i++) {
         
         free(node->node_arch);
         asprintf(&(node->node_arch), "new-arch-%d", i*10);
         node->node_state = ORTE_NODE_STATE_DOWN;
         node->node_slots_inuse = node->node_slots_inuse * 20;
    }

    fprintf(test_out, "putting second set of values on registry to test overwrite\n");
    if (ORTE_SUCCESS != (rc = test_overwrite(&nodes))) {
        fprintf(test_out, "second put of values failed with error %s\n",
            ORTE_ERROR_NAME(rc));
            return rc;
    } else {
        fprintf(test_out, "second put of values successful\n");
    }
    
    orte_gpr.dump_all(0);
    
    fclose( test_out );

    test_finalize();

    return(0);
}


int test_overwrite(ompi_list_t* nodes)
{
    ompi_list_item_t* item;
    orte_gpr_value_t **values;
    int rc, num_values, i, j;
    test_node_t* node;
    
    num_values = ompi_list_get_size(nodes);
    if (0 >= num_values) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    values = (orte_gpr_value_t**)malloc(num_values * sizeof(orte_gpr_value_t*));
    if (NULL == values) {
       ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
       return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0; i < num_values; i++) {
        orte_gpr_value_t* value = values[i] = OBJ_NEW(orte_gpr_value_t);
        if (NULL == value) {
            for (j=0; j < i; j++) {
                OBJ_RELEASE(values[j]);
            }
            free(values);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        value->addr_mode = ORTE_GPR_OVERWRITE;
        value->segment = strdup(ORTE_NODE_SEGMENT);
        value->cnt = 6;
        value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt*sizeof(orte_gpr_keyval_t*));
        if (NULL == value->keyvals) {
            for (j=0; j < i; j++) {
                OBJ_RELEASE(values[j]);
            }
            free(values);
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        
        for (j=0; j < value->cnt; j++) {
            value->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
            if (NULL == value->keyvals[j]) {
                for (j=0; j <= i; j++) {
                    OBJ_RELEASE(values[j]);
                }
                free(values);
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
        }
    }
    
    for(i=0, item =  ompi_list_get_first(nodes);
        i < num_values && item != ompi_list_get_end(nodes);
        i++, item =  ompi_list_get_next(item)) {
        orte_gpr_value_t* value = values[i];
        node = (test_node_t*)item;

        j = 0;
        (value->keyvals[j])->key = strdup(ORTE_NODE_NAME_KEY);
        (value->keyvals[j])->type = ORTE_STRING;
        (value->keyvals[j])->value.strptr = strdup(node->node_name);
        
        ++j;
        (value->keyvals[j])->key = strdup(ORTE_NODE_ARCH_KEY);
        (value->keyvals[j])->type = ORTE_STRING;
        if (NULL != node->node_arch) {
            (value->keyvals[j])->value.strptr = strdup(node->node_arch);
        } else {
            (value->keyvals[j])->value.strptr = strdup("");
        }
        
        ++j;
        (value->keyvals[j])->key = strdup(ORTE_NODE_STATE_KEY);
        (value->keyvals[j])->type = ORTE_NODE_STATE;
        (value->keyvals[j])->value.node_state = node->node_state;
        
        ++j;
        (value->keyvals[j])->key = strdup(ORTE_CELLID_KEY);
        (value->keyvals[j])->type = ORTE_CELLID;
        (value->keyvals[j])->value.cellid = node->node_cellid;
        
        ++j;
        (value->keyvals[j])->key = strdup(ORTE_NODE_SLOTS_KEY);
        (value->keyvals[j])->type = ORTE_UINT32;
        (value->keyvals[j])->value.ui32 = node->node_slots;
        
        ++j;
        (value->keyvals[j])->key = strdup(ORTE_NODE_SLOTS_MAX_KEY);
        (value->keyvals[j])->type = ORTE_UINT32;
        (value->keyvals[j])->value.ui32 = node->node_slots_max;

        /* setup index/keys for this node */
        rc = orte_schema.get_node_tokens(&value->tokens, &value->num_tokens, node->node_cellid, node->node_name);
        if (ORTE_SUCCESS != rc) {
            for (j=0; j <= i; j++) {
                OBJ_RELEASE(values[j]);
            }
            free(values);
            return rc;
        }
    }
    
    /* try the insert */
    rc = orte_gpr.put(num_values, values);

    for (j=0; j < num_values; j++) {
          OBJ_RELEASE(values[j]);
    }
    free(values);
    return rc;
}
