#include "ompi_config.h"
#include "util/output.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"
#include "svc_sched.h"
#include "svc_sched_node.h"


mca_svc_base_module_t mca_svc_sched_module = {
    mca_svc_sched_module_init,
    mca_svc_sched_module_fini
};


/**
 *  Process an OOB request.
 */

static void mca_svc_sched_recv(
    int status,
    ompi_process_name_t* peer,
    ompi_buffer_t buffer,
    int tag,
    void* cbdata)
{
    /* unpack the request */
    ompi_buffer_t response;
    int32_t jobid;
    int32_t num_nodes;
    int32_t num_procs;
    mca_svc_sched_node_t* node;
    int rc;
   
    ompi_unpack(buffer, &jobid, 1, OMPI_INT32);
    ompi_unpack(buffer, &num_nodes, 1, OMPI_INT32);
    ompi_unpack(buffer, &num_procs, 1, OMPI_INT32);
 
    ompi_buffer_init(&response, 128);

    /* iterate through the available daemons and attempt to allocate processes
     * in a round-robin fashion - building the response to the requestor.
    */   
    OMPI_THREAD_LOCK(&mca_svc_sched_component.sched_lock);

    /* start with the last used node */
    node = mca_svc_sched_component.sched_node_next;
    if (node == (mca_svc_sched_node_t*)ompi_list_get_end(&mca_svc_sched_component.sched_node_list))
        node = (mca_svc_sched_node_t*)ompi_list_get_first(&mca_svc_sched_component.sched_node_list);


    OMPI_THREAD_UNLOCK(&mca_svc_sched_component.sched_lock);
   
    /* send the response back to the peer */
    rc = mca_oob_send_packed(peer, response, tag, 0);
    if(rc < 0) {
        ompi_output(0, "mca_svc_sched_recv: mca_oob_send_packed failed with status=%d\n", rc);
    }
    ompi_buffer_free(response);
}


/**
 *  Registry callback on change to list of registered daemons.
 */

static void mca_svc_sched_registry_callback(
    ompi_registry_notify_message_t* msg,
    void* cbdata)
{
    ompi_list_item_t* item;
    if(mca_svc_sched_component.sched_debug > 1) {
        ompi_output(0, "[%d,%d,%d] mca_svc_sched_registry_callback\n",
            OMPI_NAME_ARGS(mca_oob_name_self));
    }
    OMPI_THREAD_LOCK(&mca_svc_sched_component.sched_lock);
    for(item =  ompi_list_get_first(&msg->data);
        item != ompi_list_get_end(&msg->data);
        item =  ompi_list_get_next(item)) {

        ompi_registry_value_t* value = (ompi_registry_value_t*)item;
        mca_svc_sched_node_t* node;
        char* hostname = NULL;
        ompi_process_name_t proc_name;
        char* contact_info = NULL;
        int32_t proc_slots;

        /* unpack the daemon information */
        ompi_buffer_t buffer;
        ompi_buffer_init_preallocated(&buffer, value->object, value->object_size);
        ompi_unpack_string(buffer, &hostname);
        ompi_unpack(buffer, &proc_name, 1, OMPI_NAME);
        ompi_unpack_string(buffer, &contact_info);
        ompi_unpack(buffer, &proc_slots, 1, OMPI_INT32);
        ompi_buffer_free(buffer);

        /* lookup the corresponding node */
        node = (mca_svc_sched_node_t*)ompi_rb_tree_find(
            &mca_svc_sched_component.sched_node_tree,
            &proc_name);

        /* do the right thing based on the trigger type */
        if ((OMPI_REGISTRY_NOTIFY_MODIFICATION & msg->trig_action) ||
	    (OMPI_REGISTRY_NOTIFY_ADD_ENTRY & msg->trig_action) ||
	    (OMPI_REGISTRY_NOTIFY_PRE_EXISTING & msg->trig_action)) {
                /* create or modify the corresponding daemon entry */
                if(node == NULL) {
                    node = OBJ_NEW(mca_svc_sched_node_t);
                    node->node_name = proc_name;
                    ompi_rb_tree_insert(&mca_svc_sched_component.sched_node_tree, &node->node_name, node);
                    ompi_list_append(&mca_svc_sched_component.sched_node_list, &node->node_item);
                } 
                mca_svc_sched_node_set(node,hostname,contact_info,proc_slots);
	} else if (OMPI_REGISTRY_NOTIFY_DELETE_ENTRY & msg->trig_action) {
                /* delete the corresponding deamon entry */
                if(node != NULL) {
                    ompi_list_item_t* next;
                    ompi_rb_tree_delete(&mca_svc_sched_component.sched_node_tree, &proc_name);
                    next = ompi_list_remove_item(&mca_svc_sched_component.sched_node_list, &node->node_item);
                    OBJ_RELEASE(node);
                    if(mca_svc_sched_component.sched_node_next == node)
                        mca_svc_sched_component.sched_node_next = (mca_svc_sched_node_t*)next;
                }
	}

        /* cleanup */
        if(hostname != NULL)
            free(hostname);
        if(contact_info != NULL)
            free(contact_info);
    }
    OMPI_THREAD_UNLOCK(&mca_svc_sched_component.sched_lock);
}


/**
 * Register a callback to receive OOB requests.
 */

int mca_svc_sched_module_init(mca_svc_base_module_t* module)
{
    /* register */
    int rc;
    ompi_registry_notify_id_t rc_tag;

    if(mca_svc_sched_component.sched_debug > 0) {
        ompi_output(0, "[%d,%d,%d] mca_svc_sched_module_init: calling ompi_registry.subscribe(\"vm\")");
    }
    /* initialize next node for scheduling */
    mca_svc_sched_component.sched_node_next = 
        (mca_svc_sched_node_t*)ompi_list_get_end(&mca_svc_sched_component.sched_node_list);

    rc_tag = ompi_registry.subscribe(
             OMPI_REGISTRY_NONE,
	     OMPI_REGISTRY_NOTIFY_MODIFICATION|
	     OMPI_REGISTRY_NOTIFY_ADD_ENTRY|
	     OMPI_REGISTRY_NOTIFY_DELETE_ENTRY|
	     OMPI_REGISTRY_NOTIFY_PRE_EXISTING |
	     OMPI_REGISTRY_NOTIFY_ON_STARTUP,
	     OMPI_RTE_VM_STATUS_SEGMENT, /* segment */
	     NULL, /* keys */
	     mca_svc_sched_registry_callback,
	     NULL);
    if(rc_tag -= OMPI_REGISTRY_NOTIFY_ID_MAX) {
        ompi_output(0, "[%d,%d,%d] mca_svc_sched_module_init: ompi_registry.subscribe failed", 
		    OMPI_NAME_ARGS(mca_oob_name_self));
        return OMPI_ERROR;
    }

    rc = mca_oob_recv_packed_nb(
        MCA_OOB_NAME_ANY,  
        MCA_OOB_TAG_SCHED, 
        MCA_OOB_ALLOC, 
        mca_svc_sched_recv,
        NULL);
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "[%d,%d,%d] mca_svc_sched_module_init: mca_oob_recv_packed_nb failed, error=%d\n", 
            OMPI_NAME_ARGS(mca_oob_name_self), rc);
        return rc;
    }
    return OMPI_SUCCESS;
}


/**
 *  Cleanup
 */

int mca_svc_sched_module_fini(mca_svc_base_module_t* module)
{
    return OMPI_SUCCESS;
}

