/*
 * $HEADER$
 */

/** @file **/

/* #define _GNU_SOURCE */

#include "ompi_config.h"

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/base/base.h"
#include "mca/llm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"

#include "runtime/runtime.h"

mca_pcm_base_module_t *mca_pcm;

int ompi_rte_init_stage2(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int ret;
    bool user_threads, hidden_threads;
    mca_pcm_base_module_t **pcm_modules;
    int pcm_modules_len;

    /*
     * Name Server - base already opened in stage1, so just complete the selection
     * of the proper module
     */
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_ns_base_select(&user_threads,
						  &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in ns_base_select\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    /*
     * Process Control and Monitoring Client - base already opened in stage1, so
     * just complete selection of proper module
     */
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_pcmclient_base_select(&user_threads, 
							 &hidden_threads))) {
	printf("show_help: ompi_rte_init failed in pcmclient_base_select\n");
	/* JMS show_help */
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    /*
     * Allocation code - open only.  pcm will init if needed
     */
    if (OMPI_SUCCESS != (ret = mca_llm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in llm_base_open\n");
	return ret;
    }

    /*
     * Process Control and Monitoring
     */
    if (OMPI_SUCCESS != (ret = mca_pcm_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in pcm_base_open\n");
	return ret;
    }
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_pcm_base_select(&user_threads, 
						   &hidden_threads, 0,
                                                   &pcm_modules,
                                                   &pcm_modules_len))) {
	printf("show_help: ompi_rte_init failed in pcm_base_select\n");
	/* JMS show_help */
	return ret;
    }
    if (pcm_modules_len != 1) {
        printf("show_help: unexpectedly high return from pcm_modules_len\n");
        return -1;
    }
    mca_pcm = pcm_modules[0];
    free(pcm_modules);
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    /*
     * Registry 
     */
    if (OMPI_SUCCESS != (ret = mca_gpr_base_open())) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_open()\n");
	return ret;
    }
    user_threads = true;
    hidden_threads = false;
    if (OMPI_SUCCESS != (ret = mca_gpr_base_select(&user_threads, 
						   &hidden_threads))) {
	/* JMS show_help */
	printf("show_help: ompi_rte_init failed in mca_gpr_base_select()\n");
	return ret;
    }
    *allow_multi_user_threads &= user_threads;
    *have_hidden_threads |= hidden_threads;

    /* 
     * All done 
     */
    return OMPI_SUCCESS;
}


/*
 *  interface type support
 */

/** constructor for \c ompi_rte_node_schedule_t */
static
void
ompi_rte_int_node_schedule_construct(ompi_object_t *obj)
{
    ompi_rte_node_schedule_t *sched = (ompi_rte_node_schedule_t*) obj;
    sched->nodelist = OBJ_NEW(ompi_list_t);
}


/** destructor for \c ompi_rte_node_schedule_t */
static
void
ompi_rte_int_node_schedule_destruct(ompi_object_t *obj)
{
    ompi_rte_node_schedule_t *sched = (ompi_rte_node_schedule_t*) obj;
    ompi_rte_node_allocation_t *node;
    ompi_list_item_t *item;

    if (NULL == sched->nodelist) return;

    while (NULL != (item = ompi_list_remove_first(sched->nodelist))) {
        node = (ompi_rte_node_allocation_t*) item;
        OBJ_RELEASE(node);
    }

    OBJ_RELEASE(sched->nodelist);
}


/** constructor for \c ompi_rte_node_allocation_t */
static
void
ompi_rte_int_node_allocation_construct(ompi_object_t *obj)
{
    ompi_rte_node_allocation_t *node = (ompi_rte_node_allocation_t*) obj;
    node->start = 0;
    node->nodes = 0;
    node->count = 0;
    node->data = NULL;
}


/** destructor for \c ompi_rte_node_allocation_t */
static
void
ompi_rte_int_node_allocation_destruct(ompi_object_t *obj)
{
    ompi_rte_node_allocation_t *node = (ompi_rte_node_allocation_t*) obj;

    if (NULL == node->data) return;

    OBJ_RELEASE(node->data);
}


/** constructor for \c ompi_rte_valuepair_t */
static
void
ompi_rte_int_valuepair_construct(ompi_object_t *obj)
{
    ompi_rte_valuepair_t *valpair = (ompi_rte_valuepair_t*) obj;
    valpair->key = NULL;
    valpair->value = NULL;
}


/** destructor for \c ompi_rte_valuepair_t */
static
void
ompi_rte_int_valuepair_destruct(ompi_object_t *obj)
{
    ompi_rte_valuepair_t *valpair = (ompi_rte_valuepair_t*) obj;
    if (NULL != valpair->key) free(valpair->key);
    if (NULL != valpair->value) free(valpair->value);
}

/** create instance information for \c ompi_rte_node_schedule_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_schedule_t, ompi_list_item_t,
                   ompi_rte_int_node_schedule_construct,
                   ompi_rte_int_node_schedule_destruct);
/** create instance information for \c ompi_rte_node_allocation_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_allocation_t, ompi_list_item_t, 
                   ompi_rte_int_node_allocation_construct, 
                   ompi_rte_int_node_allocation_destruct);
/** create instance information for \c ompi_rte_valuepair_t */
OBJ_CLASS_INSTANCE(ompi_rte_valuepair_t, ompi_list_item_t, 
                   ompi_rte_int_valuepair_construct,
                   ompi_rte_int_valuepair_destruct);
/** create instance information for \c ompi_rte_node_allocation_data_t */
OBJ_CLASS_INSTANCE(ompi_rte_node_allocation_data_t, ompi_object_t, 
                   NULL, NULL);
