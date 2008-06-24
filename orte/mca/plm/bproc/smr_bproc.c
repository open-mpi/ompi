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

#include "orte_config.h"

#include <pwd.h>
#include <grp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/class/opal_list.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/schema/schema_types.h"

#include "orte/mca/smr/base/smr_private.h"
#include "orte/mca/smr/bproc/smr_bproc.h"

#define BIT_MASK(bit)		(bit_set)(1 << (bit))
#define EMPTY_SET		(bit_set)0
#define BIT_NODE_NAME		0
#define BIT_NODE_STATE		1
#define BIT_NODE_BPROC_STATUS	2
#define BIT_NODE_BPROC_MODE	3
#define BIT_NODE_BPROC_USER	4
#define BIT_NODE_BPROC_GROUP	5
#define BIT_SET_ALL		( BIT_MASK(BIT_NODE_NAME) \
				| BIT_MASK(BIT_NODE_STATE) \
				| BIT_MASK(BIT_NODE_BPROC_STATUS) \
				| BIT_MASK(BIT_NODE_BPROC_MODE) \
				| BIT_MASK(BIT_NODE_BPROC_USER) \
				| BIT_MASK(BIT_NODE_BPROC_GROUP))

/* define some local variables/types */
typedef unsigned int bit_set;
static opal_list_t active_node_list;
static bool initialized=false;

static inline void set_bit(bit_set *set, int bit)
{
    *set |= BIT_MASK(bit);
}

static inline int is_set(bit_set set, int bit)
{
    return (set & BIT_MASK(bit)) == BIT_MASK(bit);
}

static inline int num_bits(bit_set set)
{
    int cnt = 0;
    int bit;

    for (bit = sizeof(bit_set) * 8 - 1; bit >= 0; bit--)
	if (is_set(set, bit))
	    cnt++;

    return cnt;
}

static inline int empty_set(bit_set set)
{
	return set == EMPTY_SET;
}


/** 
 * Query the bproc node status
 */     
              
static int orte_smr_bproc_node_state(char *status)
{   
    if (strcmp(status, "up") == 0)
	return ORTE_NODE_STATE_UP;
    if (strcmp(status, "down") == 0)
	return ORTE_NODE_STATE_DOWN;
    if (strcmp(status, "boot") == 0)
	return ORTE_NODE_STATE_REBOOT;
    return ORTE_NODE_STATE_UNKNOWN;
}

static bit_set find_changes(struct bproc_node_info_t *old, struct bproc_node_info_t *new)
{
    bit_set changes = EMPTY_SET;

    if (orte_smr_bproc_node_state(old->status) 
	    != orte_smr_bproc_node_state(new->status))
	set_bit(&changes, BIT_NODE_STATE);

    if (strcmp(old->status, new->status) != 0)
	set_bit(&changes, BIT_NODE_BPROC_STATUS);

    if (old->mode != new->mode)
	set_bit(&changes, BIT_NODE_BPROC_MODE);

    if (old->group != new->group)
	set_bit(&changes, BIT_NODE_BPROC_GROUP);

    if (old->user != new->user)
	set_bit(&changes, BIT_NODE_BPROC_USER);

    if (old->node != new->node)
	set_bit(&changes, BIT_NODE_NAME);

    return changes;
}

/**
 *  Process a BProc update notice
 */

static void update_registry(bit_set changes, struct bproc_node_info_t *ni)
{
    int idx;
    int ret;
    int cnt;
    orte_node_state_t state;
    char *node_name;
    char *user;
    char *group;
    struct passwd *pwd;
    struct group *grp;
    orte_gpr_value_t *value;
    int rc;
    orte_smr_node_state_tracker_t *node;
    opal_list_item_t *item;
    
    cnt = num_bits(changes);

    /*
     * Check if there's anything to do
     */
    if (cnt == 0)
	return;

    /* check and update the general cluster status segment - this segment has entries
     * for every node in the cluster, not just the ones we want to monitor
     */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
	                                                ORTE_BPROC_NODE_SEGMENT, cnt, 0))) {
    	ORTE_ERROR_LOG(rc);
    	return;
    }
	
    idx = 0;

    if (is_set(changes, BIT_NODE_STATE)) {
        state = orte_smr_bproc_node_state(ni->status);
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_NODE_STATE_KEY, ORTE_NODE_STATE, &state))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return;
        }
        idx++;
    }

    if (is_set(changes, BIT_NODE_BPROC_STATUS)) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_SMR_BPROC_NODE_STATUS, ORTE_STRING, ni->status))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return;
        }
        idx++;
    }

    if (is_set(changes, BIT_NODE_BPROC_MODE)) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_SMR_BPROC_NODE_MODE, ORTE_UINT32, &(ni->mode)))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return;
        }
        idx++;
    }

    if (is_set(changes, BIT_NODE_BPROC_USER)) {
    	if ((pwd = getpwuid(ni->user)))
    	    user = strdup(pwd->pw_name);
    	else
    	    asprintf(&user, "%d\n", ni->user);
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_SMR_BPROC_NODE_USER, ORTE_STRING, user))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            free(user);
            return;
        }
        free(user);
        idx++;
    }

    if (is_set(changes, BIT_NODE_BPROC_GROUP)) {
    	if ((grp = getgrgid(ni->group)))
    	    group = strdup(grp->gr_name);
    	else
    	    asprintf(&group, "%d\n", ni->group);
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_SMR_BPROC_NODE_GROUP, ORTE_STRING, group))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            free(group);
            return;
        }
        free(group);
        idx++;
    }

    asprintf(&node_name, "%d", ni->node);

    if (is_set(changes, BIT_NODE_NAME)) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[idx]), ORTE_NODE_NAME_KEY, ORTE_STRING, node_name))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            free(node_name);
            return;
        }
        idx++;
    }

    if (idx != cnt) {
    	opal_output(0, "smr_bproc: internal error %d != %d\n", idx, cnt);
        free(node_name);
    	OBJ_RELEASE(value);
    	opal_event_del(&mca_smr_bproc_component.notify_event);
    	return;
    }

    ret = orte_schema.get_node_tokens(&(value->tokens), &(value->num_tokens), 
	    ORTE_PROC_MY_NAME->cellid, node_name);

    if (ret != ORTE_SUCCESS) {
    	ORTE_ERROR_LOG(ret);
    	OBJ_RELEASE(value);
    	free(node_name);
    	opal_event_del(&mca_smr_bproc_component.notify_event);
    	return;
    }

    if (mca_smr_bproc_component.debug)
    	opal_output(0, "updating node %d to segment %s\n", ni->node, value->segment);

    if ((ret = orte_gpr.put(1, &value)) != ORTE_SUCCESS) {
    	ORTE_ERROR_LOG(ret);
    	opal_event_del(&mca_smr_bproc_component.notify_event);
    }
    OBJ_RELEASE(value);

    /* now let's see if this is one of the nodes we are monitoring and
     * update it IFF it the state changed to specified conditions. This
     * action will trigger a callback to the right place to decide what
     * to do about it
     */
    if (mca_smr_bproc_component.monitoring &&
        is_set(changes, BIT_NODE_STATE)) {
        /* see if this is a node we are monitoring */
        for (item = opal_list_get_first(&active_node_list);
             item != opal_list_get_end(&active_node_list);
             item = opal_list_get_next(item)) {
            node = (orte_smr_node_state_tracker_t*)item;
            if (0 == strcmp(node->nodename, node_name)) {
                /* This is a node we are monitoring. If this is a state we care about,
                 * and the state has changed (so we only do this once) - trip the alert monitor
                 */
                if (state != node->state &&
                    (state == ORTE_NODE_STATE_DOWN || state == ORTE_NODE_STATE_REBOOT)) {
                    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                                    ORTE_BPROC_NODE_SEGMENT, 1, 0))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    value->tokens[0] = strdup(ORTE_BPROC_NODE_GLOBALS);
                    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                                     ORTE_BPROC_NODE_ALERT_CNTR,
                                                                     ORTE_UNDEF, NULL))) {
                        ORTE_ERROR_LOG(rc);
                        OBJ_RELEASE(value);
                        return;
                    }
                    if ((rc = orte_gpr.increment_value(value)) != ORTE_SUCCESS) {
                        ORTE_ERROR_LOG(rc);
                        opal_event_del(&mca_smr_bproc_component.notify_event);
                    }
                    OBJ_RELEASE(value);
                }
                /* update our local records */
                node->state = state;
                /* cleanup and return - no need to keep searching */
                free(node_name);
                return;
            }
        }
    }
    
    /* if this isn't someone we are monitoring, or it doesn't meet specified conditions,
     * then just cleanup and leave
     */
    free(node_name);
}


static int do_update(struct bproc_node_set_t *ns)
{
    int i;
    int changed = 0;
    bit_set changes;
    struct bproc_node_info_t *ni;

    /* we assume the number of nodes does not change */
    for (i = 0; i < ns->size; i++) {
    	ni = &ns->node[i];
    
    	if (mca_smr_bproc_component.node_set.size > 0
    		&& mca_smr_bproc_component.node_set.size == ns->size)
    	    changes = find_changes(&mca_smr_bproc_component.node_set.node[i], ni);
    	else
    	    changes = BIT_SET_ALL;
    
    	if (!empty_set(changes)) {
    		update_registry(changes, ni);
    		changed = 1;
    	}
    }

    if (changed) {
    	if (mca_smr_bproc_component.node_set.size != 0)
    	    bproc_nodeset_free(&mca_smr_bproc_component.node_set);
    	mca_smr_bproc_component.node_set = *ns;
    }

    return changed;
}

static void orte_smr_bproc_notify_handler(int fd, short flags, void *user)
{
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;

    if (bproc_nodelist_(&ns, fd) < 0) {
    	/* bproc_nodelist_ error */
    	opal_event_del(&mca_smr_bproc_component.notify_event);
    	return;
    }

    if (!do_update(&ns))
	    bproc_nodeset_free(&ns);
}

/**
 * Register a callback to receive BProc update notifications
 */
static int orte_smr_bproc_module_init(void)
{
    if (mca_smr_bproc_component.debug)
	    opal_output(0, "init smr_bproc_module\n");

    mca_smr_bproc_component.node_set.size = 0;

    /* construct the monitored node list so we can track who is being monitored */
    OBJ_CONSTRUCT(&active_node_list, opal_list_t);
    
    return ORTE_SUCCESS;
}

/*
 * Setup to begin monitoring a job
 */
int orte_smr_bproc_begin_monitoring(orte_job_map_t *map, orte_gpr_trigger_cb_fn_t cbfunc, void *user_tag)
{
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;
    opal_list_item_t *item;
    orte_mapped_node_t *node;
    orte_smr_node_state_tracker_t *newnode;

    /* if our internal structures haven't been initialized, then
     * set them up
     */
    if (!initialized) {
        orte_smr_bproc_module_init();
        initialized = true;
    }
    
    /* setup the local monitoring list */
    for (item = opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item = opal_list_get_next(item)) {
        node = (orte_mapped_node_t*)item;
        
        newnode = OBJ_NEW(orte_smr_node_state_tracker_t);
        newnode->cell = node->cell;
        newnode->nodename = strdup(node->nodename);
        opal_list_append(&active_node_list, &newnode->super);
    }
    
    /* define the alert monitor to call the cbfunc if we trigger the alert */
    orte_smr.define_alert_monitor(map->job, ORTE_BPROC_NODE_ALERT_TRIG,
                                  ORTE_BPROC_NODE_ALERT_CNTR,
                                  0, 1, true, cbfunc, user_tag);
    
    /*
     * Set initial node status for all nodes in the local cell. We will
     * receive reports from them all, but we will only provide alerts
     * on those we are actively monitoring
     */
    
    if (bproc_nodelist(&ns) < 0)
    	return ORTE_ERROR;
    
    if (!do_update(&ns))
	    bproc_nodeset_free(&ns);
    
    /*
     * Now register notify event
     */
    
    mca_smr_bproc_component.notify_fd = bproc_notifier();
    if (mca_smr_bproc_component.notify_fd < 0)
	    return ORTE_ERROR;
    
    memset(&mca_smr_bproc_component.notify_event, 0, sizeof(opal_event_t));
    
    opal_event_set(
                   &mca_smr_bproc_component.notify_event,
                   mca_smr_bproc_component.notify_fd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   orte_smr_bproc_notify_handler,
                   0);
    
    opal_event_add(&mca_smr_bproc_component.notify_event, 0);
    
    return ORTE_SUCCESS;
}
/**
 *  Cleanup
 */

int orte_smr_bproc_finalize(void)
{
    opal_event_del(&mca_smr_bproc_component.notify_event);
    return ORTE_SUCCESS;
}
