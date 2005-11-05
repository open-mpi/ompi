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

#include "ompi_config.h"

#include <pwd.h>
#include <grp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "util/proc_info.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"
#include "mca/gpr/base/base.h"
#include "mca/soh/base/base.h"
#include "mca/soh/bproc/soh_bproc.h"

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

typedef unsigned int bit_set;

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

static int orte_soh_bproc_get_proc_soh(orte_proc_state_t *, int *, orte_process_name_t *);
static int orte_soh_bproc_set_proc_soh(orte_process_name_t *, orte_proc_state_t, int);
static int orte_soh_bproc_finalize(void);

/** 
 * Query the bproc node status
 */     
              
static int orte_soh_bproc_node_state(char *status)
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

    if (orte_soh_bproc_node_state(old->status) 
	    != orte_soh_bproc_node_state(new->status))
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
    char *node_name;
    char *user;
    char *group;
    struct passwd *pwd;
    struct group *grp;
    orte_gpr_value_t *value;

    cnt = num_bits(changes);

    /*
     * Check if there's anything to do
     */
    if (cnt == 0)
	return;

    value = OBJ_NEW(orte_gpr_value_t);
    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND;
    value->segment = strdup(ORTE_NODE_SEGMENT);
    
    value->cnt = cnt;

    value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));

    idx = 0;

    if (is_set(changes, BIT_NODE_STATE)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_NODE_STATE_KEY);
	value->keyvals[idx]->type = ORTE_NODE_STATE;
	value->keyvals[idx++]->value.node_state = orte_soh_bproc_node_state(ni->status);
    }

    if (is_set(changes, BIT_NODE_BPROC_STATUS)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_SOH_BPROC_NODE_STATUS);
	value->keyvals[idx]->type = ORTE_STRING;
	value->keyvals[idx++]->value.strptr = strdup(ni->status);
    }

    if (is_set(changes, BIT_NODE_BPROC_MODE)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_SOH_BPROC_NODE_MODE);
	value->keyvals[idx]->type = ORTE_UINT32;
	value->keyvals[idx++]->value.ui32 = ni->mode;
    }

    if (is_set(changes, BIT_NODE_BPROC_USER)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_SOH_BPROC_NODE_USER);
	value->keyvals[idx]->type = ORTE_STRING;
	if ((pwd = getpwuid(ni->user)))
	    user = strdup(pwd->pw_name);
	else
	    asprintf(&user, "%d\n", ni->user);
	value->keyvals[idx++]->value.strptr = user;
    }

    if (is_set(changes, BIT_NODE_BPROC_GROUP)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_SOH_BPROC_NODE_GROUP);
	value->keyvals[idx]->type = ORTE_STRING;
	if ((grp = getgrgid(ni->group)))
	    group = strdup(grp->gr_name);
	else
	    asprintf(&group, "%d\n", ni->group);
	value->keyvals[idx++]->value.strptr = group;
    }

    asprintf(&node_name, "%d", ni->node);

    if (is_set(changes, BIT_NODE_NAME)) {
	value->keyvals[idx] = OBJ_NEW(orte_gpr_keyval_t);
	value->keyvals[idx]->key = strdup(ORTE_NODE_NAME_KEY);
	value->keyvals[idx]->type = ORTE_STRING;
	value->keyvals[idx++]->value.strptr = strdup(node_name);
    }

    if (idx != cnt) {
    	opal_output(0, "soh_bproc: internal error %d != %d\n", idx, cnt);
	free(node_name);
	OBJ_RELEASE(value);
	opal_event_del(&mca_soh_bproc_component.notify_event);
	return;
    }

    ret = orte_schema.get_node_tokens(&value->tokens, &value->num_tokens, 
	    mca_soh_bproc_component.cellid, node_name);

    if (ret != ORTE_SUCCESS) {
	ORTE_ERROR_LOG(ret);
	OBJ_RELEASE(value);
	free(node_name);
	opal_event_del(&mca_soh_bproc_component.notify_event);
	return;
    }

    if (mca_soh_bproc_component.debug)
    	opal_output(0, "updating node %d\n", ni->node);

    if ((ret = orte_gpr.put(1, &value)) != ORTE_SUCCESS) {
	ORTE_ERROR_LOG(ret);
	opal_event_del(&mca_soh_bproc_component.notify_event);
    }

    free(node_name);
    OBJ_RELEASE(value);
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

	if (mca_soh_bproc_component.node_set.size > 0
		&& mca_soh_bproc_component.node_set.size == ns->size)
	    changes = find_changes(&mca_soh_bproc_component.node_set.node[i], ni);
	else
	    changes = BIT_SET_ALL;

	if (!empty_set(changes)) {
		update_registry(changes, ni);
		changed = 1;
	}
    }

    if (changed) {
	if (mca_soh_bproc_component.node_set.size != 0)
	    bproc_nodeset_free(&mca_soh_bproc_component.node_set);
	mca_soh_bproc_component.node_set = *ns;
    }

    return changed;
}

static void orte_soh_bproc_notify_handler(int fd, short flags, void *user)
{
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;

    if (bproc_nodelist_(&ns, fd) < 0) {
	/* bproc_nodelist_ error */
	opal_event_del(&mca_soh_bproc_component.notify_event);
	return;
    }

    if (!do_update(&ns))
	bproc_nodeset_free(&ns);
}

/**
 * Register a callback to receive BProc update notifications
 */
int orte_soh_bproc_module_init(void)
{
    int rc;
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;

    if (mca_soh_bproc_component.debug)
	    opal_output(0, "init soh_bproc_module\n");

    if (ORTE_SUCCESS != (rc = orte_ns.get_cellid(&mca_soh_bproc_component.cellid, orte_process_info.my_name))) {
	ORTE_ERROR_LOG(rc);
	return rc;
    }

    mca_soh_bproc_component.node_set.size = 0;

    /*
     * Set initial node status
     */

    if (bproc_nodelist(&ns) < 0)
    	return ORTE_ERROR;

    if (!do_update(&ns))
	bproc_nodeset_free(&ns);

    /*
     * Now regiser notify event
     */

    mca_soh_bproc_component.notify_fd = bproc_notifier();
    if (mca_soh_bproc_component.notify_fd < 0)
	return ORTE_ERROR;

    memset(&mca_soh_bproc_component.notify_event, 0, sizeof(opal_event_t));

    opal_event_set(
	&mca_soh_bproc_component.notify_event,
	mca_soh_bproc_component.notify_fd,
	OPAL_EV_READ|OPAL_EV_PERSIST,
	orte_soh_bproc_notify_handler,
	0);

    opal_event_add(&mca_soh_bproc_component.notify_event, 0);

    return ORTE_SUCCESS;
}

orte_soh_base_module_t orte_soh_bproc_module = {
    orte_soh_bproc_get_proc_soh,
    orte_soh_bproc_set_proc_soh,
    orte_soh_base_get_node_soh_not_available,
    orte_soh_base_set_node_soh_not_available,
    orte_soh_base_get_job_soh,
    orte_soh_base_set_job_soh,
    orte_soh_base_begin_monitoring_not_available,
    orte_soh_bproc_finalize
};

static int orte_soh_bproc_get_proc_soh(orte_proc_state_t *state, int *status, orte_process_name_t *proc)
{
    return orte_soh_base_get_proc_soh(state, status, proc);
}

static int orte_soh_bproc_set_proc_soh(orte_process_name_t *proc, orte_proc_state_t state, int status)
{
    return orte_soh_base_set_proc_soh(proc, state, status);
}

/**
 *  Cleanup
 */

int orte_soh_bproc_finalize(void)
{
    opal_event_del(&mca_soh_bproc_component.notify_event);
    return ORTE_SUCCESS;
}
