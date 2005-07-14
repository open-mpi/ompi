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

static int orte_soh_bproc_get_proc_soh(orte_proc_state_t *, int *, orte_process_name_t *);
static int orte_soh_bproc_set_proc_soh(orte_process_name_t *, orte_proc_state_t, int);
static int orte_soh_bproc_get_node_soh(orte_node_state_t *, orte_cellid_t, char *);
static int orte_soh_bproc_set_node_soh(orte_cellid_t, char *, orte_node_state_t);
static int orte_soh_bproc_finalize(void);
static void update_node_info(struct bproc_node_info_t *);

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

/**
 *  Process a BProc update notice
 */

static void update_registry(struct bproc_node_info_t *ni)
{
    int ret;
    char *node_name;
    char *user;
    char *group;
    struct passwd *pwd;
    struct group *grp;
    orte_gpr_value_t *value;

    if (mca_soh_bproc_component.debug)
    	opal_output(0, "updating node %d\n", ni->node);

    value = OBJ_NEW(orte_gpr_value_t);
    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND;
    value->segment = strdup(ORTE_NODE_SEGMENT);

    value->cnt = 5;
    value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt*sizeof(orte_gpr_keyval_t*));
    
    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    value->keyvals[0]->key = strdup(ORTE_NODE_STATE_KEY);
    value->keyvals[0]->type = ORTE_NODE_STATE;
    value->keyvals[0]->value.node_state = orte_soh_bproc_node_state(ni->status);

    value->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    value->keyvals[1]->key = strdup(ORTE_SOH_BPROC_NODE_MODE);
    value->keyvals[1]->type = ORTE_UINT32;
    value->keyvals[1]->value.ui32 = ni->mode;

    value->keyvals[2] = OBJ_NEW(orte_gpr_keyval_t);
    value->keyvals[2]->key = strdup(ORTE_SOH_BPROC_NODE_USER);
    value->keyvals[2]->type = ORTE_STRING;
    if ((pwd = getpwuid(ni->user)))
	user = strdup(pwd->pw_name);
    else
	asprintf(&user, "%d\n", ni->user);
    value->keyvals[2]->value.strptr = user;


    value->keyvals[3] = OBJ_NEW(orte_gpr_keyval_t);
    value->keyvals[3]->key = strdup(ORTE_SOH_BPROC_NODE_GROUP);
    value->keyvals[3]->type = ORTE_STRING;
    if ((grp = getgrgid(ni->group)))
	group = strdup(grp->gr_name);
    else
	asprintf(&group, "%d\n", ni->group);
    value->keyvals[3]->value.strptr = group;

    asprintf(&node_name, "%d", ni->node);

    value->keyvals[4] = OBJ_NEW(orte_gpr_keyval_t);
    value->keyvals[4]->key = strdup(ORTE_NODE_NAME_KEY);
    value->keyvals[4]->type = ORTE_STRING;
    value->keyvals[4]->value.strptr = strdup(node_name);

    ret = orte_schema.get_node_tokens(&value->tokens, &value->num_tokens, 
	    mca_soh_bproc_component.cellid, node_name);

    if (ret != ORTE_SUCCESS) {
	ORTE_ERROR_LOG(ret);
	OBJ_RELEASE(value);
	return;
    }

    if ((ret = orte_gpr.put(1, &value)) != ORTE_SUCCESS)
	ORTE_ERROR_LOG(ret);

    free(node_name);
    OBJ_RELEASE(value);
}

static int node_changed(struct bproc_node_info_t *old, struct bproc_node_info_t *new)
{
    int res;

    if (old->node != new->node) {
	res = 1;
    }

    if (strcmp(old->status, new->status) != 0) {
	res = 1;
    }

    if (old->user != new->user) {
	res = 1;
    }

    if (old->group != new->group) {
	res = 1;
    }

    if (old->mode != new->mode) {
	res = 1;
    }

    return res;
}

static int do_update(struct bproc_node_set_t *ns)
{
    int i;
    int changed = 0;
    struct bproc_node_info_t *ni;

    /* we assume the number of nodes does not change */
    for (i = 0; i < ns->size; i++) {
	ni = &ns->node[i];

	if (mca_soh_bproc_component.node_set.size == 0
	    || mca_soh_bproc_component.node_set.size != ns->size
	    || node_changed(&mca_soh_bproc_component.node_set.node[i], ni)) {
	    update_registry(ni);
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
