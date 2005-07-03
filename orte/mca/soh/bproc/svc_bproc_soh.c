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

#include <sys/poll.h>
#include <sys/bproc.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "runtime/runtime.h"

#include "svc_bproc_soh.h"


mca_svc_base_module_t mca_svc_bproc_soh_module = {
    mca_svc_bproc_soh_module_init,
    mca_svc_bproc_soh_module_fini
};


/*
 * Add a BProc node to the virtual machine SOH segment
 */
int
mca_svc_bproc_soh_add_node(mca_ns_base_cellid_t cellid, int node)
{
    ompi_rte_vm_status_t *vmdata;
    int err;

    vmdata = (ompi_rte_vm_status_t*)malloc(sizeof(ompi_rte_vm_status_t));
    vmdata->cell = cellid;
    asprintf(&(vmdata->nodename), "%d", node);
    err = bproc_getnodeattr(ni->node, "cpus", &cpus, sizeof(cpus));
    if (err != 0)
        cpus = 1;
    vmdata->cpus = (uint16_t)cpus;
    
}

/**
 *  Process a BProc update notice
 */

int
mca_svc_bproc_soh_status_changed(struct bproc_node_info_t *old, struct bproc_node_info_t *new)
{
    if (old->node != new->node)
 return 0;
    if (strcmp(old->status, new->status))
    return 1;
    if (old->user != new->user)
  return 1;
    if (old->group != new->group)
    return 1;
    if (old->mode != new->mode)
  return 1;
    return 0;
}

void
mca_svc_bproc_soh_update_node_info(mca_ns_base_cellid_t cellid, struct bproc_node_info_t *ni)
{
    int err;
    int cpus;
    char *node;
    ompi_rte_vm_status__t *vmdata;

    asprintf(&node, "%d", ni->node);
    vmdata = ompi_rte_get_vm_status(cellid, node);
    if (vmdata == NULL) {  /* this node isn't present yet - add it */
        mca_svc_bproc_soh_add_node(cellid, ni->node);
    
        return;

    /* in long-term, we will store the soh data in key-value pairs. for now,
     * we store it simply as values so we can get it working - I will update
     * this later to the final form.
     */
    vmdata->user = ni->user;
    vmdata->group = ni->group;
    vmdata->mode = ni->mode;
    if (NULL != vmdata->status) {
        free(vmdata->status);
    }
    vmdata->status = strdup(ni->status);
    /*
    ompi_vm_status_data_add_int(vmdata, "user", ni->user);
    ompi_vm_status_data_add_int(vmdata, "group", ni->group);
    ompi_vm_status_data_add_int(vmdata, "mode", ni->mode);
    ompi_vm_status_data_add_string(vmdata, "status", ni->status);
    */

    /* probably should optimize this so it only happens once */
/*    ompi_vm_status_data_add_int(vmdata, "#cpus", cpus); */

    /* registry_put(segment, cell, node, vmdata); */

    free(node);
    ompit_vm_status_data_finish(vmdata);
}

void
mca_svc_bproc_soh_check_node_info(char *segment, char *cell, 
      struct bproc_node_set_t **old, 
        struct bproc_node_set_t *new)
{
    /* we assume the number of nodes does not change */
    for (i = 0; i < new->size; i++) {
      ni = &new->node[i];
    if (!old->size || status_changed((*old)->node[i], ni))
     update_node_info(segment, cell, ni);
    }

    if ((*old)->size)
  bproc_nodeset_free(*old);
    bproc_nodeset_init(*old, new->size);
    memcpy((*old)->node, new->node, sizeof(*new->node) * new->size);
}

#if OMPI_HAVE_POSIX_THREADS
static void *
mca_svc_bproc_soh_status_thread(opal_thread_t *thread)
{
    struct pollfd pfd;
    struct bproc_node_set_t ns = BPROC_EMPTY_NODESET;
    mca_svc_bproc_soh_module_t *module = (mca_svc_bproc_soh_module_t *)thread->t_arg;

    /* This thread enter in a cancel enabled state */
    pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, NULL );

    for (;;) {
 pfd.fd = module->notify_fd;
    pfd.events = POLLIN;
   res = poll(&pfd, 1, -1);
   if (res < 0) {
     /* poll error */
       break;
 }
  if (bproc_nodelist_(&ns, module->notify_fd) < 0) {
     /* bproc_nodelist_ error */
        break;
 }

 mca_svc_bproc_soh_check_node_info(module->segment, module->cell, &module->node_info, ns);

   bproc_nodeset_free(&ns);
    }

    return PTHREAD_CANCELED;
}
#endif  /* OMPI_HAVE_POSIX_THREADS */


/**
 * Register a callback to receive BProc update notifications
 */

int mca_svc_bproc_soh_module_init(mca_svc_base_module_t* base)
{
    int i;
    int num_nodes;
    bproc_node_set_t node_list;
    int node_num;
    char *segment, *jobid_string;
    mca_svc_bproc_soh_module_t *module /* = somthing */;
    
    jobid_string = ompi_name_server.get_jobid_string(ompi_rte_get_self());
    asprintf(&module->segment, "%s-bproc", OMPI_RTE_VM_STATUS_SEGMENT);
    module->cell = /* get cell somehow */;
         
    num_nodes = bproc_nodelist(&module->node_info);
    if (num_nodes < 0)
    return OMPI_ERROR;

    for (i = 0; i < module->node_info->size; i++) {
    update_node_info(&module->node_info[i]);
    }

    module->notify_fd = bproc_notifier();
    if (module->notify_fd < 0)
   return OMPI_ERROR;

    if (ompi_using_thread()) {
#if OMPI_HAVE_POSIX_THREADS
    module->thread.t_handle = 0;
    module->thread.t_run = (opal_thread_fn_t)mca_bproc_status_thread;
    module->thread.t_arg = (void *)module;
#endif  /* OMPI_HAVE_POSIX_THREADS */
    }

    return opal_thread_start(&module->thread);
}

/**
 *  Cleanup
 */

int mca_svc_bproc_soh_module_fini(mca_svc_base_module_t* base)
{
    mca_svc_bproc_soh_module_t *module /* = somthing */;

#if OMPI_HAVE_POSIX_THREADS
    if (module->thread.t_handle != 0) {
        void *thread_return;
        pthread_cancel(ptl->thread.t_handle);
        opal_thread_join(&(module->thread), &thread_return);
    }
#endif  /* OMPI_HAVE_POSIX_THREADS */
    return OMPI_SUCCESS;
}

