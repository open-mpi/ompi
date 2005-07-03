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

#include <string.h>

#include "opal/threads/mutex.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "dps/dps.h"
#include "proc/proc.h"
#include "mca/oob/oob.h"
#include "mca/ns/ns.h"
#include "mca/pml/pml.h"
#include "datatype/convertor.h"

static opal_list_t  ompi_proc_list;
static opal_mutex_t ompi_proc_lock;
ompi_proc_t* ompi_proc_local_proc = NULL;

static void ompi_proc_construct(ompi_proc_t* proc);
static void ompi_proc_destruct(ompi_proc_t* proc);


OBJ_CLASS_INSTANCE(
    ompi_proc_t, 
    opal_list_item_t,
    ompi_proc_construct, 
    ompi_proc_destruct
);


void ompi_proc_construct(ompi_proc_t* proc)
{
    proc->proc_pml = NULL;
    proc->proc_modex = NULL;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);

    /* FIX - need to determine remote process architecture */
    proc->proc_convertor = ompi_convertor_create(0, 0);
    proc->proc_arch = 0;

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_append(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    if(proc->proc_modex != NULL)
        OBJ_RELEASE(proc->proc_modex);
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_remove_item(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    OBJ_DESTRUCT(&proc->proc_lock);
}


int ompi_proc_init(void)
{
    orte_process_name_t *peers;
    size_t i, npeers, self;
    int rc;

    OBJ_CONSTRUCT(&ompi_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&ompi_proc_lock, opal_mutex_t);

    if(OMPI_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, &self))) {
        ompi_output(0, "ompi_proc_init: get_peers failed with errno=%d", rc);
        return rc;
    }

    for(i=0; i<npeers; i++) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_name = peers[i];
        if( i == self ) {
            ompi_proc_local_proc = proc;
        }
    }
    free(peers);
    return OMPI_SUCCESS;
}

int ompi_proc_finalize (void)
{
    ompi_proc_t *proc, *nextproc, *endproc;

    proc      = (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
    nextproc  = (ompi_proc_t*)opal_list_get_next(proc);
    endproc   = (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);

    OBJ_RELEASE(proc);
    while ( nextproc != endproc ) {
	proc = nextproc;
	nextproc = (ompi_proc_t *)opal_list_get_next(proc);
	OBJ_RELEASE(proc);
    }
    OBJ_DESTRUCT(&ompi_proc_list);

    return OMPI_SUCCESS;
}

ompi_proc_t** ompi_proc_world(size_t *size)
{
    ompi_proc_t **procs = 
        (ompi_proc_t**) malloc(opal_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    /* return only the procs that match this jobid */
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        /* TSW - FIX */
        procs[count++] = proc;
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_all(size_t* size)
{
    ompi_proc_t **procs = 
        (ompi_proc_t**) malloc(opal_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_self(size_t* size)
{
    ompi_proc_t **procs = (ompi_proc_t**) malloc(sizeof(ompi_proc_t*));
    if(NULL == procs)
        return NULL;
    OBJ_RETAIN(ompi_proc_local_proc);
    *procs = ompi_proc_local_proc;
    *size = 1;
    return procs;
}

ompi_proc_t * ompi_proc_find ( const orte_process_name_t * name )
{
    ompi_proc_t *proc, *rproc=NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */

    mask = ORTE_NS_CMP_CELLID | ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (orte_ns.compare(mask, &proc->proc_name, name) == 0) {
            rproc = proc;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return rproc;
}


ompi_proc_t * ompi_proc_find_and_add ( const orte_process_name_t * name, bool* isnew )
{
    ompi_proc_t *proc, *rproc=NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_CELLID | ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if(orte_ns.compare(mask, &proc->proc_name, name) == 0) {
            *isnew = false;
            rproc = proc;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    if ( NULL == rproc ) {
	    ompi_proc_t *tproc = OBJ_NEW(ompi_proc_t);
	    rproc = tproc;
	    rproc->proc_name = *name;
        *isnew = true;
    }
    return rproc;
}

int ompi_proc_get_namebuf ( ompi_proc_t **proclist, int proclistsize, orte_buffer_t* buf)
{
    int i;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for (i=0; i<proclistsize; i++) {
        int rc = orte_dps.pack(buf, &(proclist[i]->proc_name), 1, ORTE_NAME);
        if(rc != OMPI_SUCCESS) {
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return OMPI_SUCCESS;
}


int ompi_proc_get_proclist (orte_buffer_t* buf, int proclistsize, ompi_proc_t ***proclist)
{
    int i;
    ompi_proc_t **plist=NULL;
    orte_process_name_t name;
    bool isnew = false;

    /* do not free plist *ever*, since it is used in the remote group structure
       of a communicator */
    plist = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if ( NULL == plist ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for ( i=0; i<proclistsize; i++ ){
        size_t count=1;
	    int rc = orte_dps.unpack(buf, &name, &count, ORTE_NAME);
        if(rc != OMPI_SUCCESS) 
            return rc;
        plist[i] = ompi_proc_find_and_add ( &name, &isnew );
        if(isnew) {
            MCA_PML_CALL(add_procs(&plist[i], 1));
        }
    }
    *proclist = plist;
    return OMPI_SUCCESS;
}

