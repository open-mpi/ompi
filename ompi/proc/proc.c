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
#include "opal/util/output.h"
#include "opal/util/sys_info.h"
#include "orte/dps/dps.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/util/proc_info.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/convertor.h"

static opal_list_t  ompi_proc_list;
static opal_mutex_t ompi_proc_lock;
ompi_proc_t* ompi_proc_local_proc = NULL;

static void ompi_proc_construct(ompi_proc_t* proc);
static void ompi_proc_destruct(ompi_proc_t* proc);
static int setup_registry_callback(void);
static void callback(orte_gpr_notify_data_t *data, void *cbdata);

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

    proc->proc_flags = 0;

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_append(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    if (proc->proc_modex != NULL) {
        OBJ_RELEASE(proc->proc_modex);
    }
    OBJ_RELEASE( proc->proc_convertor );
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

    /* get all peers in this job */
    if(OMPI_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, &self))) {
        opal_output(0, "ompi_proc_init: get_peers failed with errno=%d", rc);
        return rc;
    }

    /* find self */
    for(i=0; i<npeers; i++) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_name = peers[i];
        if( i == self ) {
            ompi_proc_local_proc = proc;
            proc->proc_flags |= OMPI_PROC_FLAG_LOCAL;
        }
    }
    free(peers);

    /* setup registry callback to find everyone on my local node.
       Can't do a GPR get because we're in the middle of MPI_INIT,
       and we're setup for the GPR compound command -- so create a
       subscription which will be serviced later, at the end of the
       compound command. */
    if (ORTE_SUCCESS != (rc = setup_registry_callback())) {
        return rc;
    }

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
    ompi_proc_t **procs;
    ompi_proc_t *proc;
    size_t count = 0;
    orte_ns_cmp_bitmask_t mask;
    orte_process_name_t my_name;

    /* check bozo case */
    if (NULL == ompi_proc_local_proc) {
        return NULL;
    }
    mask = ORTE_NS_CMP_JOBID;
    my_name = ompi_proc_local_proc->proc_name;

    /* First count how many match this jobid */
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
         proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
         proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (0 == orte_ns.compare(mask, &proc->proc_name, &my_name)) {
            ++count;
        }
    }

    /* allocate an array */
    procs = (ompi_proc_t**) malloc(count * sizeof(ompi_proc_t*));
    if (NULL == procs) {
        return NULL;
    }

    /* now save only the procs that match this jobid */
    count = 0;
    for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
         proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
         proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (0 == orte_ns.compare(mask, &proc->proc_name, &my_name)) {
            procs[count++] = proc;
        }
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

    if (NULL == procs) {
        return NULL;
    }

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
    if (NULL == procs) {
        return NULL;
    }
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
        if (0 == orte_ns.compare(mask, &proc->proc_name, name)) {
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
        if (0 == orte_ns.compare(mask, &proc->proc_name, name)) {
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

    /* do not free plist *ever*, since it is used in the remote group
       structure of a communicator */
    plist = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if ( NULL == plist ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for ( i=0; i<proclistsize; i++ ){
        size_t count=1;
        int rc = orte_dps.unpack(buf, &name, &count, ORTE_NAME);
        if(rc != ORTE_SUCCESS) {
            return rc;
        }
        plist[i] = ompi_proc_find_and_add ( &name, &isnew );
        if(isnew) {
            MCA_PML_CALL(add_procs(&plist[i], 1));
        }
    }
    *proclist = plist;
    return OMPI_SUCCESS;
}


/*
 * As described above, we cannot do a simple GPR get because we're in
 * the middle of the GPR compound command in MPI_INIT.  So setup a
 * subscription that will be fullfilled later in MPI_INIT.
 */
static int setup_registry_callback(void)
{
    int rc;
    char *segment;
    ompi_proc_t *local = ompi_proc_local();
    orte_jobid_t jobid;
    orte_gpr_trigger_t trig, *trig1;
    orte_gpr_value_t value, *values;
    orte_gpr_subscription_t sub, *sub1;
    
    if (ORTE_SUCCESS != orte_ns.get_jobid(&jobid, &local->proc_name)) {
        printf("Badness!\n");
    }
    
    /* find the job segment on the registry */
    if (ORTE_SUCCESS != 
        (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        return rc;
    }

    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    /* indicate that this is a standard subscription. This indicates
       that the subscription will be common to all processes. Thus,
       the resulting data can be consolidated into a
       process-independent message and broadcast to all processes */
    if (ORTE_SUCCESS != 
        (rc = orte_schema.get_std_subscription_name(&(sub.name),
                                                    OMPI_PROC_SUBSCRIPTION, jobid))) {
        return rc;
    }

    /* send data when trigger fires, then delete - no need for further
       notifications */
    sub.action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    values = &value;
    sub.values = &values;
    sub.cnt = 1;
    
    value.addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    value.segment = segment;
    value.tokens = NULL; /* wildcard - look at all containers */
    value.num_tokens = 0;
    value.cnt = 2;
    value.keyvals = 
        (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*) * 2);
    if (NULL == value.keyvals) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    value.keyvals[0] = NULL;
    value.keyvals[1] = NULL;

    value.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    value.keyvals[0]->key = strdup(ORTE_PROC_NAME_KEY);
    if (NULL == value.keyvals[0]->key) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    value.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    value.keyvals[1]->key = strdup(ORTE_NODE_NAME_KEY);
    if (NULL == value.keyvals[0]->key) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    sub.cbfunc = callback;
    sub.user_tag = NULL;
    
    /* setup the trigger information */
    OBJ_CONSTRUCT(&trig, orte_gpr_trigger_t);
    if (ORTE_SUCCESS != 
        (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                               ORTE_STG1_TRIGGER, jobid))) {
        goto cleanup;
    }

    /* do the subscription */
    sub1 = &sub;
    trig1 = &trig;
    rc = orte_gpr.subscribe(1, &sub1, 1, &trig1);

 cleanup:
    OBJ_DESTRUCT(&value);
    sub.values = NULL;
    OBJ_DESTRUCT(&sub);
    OBJ_DESTRUCT(&trig);
    return rc;
}


/*
 * This callback is invoked by a subscription during MPI_INIT to let
 * us know what procs are on what hosts.  We look at the results and
 * figure out which procs are on the same host as the local proc.  For
 * each proc that is on the same host as the local proc, we set that
 * proc's OMPI_PROC_FLAG_LOCAL flag.
 */ 
static void callback(orte_gpr_notify_data_t *data, void *cbdata)
{
    size_t i, j, k;
    char *str;
    bool found_name;
    orte_ns_cmp_bitmask_t mask;
    orte_process_name_t name;
    orte_gpr_value_t **value;
    orte_gpr_keyval_t **keyval;
    ompi_proc_t *proc;

    /* check bozo case */
    if (0 == data->cnt) {
        return;
    }

    /* locks are probably not necessary here, but just be safe anyway */
    OPAL_THREAD_LOCK(&ompi_proc_lock);

    /* loop over the data returned in the subscription */
    mask = ORTE_NS_CMP_CELLID | ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    value = (orte_gpr_value_t**)(data->values)->addr;
    for (i = 0, k=0; k < data->cnt &&
                     i < (data->values)->size; ++i) {
        if (NULL != value[i]) {
            k++;
            str = NULL;
            found_name = false;
            keyval = value[i]->keyvals;
    
            /* find the 2 keys that we're looking for */
            for (j = 0; j < value[i]->cnt; ++j) {
                if (strcmp(keyval[j]->key, ORTE_PROC_NAME_KEY) == 0) {
                    orte_ns.get_proc_name_string(&str, &keyval[j]->value.proc);
                    name = keyval[j]->value.proc;
                    found_name = true;
                } else if (strcmp(keyval[j]->key, ORTE_NODE_NAME_KEY) == 0) {
                    if (NULL != str) {
                        free(str);
                    }
                    str = strdup(keyval[j]->value.strptr);
                }
            }
    
            /* if we found both keys and the proc is on my local host,
               find it in the master proc list and set the "local" flag */
            if (NULL != str && found_name &&
                0 == strcmp(str, orte_system_info.nodename)) {
                for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list); 
                     proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
                     proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
                    if (0 == orte_ns.compare(mask, &name, 
                                             &proc->proc_name)) {
                        proc->proc_flags |= OMPI_PROC_FLAG_LOCAL;
                    }
                }
            }
        }
    }

    /* unlock */
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}

