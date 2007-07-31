/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/util/show_help.h"
#include "orte/util/sys_info.h"
#include "orte/dss/dss.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/dt_arch.h"
#include "ompi/datatype/convertor.h"
#include "ompi/runtime/params.h"
#include "ompi/runtime/mpiruntime.h"

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
    proc->proc_bml = NULL;
    proc->proc_pml = NULL;
    proc->proc_modex = NULL;
    OBJ_CONSTRUCT(&proc->proc_lock, opal_mutex_t);

    /* By default all processors are supposelly having the same architecture as me. Thus,
     * by default we run in a homogeneous environment. Later when the registry callback
     * get fired we will have to set the convertors to the correct architecture.
     */
    proc->proc_convertor = ompi_mpi_local_convertor;
    OBJ_RETAIN( ompi_mpi_local_convertor );
    proc->proc_arch = ompi_mpi_local_arch;

    proc->proc_flags = 0;

    /* By default, put NULL in the hostname.  It may or may not get
       filled in later -- consumer of this field beware! */
    proc->proc_hostname = NULL;

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_append(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    if (proc->proc_modex != NULL) {
        OBJ_RELEASE(proc->proc_modex);
    }
    /* As all the convertors are created with OBJ_NEW we can just call OBJ_RELEASE. All, except
     * the local convertor, will get destroyed at some point here. If the reference count is correct
     * the local convertor (who has the reference count increased in the datatype) will not get
     * destroyed here. It will be destroyed later when the ompi_ddt_finalize is called.
     */
    OBJ_RELEASE( proc->proc_convertor );
    if (NULL != proc->proc_hostname) {
        free(proc->proc_hostname);
    }
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    opal_list_remove_item(&ompi_proc_list, (opal_list_item_t*)proc);
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    OBJ_DESTRUCT(&proc->proc_lock);
}


int ompi_proc_init(void)
{
    orte_process_name_t *peers;
    orte_std_cntr_t i, npeers, num_tokens;
    orte_jobid_t jobid;
    char *segment, **tokens;
    orte_data_value_t value = { {OBJ_CLASS(orte_data_value_t),0}, ORTE_NULL, NULL};
    uint32_t ui32;
    int rc;

    OBJ_CONSTRUCT(&ompi_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&ompi_proc_lock, opal_mutex_t);

    /* get all peers in this job */
    if(ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, NULL))) {
        opal_output(0, "ompi_proc_init: get_peers failed with errno=%d", rc);
        return rc;
    }

    /* find self */
    for( i = 0; i < npeers; i++ ) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_name = peers[i];
        if(peers[i].vpid == ORTE_PROC_MY_NAME->vpid ) {
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

    /* Here we have to add to the GPR the information about the current architecture.
     */
    if (OMPI_SUCCESS != (rc = ompi_arch_compute_local_id(&ui32))) {
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_dss.set(&value, &ui32, ORTE_UINT32))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    jobid = ORTE_PROC_MY_NAME->jobid;

    /* find the job segment on the registry */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        return rc;
    }

    /* get the registry tokens for this node */
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens,
                                orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }

    /* put the arch info on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                             segment, tokens,
                                             OMPI_PROC_ARCH, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    free(segment);
    for (i=0; i < num_tokens; i++) {
        free(tokens[i]);
        tokens[i] = NULL;
    }
    if (NULL != tokens) free(tokens);

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
        if (ORTE_EQUAL == orte_ns.compare_fields(mask, &proc->proc_name, &my_name)) {
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
        if (ORTE_EQUAL == orte_ns.compare_fields(mask, &proc->proc_name, &my_name)) {
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
        if (ORTE_EQUAL == orte_ns.compare_fields(mask, &proc->proc_name, name)) {
            rproc = proc;
            break;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    return rproc;
}


static ompi_proc_t *
ompi_proc_find_and_add(const orte_process_name_t * name, bool* isnew)
{
    ompi_proc_t *proc, *rproc = NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_CELLID | ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
        proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)opal_list_get_next(proc)) {
        if (ORTE_EQUAL == orte_ns.compare_fields(mask, &proc->proc_name, name)) {
            rproc = proc;
            *isnew = false;
            break;
        }
    }

    if (NULL == rproc) {
        *isnew = true;
        rproc = OBJ_NEW(ompi_proc_t);
        if (NULL != rproc) {
            rproc->proc_name = *name;
        }
        /* caller had better fill in the rest of the proc, or there's
           going to be pain later... */
    }

    OPAL_THREAD_UNLOCK(&ompi_proc_lock);

    return rproc;
}


int
ompi_proc_pack(ompi_proc_t **proclist, int proclistsize, orte_buffer_t* buf)
{
    int i, rc;

    OPAL_THREAD_LOCK(&ompi_proc_lock);
    for (i=0; i<proclistsize; i++) {
        rc = orte_dss.pack(buf, &(proclist[i]->proc_name), 1, ORTE_NAME);
        if(rc != ORTE_SUCCESS) {
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
        rc = orte_dss.pack(buf, &(proclist[i]->proc_arch), 1, ORTE_UINT32);
        if(rc != ORTE_SUCCESS) {
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
        rc = orte_dss.pack(buf, &(proclist[i]->proc_hostname), 1, ORTE_STRING);
        if(rc != ORTE_SUCCESS) {
            OPAL_THREAD_UNLOCK(&ompi_proc_lock);
            return rc;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return OMPI_SUCCESS;
}


int
ompi_proc_unpack(orte_buffer_t* buf, int proclistsize, ompi_proc_t ***proclist)
{
    int i;
    size_t newprocs_len = 0;
    ompi_proc_t **plist=NULL, **newprocs = NULL;

    /* do not free plist *ever*, since it is used in the remote group
       structure of a communicator */
    plist = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if ( NULL == plist ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* free this on the way out */
    newprocs = (ompi_proc_t **) calloc (proclistsize, sizeof (ompi_proc_t *));
    if (NULL == newprocs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for ( i=0; i<proclistsize; i++ ){
        orte_std_cntr_t count=1;
        orte_process_name_t new_name;
        uint32_t new_arch;
        char *new_hostname;
        bool isnew = false;
        int rc;

        rc = orte_dss.unpack(buf, &new_name, &count, ORTE_NAME);
        if (rc != ORTE_SUCCESS) {
            return rc;
        }
        rc = orte_dss.unpack(buf, &new_arch, &count, ORTE_UINT32);
        if (rc != ORTE_SUCCESS) {
            return rc;
        }
        rc = orte_dss.unpack(buf, &new_hostname, &count, ORTE_STRING);
        if (rc != ORTE_SUCCESS) {
            return rc;
        }

        plist[i] = ompi_proc_find_and_add(&new_name, &isnew);
        if (isnew) {
            newprocs[newprocs_len++] = plist[i];

            plist[i]->proc_arch = new_arch;

            /* if arch is different than mine, create a new convertor for this proc */
            if (plist[i]->proc_arch != ompi_mpi_local_arch) {
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(plist[i]->proc_convertor);
                plist[i]->proc_convertor = ompi_convertor_create(plist[i]->proc_arch, 0);
#else
                opal_show_help("help-mpi-runtime",
                               "heterogeneous-support-unavailable",
                               true, orte_system_info.nodename, 
                               new_hostname == NULL ? "<hostname unavailable>" :
                                   new_hostname);
                return OMPI_ERR_NOT_SUPPORTED;
#endif
            }

            /* Save the hostname */
            if (ompi_mpi_keep_peer_hostnames) {
                plist[i]->proc_hostname = new_hostname;
            }
        }
    }

    if (newprocs_len > 0) MCA_PML_CALL(add_procs(newprocs, newprocs_len));
    if (newprocs != NULL) free(newprocs);

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
    char *segment, *sub_name, *trig_name, *keys[3];
    ompi_proc_t *local = ompi_proc_local();
    orte_gpr_subscription_id_t id;
    orte_jobid_t jobid;

    jobid = local->proc_name.jobid;

    /* find the job segment on the registry */
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* indicate that this is a standard subscription. This indicates
       that the subscription will be common to all processes. Thus,
       the resulting data can be consolidated into a
       process-independent message and broadcast to all processes */
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_std_subscription_name(&sub_name,
                                                    OMPI_PROC_SUBSCRIPTION, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }

    /* define the keys to be returned */
    keys[0] = strdup(ORTE_PROC_NAME_KEY);
    keys[1] = strdup(ORTE_NODE_NAME_KEY);
    keys[2] = strdup(OMPI_PROC_ARCH);

    /* Here we have to add another key to the registry to be able to get the information
     * about the remote architectures.
     * TODO: George.
     */

    /* attach ourselves to the standard stage-1 trigger */
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_std_trigger_name(&trig_name,
                                               ORTE_STG1_TRIGGER, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_N(&id, trig_name, sub_name,
                                ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG,
                                ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR | ORTE_GPR_STRIPPED,
                                segment,
                                NULL,  /* wildcard - look at all containers */
                                3, keys,
                                callback, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    free(trig_name);

CLEANUP:
    free(segment);
    free(sub_name);
    free(keys[0]);
    free(keys[1]);
    free(keys[2]);

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
    orte_std_cntr_t i, j, k;
    char *str = NULL;
    uint32_t arch = 0, *ui32;
    bool found_name, found_arch;
    orte_ns_cmp_bitmask_t mask;
    orte_process_name_t name, *nptr;
    orte_gpr_value_t **value;
    orte_gpr_keyval_t **keyval;
    ompi_proc_t *proc;
    int rc;

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
            found_arch = false;
            keyval = value[i]->keyvals;

            /* find the 2 keys that we're looking for */
            for (j = 0; j < value[i]->cnt; ++j) {
                if (strcmp(keyval[j]->key, ORTE_PROC_NAME_KEY) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&nptr, keyval[j]->value, ORTE_NAME))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    orte_ns.get_proc_name_string(&str, nptr);
                    name = *nptr;
                    found_name = true;
                } else if (strcmp(keyval[j]->key, ORTE_NODE_NAME_KEY) == 0) {
                    if (NULL != str) {
                        free(str);
                    }
                    str = strdup((const char*)keyval[j]->value->data);
                } else if (strcmp(keyval[j]->key, OMPI_PROC_ARCH) == 0) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&ui32, keyval[j]->value, ORTE_UINT32))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                    arch = *ui32;
                    found_arch = true;
                }
            }

            /* if we found all keys and the proc is on my local host,
               find it in the master proc list and set the "local" flag */
            if (NULL != str && found_name && found_arch) {
                for (proc =  (ompi_proc_t*)opal_list_get_first(&ompi_proc_list);
                     proc != (ompi_proc_t*)opal_list_get_end(&ompi_proc_list);
                     proc =  (ompi_proc_t*)opal_list_get_next(proc)) {

                    /* find the associated proc entry and update its
                       arch flag.  If the nodename of this info is
                       my local host, also set the LOCAL flag. */
                    if (ORTE_EQUAL == orte_ns.compare_fields(mask, &name, &proc->proc_name)) {
                        proc->proc_arch = arch;
                        if (0 == strcmp(str, orte_system_info.nodename)) {
                            proc->proc_flags |= OMPI_PROC_FLAG_LOCAL;
                        }

                        /* if arch is different than mine, create a
                           new convertor for this proc in
                           heterogeneous mode or abort in
                           non-heterogeneous mode. */
                        if (proc->proc_arch != ompi_mpi_local_arch) {
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
                            OBJ_RELEASE(proc->proc_convertor);
                            proc->proc_convertor = ompi_convertor_create(proc->proc_arch, 0);
#else
                            opal_show_help("help-mpi-runtime",
                                           "proc:heterogeneous-support-unavailable",
                                           true, orte_system_info.nodename, str);
                            /* we can't return an error, so abort. */
                            ompi_mpi_abort(MPI_COMM_WORLD, OMPI_ERR_NOT_SUPPORTED, false);
#endif
                        }

                        /* Save the hostname */
                        if (ompi_mpi_keep_peer_hostnames && NULL == proc->proc_hostname) {
                            proc->proc_hostname = str;
                            str = NULL;
                        }
                    }
                }
            }
        }
    }

    if (NULL != str) {
        free(str);
    }

    /* unlock */
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
}

