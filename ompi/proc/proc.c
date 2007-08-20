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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/runtime/ompi_module_exchange.h"

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
    orte_std_cntr_t i, npeers, datalen;
    void *data;
    orte_buffer_t* buf;    
    uint32_t ui32;
    int rc;

    OBJ_CONSTRUCT(&ompi_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&ompi_proc_lock, opal_mutex_t);

    /* create a shell of a proc structure for every proc in MPI_COMM_WORLD */
    if(ORTE_SUCCESS != (rc = orte_ns.get_peers(&peers, &npeers, NULL))) {
        opal_output(0, "ompi_proc_init: get_peers failed with errno=%d", rc);
        return rc;
    }
    for( i = 0; i < npeers; i++ ) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_name = peers[i];
        if( i == ORTE_PROC_MY_NAME->vpid ) {
            ompi_proc_local_proc = proc;
            proc->proc_flags |= OMPI_PROC_FLAG_LOCAL;
        }
    }
    free(peers);

    /* Fill in our local information */
    rc = ompi_arch_compute_local_id(&ui32);
    if (OMPI_SUCCESS != rc) return rc;

    ompi_proc_local_proc->proc_arch = ui32;
    ompi_proc_local_proc->proc_hostname = strdup(orte_system_info.nodename);

    /* pack our local data for others to use */
    buf = OBJ_NEW(orte_buffer_t);
    rc = ompi_proc_pack(&ompi_proc_local_proc, 1, buf);
    if (OMPI_SUCCESS != rc) return rc;

    /* send our data into the ether */
    rc = orte_dss.unload(buf, &data, &datalen);
    if (OMPI_SUCCESS != rc) return rc;
    OBJ_RELEASE(buf);

    rc = ompi_modex_send_string("ompi-proc-info", data, datalen);

    free(data);

    return rc;
}


int
ompi_proc_get_info(void)
{
    int ret = OMPI_SUCCESS;
    opal_list_item_t *item;

    OPAL_THREAD_LOCK(&ompi_proc_lock);

    for (item = opal_list_get_first(&ompi_proc_list) ;
         item != opal_list_get_end(&ompi_proc_list) ;
         item = opal_list_get_next(item)) {
        ompi_proc_t *proc = (ompi_proc_t*) item;
        orte_process_name_t name;
        uint32_t arch;
        char *hostname;
        orte_std_cntr_t count=1;
        void *data;
        size_t datalen;
        orte_buffer_t *buf;

        if (ORTE_EQUAL != orte_ns.compare_fields(ORTE_NS_CMP_JOBID,
                                                 &ompi_proc_local_proc->proc_name,
                                                 &proc->proc_name)) {
            /* not in our jobid -- this shouldn't happen */
            ret = OMPI_ERR_FATAL;
            goto out;
        }

        ret = ompi_modex_recv_string("ompi-proc-info", proc, &data, &datalen);
        if (OMPI_SUCCESS != ret)
            goto out;

        buf = OBJ_NEW(orte_buffer_t);
        ret = orte_dss.load(buf, data, datalen);
        if (OMPI_SUCCESS != ret)
            goto out;

        /* This isn't needed here, but packed just so that you could,
           in theory, use the unpack code on this proc.  We
           don't,because we aren't adding procs, but need to update
           them */
        ret = orte_dss.unpack(buf, &name, &count, ORTE_NAME);
        if (ret != ORTE_SUCCESS)
            goto out;

        ret = orte_dss.unpack(buf, &arch, &count, ORTE_UINT32);
        if (ret != ORTE_SUCCESS)
            goto out;
        ret = orte_dss.unpack(buf, &hostname, &count, ORTE_STRING);
        if (ret != ORTE_SUCCESS)
            goto out;

        proc->proc_arch = arch;

        /* if arch is different than mine, create a new convertor for this proc */
        if (proc->proc_arch != ompi_proc_local_proc->proc_arch) {
#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
            OBJ_RELEASE(proc->proc_convertor);
            proc->proc_convertor = ompi_convertor_create(proc->proc_arch, 0);
#else
            opal_show_help("help-mpi-runtime",
                           "heterogeneous-support-unavailable",
                           true, orte_system_info.nodename, 
                           hostname == NULL ? "<hostname unavailable>" :
                           hostname);
            ret = OMPI_ERR_NOT_SUPPORTED;
            goto out;
#endif
        } else if (0 == strcmp(hostname, orte_system_info.nodename)) {
            proc->proc_flags |= OMPI_PROC_FLAG_LOCAL;
        }

        /* Save the hostname */
        if (ompi_mpi_keep_peer_hostnames) {
            /* the dss code will have strdup'ed this for us -- no need
               to do so again */
            proc->proc_hostname = hostname;
        }

        /* Free the buffer for the next proc */
        OBJ_RELEASE(buf);
    }

out:
    OPAL_THREAD_UNLOCK(&ompi_proc_lock);
    return ret;
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
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
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
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
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
