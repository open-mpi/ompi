/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2017 Mellanox Technologies. All rights reserved.
 *
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "ompi_config.h"

#include <string.h>
#include <strings.h>

#include "ompi/constants.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/threads/mutex.h"
#include "opal/util/arch.h"
#include "opal/util/show_help.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/argv.h"

#include "ompi/proc/proc.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/ompi_modex.h"
#include "ompi/runtime/params.h"
#include "ompi/mca/pml/pml.h"

opal_list_t  ompi_proc_list = {{0}};
static opal_mutex_t ompi_proc_lock;
static opal_hash_table_t ompi_proc_hash;
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
/* Serializes the architecture (and therefore convertor) seeding, which
 * can now happen from any thread that first talks to a peer. It cannot
 * be ompi_proc_lock: that one is held across complete_init_single. */
static opal_mutex_t ompi_proc_arch_lock = OPAL_MUTEX_STATIC_INIT;
#endif

ompi_proc_t* ompi_proc_local_proc = NULL;

static void ompi_proc_construct(ompi_proc_t* proc);
static void ompi_proc_destruct(ompi_proc_t* proc);
static ompi_proc_t *ompi_proc_for_name_nolock (const opal_process_name_t proc_name);

OBJ_CLASS_INSTANCE(
    ompi_proc_t,
    opal_proc_t,
    ompi_proc_construct,
    ompi_proc_destruct
);


void ompi_proc_construct(ompi_proc_t* proc)
{
#if OPAL_ENABLE_FT_MPI
    proc->proc_active = true;
#endif
    bzero(proc->proc_endpoints, sizeof(proc->proc_endpoints));

    /* By default all processors are supposedly having the same architecture as me. Thus,
     * by default we run in a homogeneous environment. Later, when the RTE can tell us
     * the arch of the remote nodes, we will have to set the convertors to the correct
     * architecture.
     */
    OBJ_RETAIN( ompi_mpi_local_convertor );
    proc->super.proc_convertor = ompi_mpi_local_convertor;
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    /* As all the convertors are created with OBJ_NEW we can just call OBJ_RELEASE. All, except
     * the local convertor, will get destroyed at some point here. If the reference count is correct
     * the local convertor (who has the reference count increased in the datatype) will not get
     * destroyed here. It will be destroyed later when the ompi_datatype_finalize is called.
     */
    OBJ_RELEASE( proc->super.proc_convertor );
    opal_mutex_lock (&ompi_proc_lock);
    opal_list_remove_item(&ompi_proc_list, (opal_list_item_t*)proc);
    opal_hash_table_remove_value_ptr (&ompi_proc_hash, &proc->super.proc_name, sizeof (proc->super.proc_name));
    opal_mutex_unlock (&ompi_proc_lock);
}

/**
 * Allocate a new ompi_proc_T for the given jobid/vpid
 *
 * @param[in]  jobid Job identifier
 * @param[in]  vpid  Process identifier
 * @param[out] procp New ompi_proc_t structure
 *
 * This function allocates a new ompi_proc_t and inserts it into
 * the process list and hash table.
 */
static int ompi_proc_allocate (ompi_jobid_t jobid, ompi_vpid_t vpid, ompi_proc_t **procp) {
    ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);

    opal_list_append(&ompi_proc_list, (opal_list_item_t*)proc);

    OMPI_CAST_RTE_NAME(&proc->super.proc_name)->jobid = jobid;
    OMPI_CAST_RTE_NAME(&proc->super.proc_name)->vpid = vpid;

    opal_hash_table_set_value_ptr (&ompi_proc_hash, &proc->super.proc_name, sizeof (proc->super.proc_name),
                                   proc);

    /* by default we consider process to be remote */
    proc->super.proc_flags = OPAL_PROC_NON_LOCAL;

    /* Built after the exchange delivered, so what this peer published is
     * local already; its architecture still has to be read out of it. */
    if (ompi_modex_all_ready()) {
        opal_proc_learned(&proc->super, OPAL_PROC_FLAG_AVAILABLE);
    }

    *procp = proc;

    return OMPI_SUCCESS;
}

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
/**
 * Read this peer's architecture from the modex and, if it differs from
 * ours, give the proc a matching convertor.
 *
 * Must be called with ompi_proc_arch_lock held: seeding twice would
 * leak a convertor and drop a reference we no longer own.
 */
static int ompi_proc_seed_arch (ompi_proc_t *proc)
{
    /* if the proc is local, then no need to fetch it */
    if (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)) {
        proc->super.proc_arch = opal_local_arch;
    } else {
        uint32_t *ui32ptr = &(proc->super.proc_arch);
        int ret;

        OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, "OMPI_ARCH", &proc->super.proc_name,
                                       (void**)&ui32ptr, PMIX_UINT32);
        if (OPAL_SUCCESS == ret) {
            /* if arch is different than mine, create a new convertor for this proc */
            if (proc->super.proc_arch != opal_local_arch) {
                OBJ_RELEASE(proc->super.proc_convertor);
                proc->super.proc_convertor = opal_convertor_create(proc->super.proc_arch, 0);
            }
        } else if (OMPI_ERR_NOT_IMPLEMENTED == ret || ompi_modex_proc_ready(proc)) {
            /* Either the runtime does not carry that key at all, or the
             * peer's data is local and holds no architecture: it was
             * built without heterogeneous support, so it can only be
             * running the same architecture as we are. */
            proc->super.proc_arch = opal_local_arch;
        } else {
            /* The peer has not published yet. Leaving proc_arch alone
             * keeps proc_convertor at the local one, which would
             * silently mistranslate every message to and from that
             * peer, so the caller has to come back instead. */
            return OMPI_ERR_NOT_READY;
        }
    }

    /* The convertor has to be in place before the flag that announces
     * it: readers test the flag without the lock. */
    opal_atomic_wmb();
    opal_proc_learned(&proc->super, OPAL_PROC_FLAG_INITIALIZED);

    return OMPI_SUCCESS;
}
#endif  /* OPAL_ENABLE_HETEROGENEOUS_SUPPORT */

/**
 * Finish setting up an ompi_proc_t
 *
 * @param[in] proc ompi process structure
 *
 * This function contains the core code of ompi_proc_complete_init() and
 * ompi_proc_refresh(). The tasks performed by this function include
 * retrieving the hostname (if below the modex cutoff), determining the
 * remote architecture, and calculating the locality of the process.
 */
int ompi_proc_complete_init_single (ompi_proc_t *proc)
{
    if ((OMPI_CAST_RTE_NAME(&proc->super.proc_name)->jobid == OMPI_PROC_MY_NAME->jobid) &&
        (OMPI_CAST_RTE_NAME(&proc->super.proc_name)->vpid  == OMPI_PROC_MY_NAME->vpid)) {
        /* nothing else to do, our own architecture is the local one, and
         * what we published we can read */
        opal_proc_learned(&proc->super,
                          OPAL_PROC_FLAG_AVAILABLE | OPAL_PROC_FLAG_INITIALIZED);
        return OMPI_SUCCESS;
    }

    if (OPAL_PROC_NON_LOCAL == proc->super.proc_flags ||
        0 == proc->super.proc_flags) {
        uint16_t u16, *u16ptr = &u16;
        int loc_ret;
        OPAL_MODEX_RECV_VALUE_OPTIONAL(loc_ret, PMIX_LOCALITY, &proc->super.proc_name,
                                       &u16ptr, PMIX_UINT16);
        if (OPAL_SUCCESS == loc_ret) {
            proc->super.proc_flags = u16;
        }
        /* A miss is final, and there is nothing to come back for, unlike
         * the architecture read below. This key is not something a peer
         * publishes and we then wait for: ompi_rte_init() computes it
         * locally for every name in PMIX_LOCAL_PEERS and stores it with
         * PMIx_Store_internal() before any proc exists, failing init
         * outright if that store fails. So it is present for a
         * node-local peer of ours and absent for every other, which is
         * exactly what the OPAL_PROC_NON_LOCAL default already says.
         *
         * Nor could the status say otherwise if it wanted to:
         * OPAL_MODEX_RECV_VALUE_OPTIONAL is one of the macros that hands
         * back what PMIx said, ungated on whether the peer's data has
         * arrived, so a miss is PMIX_ERR_NOT_FOUND -- which is -46, the
         * number OPAL gives OPAL_ERR_TAKE_NEXT_OPTION. */
    }

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    /* Get the remote architecture - this might force a modex except for
     * those environments where the RM provides it. It selects the
     * convertor used for that peer, so it is read once and only once. */
    if (!opal_proc_known(&proc->super, OPAL_PROC_FLAG_INITIALIZED)) {
        int ret = OMPI_SUCCESS;

        opal_mutex_lock (&ompi_proc_arch_lock);
        if (!opal_proc_known(&proc->super, OPAL_PROC_FLAG_INITIALIZED)) {
            ret = ompi_proc_seed_arch (proc);
        }
        opal_mutex_unlock (&ompi_proc_arch_lock);

        if (OMPI_SUCCESS != ret) {
            return ret;
        }
    }
#else
    /* must be same arch as my own */
    proc->super.proc_arch = opal_local_arch;
    opal_proc_learned(&proc->super, OPAL_PROC_FLAG_INITIALIZED);
#endif

    return OMPI_SUCCESS;
}

opal_proc_t *ompi_proc_lookup (const opal_process_name_t proc_name)
{
    ompi_proc_t *proc = NULL;
    int ret;

    /* try to lookup the value in the hash table */
    ret = opal_hash_table_get_value_ptr (&ompi_proc_hash, &proc_name, sizeof (proc_name), (void **) &proc);

    if (OPAL_SUCCESS == ret) {
        return &proc->super;
    }

    return NULL;
}

static ompi_proc_t *ompi_proc_for_name_nolock (const opal_process_name_t proc_name)
{
    ompi_proc_t *proc = NULL;
    int ret;

    /* double-check that another competing thread has not added this proc */
    ret = opal_hash_table_get_value_ptr (&ompi_proc_hash, &proc_name, sizeof (proc_name), (void **) &proc);
    if (OPAL_SUCCESS == ret) {
        goto exit;
    }

    /* allocate a new ompi_proc_t object for the process and insert it into the process table */
    ret = ompi_proc_allocate (proc_name.jobid, proc_name.vpid, &proc);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        /* allocation fail */
        goto exit;
    }

    /* Leave the proc as a skeleton (name + default convertor). Arch,
     * locality and convertor are filled in when the PML constructs the
     * BML endpoint, after the peer blob is local. */
exit:
    return proc;
}

opal_proc_t *ompi_proc_for_name (const opal_process_name_t proc_name)
{
    ompi_proc_t *proc = NULL;
    int ret;

    /* try to lookup the value in the hash table */
    ret = opal_hash_table_get_value_ptr (&ompi_proc_hash, &proc_name, sizeof (proc_name), (void **) &proc);
    if (OPAL_SUCCESS == ret) {
        return &proc->super;
    }

    opal_mutex_lock (&ompi_proc_lock);
    proc = ompi_proc_for_name_nolock (proc_name);
    opal_mutex_unlock (&ompi_proc_lock);

    return (opal_proc_t *) proc;
}

int ompi_proc_init(void)
{
    int opal_proc_hash_init_size = (ompi_process_info.num_procs < ompi_add_procs_cutoff) ? ompi_process_info.num_procs :
        1024;
    ompi_proc_t *proc;
    int ret;

    OBJ_CONSTRUCT(&ompi_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&ompi_proc_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ompi_proc_hash, opal_hash_table_t);

    ret = opal_hash_table_init (&ompi_proc_hash, opal_proc_hash_init_size);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* create a proc for the local process */
    ret = ompi_proc_allocate (OMPI_PROC_MY_NAME->jobid, OMPI_PROC_MY_NAME->vpid, &proc);
    if (OMPI_SUCCESS != ret) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* set local process data */
    ompi_proc_local_proc = proc;
    proc->super.proc_flags = OPAL_PROC_ALL_LOCAL;
    proc->super.proc_arch = opal_local_arch;
    opal_proc_learned(&proc->super,
                      OPAL_PROC_FLAG_AVAILABLE | OPAL_PROC_FLAG_INITIALIZED);
    /* Register the local proc with OPAL */
    opal_proc_local_set(&proc->super);
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    /* add our arch to the modex */
    OPAL_MODEX_SEND_VALUE(ret, PMIX_GLOBAL,
                          "OMPI_ARCH", &opal_local_arch, PMIX_UINT32);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }
#endif

    return OMPI_SUCCESS;
}

static int ompi_proc_compare_vid (opal_list_item_t **a, opal_list_item_t **b)
{
    ompi_proc_t *proca = (ompi_proc_t *) *a;
    ompi_proc_t *procb = (ompi_proc_t *) *b;

    if (proca->super.proc_name.vpid > procb->super.proc_name.vpid) {
        return 1;
    } else {
        return -1;
    }

    /* they should never be equal */
}

/**
 * The process creation is split into two steps. The second step
 * is the important one, it sets the properties of the remote
 * process, such as architecture, node name and locality flags.
 *
 * This function is to be called __only__ after the modex exchange
 * has been performed, in order to allow the modex to carry the data
 * instead of requiring the runtime to provide it.
 */
int ompi_proc_complete_init(void)
{
    opal_process_name_t wildcard_rank;
    ompi_proc_t *proc;
    int ret, errcode = OMPI_SUCCESS;
    char *val = NULL;

    opal_mutex_lock (&ompi_proc_lock);

    /* Add all local peers first */
    wildcard_rank.jobid = OMPI_PROC_MY_NAME->jobid;
    wildcard_rank.vpid = OMPI_NAME_WILDCARD->vpid;
    /* retrieve the local peers */
    OPAL_MODEX_RECV_VALUE(ret, PMIX_LOCAL_PEERS,
                          &wildcard_rank, &val, PMIX_STRING);
    if (OPAL_SUCCESS == ret && NULL != val) {
        char **peers = opal_argv_split(val, ',');
        int i;
        free(val);
        for (i=0; NULL != peers[i]; i++) {
            ompi_vpid_t local_rank = strtoul(peers[i], NULL, 10);
            uint16_t u16, *u16ptr = &u16;
            if (OMPI_PROC_MY_NAME->vpid == local_rank) {
                continue;
            }
            ret = ompi_proc_allocate (OMPI_PROC_MY_NAME->jobid, local_rank, &proc);
            if (OMPI_SUCCESS != ret) {
                return ret;
            }
            /* get the locality information - all RTEs are required
             * to provide this information at startup */
            OPAL_MODEX_RECV_VALUE_OPTIONAL(ret, PMIX_LOCALITY, &proc->super.proc_name, &u16ptr, PMIX_UINT16);
            if (OPAL_SUCCESS == ret) {
                proc->super.proc_flags = u16;
            }
        }
        opal_argv_free(peers);
    }

    /* Complete initialization of node-local procs */
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        ret = ompi_proc_complete_init_single (proc);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            errcode = ret;
            break;
        }
    }

    /* Do not force-create every remote rank. Groups keep sentinels;
     * the PML materializes a skeleton on first use. */

    opal_list_sort (&ompi_proc_list, ompi_proc_compare_vid);

    opal_mutex_unlock (&ompi_proc_lock);

    return errcode;
}

int ompi_proc_finalize (void)
{
    ompi_proc_t *proc;

    /* Unregister the local proc from OPAL */
    opal_proc_local_set(NULL);

    /* remove all items from list and destroy them. Since we cannot know
     * the reference count of the procs for certain, it is possible that
     * a single OBJ_RELEASE won't drive the count to zero, and hence will
     * not release the memory. Accordingly, we cycle through the list here,
     * calling release on each item.
     *
     * This will cycle until it forces the reference count of each item
     * to zero, thus causing the destructor to run - which will remove
     * the item from the list!
     *
     * We cannot do this under the thread lock as the destructor will
     * call it when removing the item from the list. However, this function
     * is ONLY called from MPI_Finalize, and all threads are prohibited from
     * calling an MPI function once ANY thread has called MPI_Finalize. Of
     * course, multiple threads are allowed to call MPI_Finalize, so this
     * function may get called multiple times by various threads. We believe
     * it is thread safe to do so...though it may not -appear- to be so
     * without walking through the entire list/destructor sequence.
     */
    while ((ompi_proc_t *)opal_list_get_end(&ompi_proc_list) != (proc = (ompi_proc_t *)opal_list_get_first(&ompi_proc_list))) {
        OBJ_RELEASE(proc);
    }
    /* now destruct the list and thread lock */
    OBJ_DESTRUCT(&ompi_proc_list);
    OBJ_DESTRUCT(&ompi_proc_lock);
    OBJ_DESTRUCT(&ompi_proc_hash);

    return OMPI_SUCCESS;
}

int ompi_proc_world_size (void)
{
    return ompi_process_info.num_procs;
}

ompi_proc_t **ompi_proc_get_allocated (size_t *size)
{
    ompi_proc_t **procs;
    ompi_proc_t *proc;
    size_t count = 0;
    ompi_rte_cmp_bitmask_t mask;
    ompi_process_name_t my_name;

    /* check bozo case */
    if (NULL == ompi_proc_local_proc) {
        return NULL;
    }
    mask = OMPI_RTE_CMP_JOBID;
    my_name = *OMPI_CAST_RTE_NAME(&ompi_proc_local_proc->super.proc_name);

    /* First count how many match this jobid */
    opal_mutex_lock (&ompi_proc_lock);
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        if (OPAL_EQUAL == ompi_rte_compare_name_fields(mask, OMPI_CAST_RTE_NAME(&proc->super.proc_name), &my_name)) {
            ++count;
        }
    }

    /* allocate an array */
    procs = (ompi_proc_t**) malloc(count * sizeof(ompi_proc_t*));
    if (NULL == procs) {
        opal_mutex_unlock (&ompi_proc_lock);
        return NULL;
    }

    /* now save only the procs that match this jobid */
    count = 0;
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        if (OPAL_EQUAL == ompi_rte_compare_name_fields(mask, &proc->super.proc_name, &my_name)) {
            /* DO NOT RETAIN THIS OBJECT - the reference count on this
             * object will be adjusted by external callers. The intent
             * here is to allow the reference count to drop to zero if
             * the app no longer desires to communicate with this proc.
             * For example, the proc may call comm_disconnect on all
             * communicators involving this proc. In such cases, we want
             * the proc object to be removed from the list. By not incrementing
             * the reference count here, we allow this to occur.
             *
             * We don't implement that yet, but we are still safe for now as
             * the OBJ_NEW in ompi_proc_init owns the initial reference
             * count which cannot be released until ompi_proc_finalize is
             * called.
             */
            procs[count++] = proc;
        }
    }
    opal_mutex_unlock (&ompi_proc_lock);

    *size = count;
    return procs;
}

ompi_proc_t **ompi_proc_world (size_t *size)
{
    ompi_proc_t **procs;
    size_t count = 0;

    /* check bozo case */
    if (NULL == ompi_proc_local_proc) {
        return NULL;
    }

    /* First count how many match this jobid (we already know this from our process info) */
    count = ompi_process_info.num_procs;

    /* allocate an array */
    procs = (ompi_proc_t **) malloc (count * sizeof(ompi_proc_t*));
    if (NULL == procs) {
        return NULL;
    }

    /* now get/allocate all the procs in this jobid */
    for (size_t i = 0 ; i < count ; ++i) {
        opal_process_name_t name = {.jobid = OMPI_CAST_RTE_NAME(&ompi_proc_local_proc->super.proc_name)->jobid,
                                    .vpid = i};

        /* DO NOT RETAIN THIS OBJECT - the reference count on this
         * object will be adjusted by external callers. The intent
         * here is to allow the reference count to drop to zero if
         * the app no longer desires to communicate with this proc.
         * For example, the proc may call comm_disconnect on all
         * communicators involving this proc. In such cases, we want
         * the proc object to be removed from the list. By not incrementing
         * the reference count here, we allow this to occur.
         *
         * We don't implement that yet, but we are still safe for now as
         * the OBJ_NEW in ompi_proc_init owns the initial reference
         * count which cannot be released until ompi_proc_finalize is
         * called.
         */
        procs[i] = (ompi_proc_t*)ompi_proc_for_name (name);
    }

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

    opal_mutex_lock (&ompi_proc_lock);
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        /* We know this isn't consistent with the behavior in ompi_proc_world,
         * but we are leaving the RETAIN for now because the code using this function
         * assumes that the results need to be released when done. It will
         * be cleaned up later as the "fix" will impact other places in
         * the code
         */
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    opal_mutex_unlock (&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_self(size_t* size)
{
    ompi_proc_t **procs = (ompi_proc_t**) malloc(sizeof(ompi_proc_t*));
    if (NULL == procs) {
        return NULL;
    }
    /* We know this isn't consistent with the behavior in ompi_proc_world,
     * but we are leaving the RETAIN for now because the code using this function
     * assumes that the results need to be released when done. It will
     * be cleaned up later as the "fix" will impact other places in
     * the code
     */
    OBJ_RETAIN(ompi_proc_local_proc);
    *procs = ompi_proc_local_proc;
    *size = 1;
    return procs;
}

ompi_proc_t * ompi_proc_find ( const ompi_process_name_t * name )
{
    ompi_proc_t *proc, *rproc=NULL;
    ompi_rte_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = OMPI_RTE_CMP_JOBID | OMPI_RTE_CMP_VPID;
    opal_mutex_lock (&ompi_proc_lock);
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        if (OPAL_EQUAL == ompi_rte_compare_name_fields(mask, &proc->super.proc_name, name)) {
            rproc = proc;
            break;
        }
    }
    opal_mutex_unlock (&ompi_proc_lock);

    return rproc;
}


int ompi_proc_refresh(void)
{
    ompi_proc_t *proc = NULL;
    ompi_vpid_t i = 0;
    int ret=OMPI_SUCCESS;

    opal_mutex_lock (&ompi_proc_lock);

    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        /* Does not change: proc->super.proc_name.vpid */
        OMPI_CAST_RTE_NAME(&proc->super.proc_name)->jobid = OMPI_PROC_MY_NAME->jobid;

        /* Make sure to clear the local flag before we set it below */
        proc->super.proc_flags = 0;

        if (i == OMPI_PROC_MY_NAME->vpid) {
            ompi_proc_local_proc = proc;
            proc->super.proc_flags = OPAL_PROC_ALL_LOCAL;
            proc->super.proc_arch = opal_local_arch;
            opal_proc_learned(&proc->super,
                              OPAL_PROC_FLAG_AVAILABLE | OPAL_PROC_FLAG_INITIALIZED);
            opal_proc_local_set(&proc->super);
        } else {
            /* The name above just changed, so everything known about this
             * proc was known about somebody else: what was read out of the
             * old job, what was fetched for it, what was wired to reach
             * it. None of it holds, and the new name has published nothing
             * here yet. Resetting rather than clearing named flags is safe
             * because this walks the whole list at a restart, with nothing
             * else running to have its update overwritten. */
            opal_proc_forget_all(&proc->super);
            ret = ompi_proc_complete_init_single (proc);
            if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                break;
            }
        }
    }

    opal_mutex_unlock (&ompi_proc_lock);

    return ret;
}

int
ompi_proc_pack(ompi_proc_t **proclist, int proclistsize,
               pmix_data_buffer_t* buf)
{
    int rc;
    char *nspace;

    opal_mutex_lock (&ompi_proc_lock);

    /* cycle through the provided array, packing the OMPI level
     * data for each proc. This data may or may not be included
     * in any subsequent modex operation, so we include it here
     * to ensure completion of a connect/accept handshake. See
     * the ompi/mca/dpm framework for an example of where and how
     * this info is used.
     *
     * Eventually, we will review the procedures that call this
     * function to see if duplication of communication can be
     * reduced. For now, just go ahead and pack the info so it
     * can be sent.
     */
    for (int i = 0 ; i < proclistsize ; ++i) {
        ompi_proc_t *proc = proclist[i];
        pmix_proc_t prc;

        if (ompi_proc_is_sentinel (proc)) {
            proc = ompi_proc_for_name_nolock (ompi_proc_sentinel_to_name ((uintptr_t) proc));
        }

        /* send proc name */
        OPAL_PMIX_CONVERT_NAME(&prc, &(proc->super.proc_name));
        rc = PMIx_Data_pack(NULL, buf, &prc, 1, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            opal_mutex_unlock (&ompi_proc_lock);
            return opal_pmix_convert_status(rc);
        }
        /* retrieve and send the corresponding nspace for this job
         * as the remote side may not know the translation */
        nspace = opal_jobid_print(proc->super.proc_name.jobid);
        rc = PMIx_Data_pack(NULL, buf, &nspace, 1, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            opal_mutex_unlock (&ompi_proc_lock);
            return opal_pmix_convert_status(rc);
        }
        /* pack architecture flag */
        rc = PMIx_Data_pack(NULL, buf, &(proc->super.proc_arch), 1, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            opal_mutex_unlock (&ompi_proc_lock);
            return opal_pmix_convert_status(rc);
        }
    }
    opal_mutex_unlock (&ompi_proc_lock);
    return OMPI_SUCCESS;
}

ompi_proc_t *
ompi_proc_find_and_add(const ompi_process_name_t * name, bool* isnew)
{
    ompi_proc_t *proc, *rproc = NULL;
    ompi_rte_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = OMPI_RTE_CMP_JOBID | OMPI_RTE_CMP_VPID;
    opal_mutex_lock (&ompi_proc_lock);
    OPAL_LIST_FOREACH(proc, &ompi_proc_list, ompi_proc_t) {
        if (OPAL_EQUAL == ompi_rte_compare_name_fields(mask, &proc->super.proc_name, name)) {
            rproc = proc;
            *isnew = false;
            break;
        }
    }

    /* if we didn't find this proc in the list, create a new
     * proc_t and append it to the list
     */
    if (NULL == rproc) {
        *isnew = true;
        ompi_proc_allocate (name->jobid, name->vpid, &rproc);
    }

    opal_mutex_unlock (&ompi_proc_lock);

    return rproc;
}


int
ompi_proc_unpack(pmix_data_buffer_t* buf,
                 int proclistsize, ompi_proc_t ***proclist,
                 int *newproclistsize, ompi_proc_t ***newproclist)
{
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
        free(plist);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* cycle through the array of provided procs and unpack
     * their info - as packed by ompi_proc_pack
     */
    for (int i = 0; i < proclistsize ; ++i){
        int32_t count=1;
        ompi_process_name_t new_name;
        pmix_proc_t prc;
        uint32_t new_arch;
        bool isnew = false;
        int rc;
        char *nspace;
        uint16_t u16, *u16ptr;

        rc = PMIx_Data_unpack(NULL, buf, &prc, &count, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return opal_pmix_convert_status(rc);
        }
        OPAL_PMIX_CONVERT_PROCT(rc, &new_name, &prc);
        rc = PMIx_Data_unpack(NULL, buf, &nspace, &count, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return opal_pmix_convert_status(rc);
        }
        free(nspace);
        rc = PMIx_Data_unpack(NULL, buf, &new_arch, &count, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return opal_pmix_convert_status(rc);
        }
        /* see if this proc is already on our ompi_proc_list */
        plist[i] = ompi_proc_find_and_add(&new_name, &isnew);
        if (isnew) {
            /* if not, then it was added, so update the values
             * in the proc_t struct with the info that was passed
             * to us
             */
            newprocs[newprocs_len++] = plist[i];
        }

        /* A proc we already know can still be an unseeded skeleton -- a
         * wild receive or a sentinel resolution builds one -- so it
         * needs these values as much as a new one does, and it will not
         * find them in a modex this peer never sent us. */
        if (!opal_proc_known(&plist[i]->super, OPAL_PROC_FLAG_INITIALIZED)) {
            /* update all the values. The architecture came in the
             * packed proc, so nothing has to be read from the modex. */
            plist[i]->super.proc_arch = new_arch;
            /* if arch is different than mine, create a new convertor for this proc */
            if (plist[i]->super.proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(plist[i]->super.proc_convertor);
                plist[i]->super.proc_convertor = opal_convertor_create(plist[i]->super.proc_arch, 0);
#else
                char *errhost = opal_get_proc_hostname(&plist[i]->super);
                opal_show_help("help-mpi-runtime.txt",
                               "heterogeneous-support-unavailable",
                               true, ompi_process_info.nodename,
                               errhost);
                free(plist);
                free(newprocs);
                free(errhost);
                return OMPI_ERR_NOT_SUPPORTED;
#endif
            }

            /* Announce the convertor only once it is the peer's. */
            opal_atomic_wmb();
            opal_proc_learned(&plist[i]->super, OPAL_PROC_FLAG_INITIALIZED);

            /* get the locality information - all RTEs are required
             * to provide this information at startup */
            u16ptr = &u16;
            OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCALITY, &plist[i]->super.proc_name, &u16ptr, PMIX_UINT16);
            if (OPAL_SUCCESS == rc) {
                plist[i]->super.proc_flags = u16;
            }
        }
    }

    if (NULL != newproclistsize) *newproclistsize = newprocs_len;
    if (NULL != newproclist) {
        *newproclist = newprocs;
    } else if (newprocs != NULL) {
        free(newprocs);
    }

    *proclist = plist;
    return OMPI_SUCCESS;
}
