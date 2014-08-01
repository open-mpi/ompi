/*
 * Copyright (c) 2013      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013      Inria.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "proc.h"
#include "opal/util/proc.h"
#include "opal/util/arch.h"
#include "opal/mca/dstore/dstore.h"

opal_process_info_t opal_process_info = {
    .nodename = "not yet named",
    .job_session_dir = "not yet defined",
    .proc_session_dir = "not yet defined",
    .num_local_peers = 1,  /* I'm the only process around here */
    .my_local_rank = 0,    /* I'm the only process around here */
#if OPAL_HAVE_HWLOC
    .cpuset = NULL,
#endif
};

static opal_proc_t opal_local_proc = {
    { .opal_list_next = NULL,
      .opal_list_prev = NULL},
    0x1122334455667788,
    0,
    0,
    NULL,
    "localhost - unnamed"
};
static opal_proc_t* opal_proc_my_name = &opal_local_proc;

static void opal_proc_construct(opal_proc_t* proc)
{
    proc->proc_arch = opal_local_arch;
    proc->proc_convertor = NULL;
    proc->proc_flags = 0;
    proc->proc_name = 0;
}

static void opal_proc_destruct(opal_proc_t* proc)
{
    proc->proc_flags     = 0;
    proc->proc_name      = 0;
    proc->proc_hostname  = NULL;
    proc->proc_convertor = NULL;
}

OBJ_CLASS_INSTANCE(opal_proc_t, opal_list_item_t,
                   opal_proc_construct, opal_proc_destruct);

static int
opal_compare_opal_procs(const opal_process_name_t proc1,
                        const opal_process_name_t proc2)
{
    if( proc1 == proc2 ) return  0;
    if( proc1 <  proc2 ) return -1;
    return 1;
}

opal_compare_proc_fct_t opal_compare_proc = opal_compare_opal_procs;

opal_proc_t* opal_proc_local_get(void)
{
    return opal_proc_my_name;
}

int opal_proc_local_set(opal_proc_t* proc)
{
    if( proc != opal_proc_my_name ) {
        if( NULL != proc )
            OBJ_RETAIN(proc);
        if( &opal_local_proc != opal_proc_my_name )
            OBJ_RELEASE(opal_proc_my_name);
        if( NULL != proc ) {
            opal_proc_my_name = proc;
        } else {
            opal_proc_my_name = &opal_local_proc;
        }
    }
    return OPAL_SUCCESS;
}

/**
 * The following functions are surrogates for the RTE functionality, and are not supposed
 * to be called. Instead, the corresponding function pointer should be set by the upper layer
 * before the call to opal_init, to make them point to the correct accessors based on the
 * underlying RTE.
 */
static char*
opal_process_name_print_should_never_be_called(const opal_process_name_t procname)
{
    return "My Name is Nobody";
}

static uint32_t
opal_process_name_vpid_should_never_be_called(const opal_process_name_t unused)
{
    return UINT_MAX;
}

char* (*opal_process_name_print)(const opal_process_name_t) = opal_process_name_print_should_never_be_called;
uint32_t (*opal_process_name_vpid)(const opal_process_name_t) = opal_process_name_vpid_should_never_be_called;
uint32_t (*opal_process_name_jobid)(const opal_process_name_t) = opal_process_name_vpid_should_never_be_called;

static int
opal_modex_send_internal(const mca_base_component_t *source_component,
                         const void *data, size_t size)
{
    int rc;
    char *key;
    opal_byte_object_t bo;
    opal_value_t kv;
    const opal_proc_t *proc = opal_proc_local_get();

    key = mca_base_component_to_string(source_component);
    if (NULL == key) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    bo.bytes = (uint8_t *)data;
    bo.size = size;

    /* the store API makes a copy of the provided data */
    OBJ_CONSTRUCT(&kv, opal_value_t);
    kv.key = key;
    if (OPAL_SUCCESS != (rc = opal_value_load(&kv, (void*)&bo, OPAL_BYTE_OBJECT))) {
        OBJ_DESTRUCT(&kv);
        free(key);
        return rc;
    }

    /* MPI connection data is to be shared with ALL other processes */
    rc = opal_dstore.store(opal_dstore_peer, (opal_identifier_t*)&proc->proc_name, &kv);
    OBJ_DESTRUCT(&kv);
    return rc;
}

static int
opal_modex_recv_internal(const mca_base_component_t *component,
                         const opal_proc_t *proc,
                         void **buffer, size_t *size)
{
    int rc;
    opal_list_t myvals;
    opal_value_t *kv;
    char *key;
    opal_byte_object_t *boptr;

    key = mca_base_component_to_string(component);
    if (NULL == key) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    OPAL_OUTPUT_VERBOSE((2, 0, "%s fetch data from %s for %s",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                         OPAL_NAME_PRINT(proc->proc_name), key));

    OBJ_CONSTRUCT(&myvals, opal_list_t);
    /* the peer dstore contains our own data that will be shared
     * with our peers - the nonpeer dstore contains data we received
     * that would only be shared with nonpeer procs
     */
    if (OPAL_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_nonpeer,
                                                (opal_identifier_t*)(&proc->proc_name),
                                                key, &myvals))) {
        /* see if we can find it in the internal dstore */
        OPAL_OUTPUT_VERBOSE((2, 0, "%s searching nonpeer dstore for %s",
                             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key));
        if (OPAL_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_internal,
                                                    (opal_identifier_t*)(&proc->proc_name),
                                                    key, &myvals))) {
            /* try one last place - the peer dstore in case it got stuck there for some reason */
            OPAL_OUTPUT_VERBOSE((2, 0, "%s searching internal dstore for %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), key));
            if (OPAL_SUCCESS != (rc = opal_dstore.fetch(opal_dstore_peer,
                                                        (opal_identifier_t*)(&proc->proc_name),
                                                        key, &myvals))) {
                OPAL_LIST_DESTRUCT(&myvals);
                free(key);
                return rc;
            }
        }
    }
    /* only one value should have been returned */
    kv = (opal_value_t*)opal_list_get_first(&myvals);
    if (NULL == kv) {
        free(key);
        return OPAL_ERROR;
    }
    opal_value_unload(kv, (void**)&boptr, OPAL_BYTE_OBJECT);
    OPAL_LIST_DESTRUCT(&myvals);

    if (OPAL_SUCCESS == rc) {
        /* xfer the data - it was allocated in the call */
        *buffer = (void*)boptr->bytes;
        *size = boptr->size;
        /* we no longer require the struct itself since all we
         * wanted was the data inside it
         */
        free(boptr);
    }
    free(key);
    return OPAL_SUCCESS;
}

int
(*opal_modex_send)(const mca_base_component_t *source_component,
                   const void *data, size_t size) = opal_modex_send_internal;
int
(*opal_modex_recv)(const mca_base_component_t *component,
                   const opal_proc_t *proc,
                   void **buffer, size_t *size) = opal_modex_recv_internal;

