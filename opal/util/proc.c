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
#include "opal/mca/pmix/pmix.h"

opal_process_info_t opal_process_info = {
    .nodename = "not yet named",
    .job_session_dir = "not yet defined",
    .proc_session_dir = "not yet defined",
    .num_local_peers = 0,  /* there is nobody else but me */
    .my_local_rank = 0,    /* I'm the only process around here */
#if OPAL_HAVE_HWLOC
    .cpuset = NULL,
#endif
};

static opal_proc_t opal_local_proc = {
    { .opal_list_next = NULL,
      .opal_list_prev = NULL},
    OPAL_NAME_INVALID,
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

/* this function is used to temporarily set the local
 * name while OPAL and upper layers are initializing,
 * thus allowing debug messages to be more easily
 * understood */
void opal_proc_set_name(opal_process_name_t *name)
{
    opal_local_proc.proc_name = *name;
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

