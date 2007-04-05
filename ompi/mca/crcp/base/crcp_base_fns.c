/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
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

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>
#include <ctype.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/util/output.h"
#include "opal/util/os_dirpath.h"

#include "orte/mca/smr/smr.h"
#include "orte/mca/gpr/gpr.h"

#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"
#include "ompi/mca/pml/base/pml_base_request.h"

/******************
 * Local Functions
 ******************/

/******************
 * Object stuff
 ******************/
OBJ_CLASS_INSTANCE(ompi_crcp_base_pml_state_t,
                   ompi_free_list_item_t,
                   NULL,
                   NULL
                   );

OBJ_CLASS_INSTANCE(ompi_crcp_base_btl_state_t,
                   ompi_free_list_item_t,
                   NULL,
                   NULL
                   );

/***********************
 * None component stuff
 ************************/
int ompi_crcp_base_none_open(void)
{
    return OMPI_SUCCESS;
}

int ompi_crcp_base_none_close(void)
{
    return OMPI_SUCCESS;
}

int ompi_crcp_base_module_init(void)
{
    return OMPI_SUCCESS;
}

int ompi_crcp_base_module_finalize(void)
{
    return OMPI_SUCCESS;
}

/****************
 * PML Wrapper
 ****************/
ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_enable( bool enable,
                                                            ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_add_comm( struct ompi_communicator_t* comm,
                                                              ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_del_comm( struct ompi_communicator_t* comm,
                                                              ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_add_procs( struct ompi_proc_t **procs,
                                                               size_t nprocs,
                                                               ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_del_procs( struct ompi_proc_t **procs,
                                                               size_t nprocs,
                                                               ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_progress(ompi_crcp_base_pml_state_t* pml_state)
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}
    
ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_iprobe(int dst, int tag,
                                                           struct ompi_communicator_t* comm,
                                                           int *matched, ompi_status_public_t* status,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_probe( int dst, int tag,
                                                           struct ompi_communicator_t* comm,
                                                           ompi_status_public_t* status,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_isend_init( void *buf, size_t count,
                                                                ompi_datatype_t *datatype,
                                                                int dst, int tag, 
                                                                mca_pml_base_send_mode_t mode,
                                                                struct ompi_communicator_t* comm,
                                                                struct ompi_request_t **request,
                                                                ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_isend( void *buf, size_t count,
                                                           ompi_datatype_t *datatype,
                                                           int dst, int tag,
                                                           mca_pml_base_send_mode_t mode,
                                                           struct ompi_communicator_t* comm,
                                                           struct ompi_request_t **request,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_send(  void *buf, size_t count,
                                                           ompi_datatype_t *datatype,
                                                           int dst, int tag,
                                                           mca_pml_base_send_mode_t mode,
                                                           struct ompi_communicator_t* comm,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_irecv_init( void *buf, size_t count,
                                                                ompi_datatype_t *datatype,
                                                                int src, int tag,
                                                                struct ompi_communicator_t* comm,
                                                                struct ompi_request_t **request,
                                                                ompi_crcp_base_pml_state_t* pml_state)
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_irecv( void *buf, size_t count,
                                                           ompi_datatype_t *datatype,
                                                           int src, int tag,
                                                           struct ompi_communicator_t* comm,
                                                           struct ompi_request_t **request,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_recv(  void *buf, size_t count,
                                                           ompi_datatype_t *datatype,
                                                           int src, int tag,
                                                           struct ompi_communicator_t* comm,
                                                           ompi_status_public_t* status,
                                                           ompi_crcp_base_pml_state_t* pml_state)
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_dump( struct ompi_communicator_t* comm,
                                                          int verbose,
                                                          ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}
    
ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_start( size_t count,
                                                           ompi_request_t** requests,
                                                           ompi_crcp_base_pml_state_t* pml_state )
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

ompi_crcp_base_pml_state_t* ompi_crcp_base_none_pml_ft_event(int state,
                                                             ompi_crcp_base_pml_state_t* pml_state)
{
    pml_state->error_code = OMPI_SUCCESS;
    return pml_state;
}

/********************
 * Request Interface
 ********************/
int ompi_crcp_base_none_request_complete( struct ompi_request_t *request ) {
    return OMPI_SUCCESS;
}

/********************
 * BTL Interface
 ********************/
ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_add_procs( struct mca_btl_base_module_t* btl,
                                   size_t nprocs,
                                   struct ompi_proc_t** procs,
                                   struct mca_btl_base_endpoint_t** endpoints,
                                   struct ompi_bitmap_t* reachable,
                                   ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_del_procs( struct mca_btl_base_module_t* btl,
                                   size_t nprocs,
                                   struct ompi_proc_t** procs,
                                   struct mca_btl_base_endpoint_t** endpoints,
                                   ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_register( struct mca_btl_base_module_t* btl,
                                  mca_btl_base_tag_t tag,
                                  mca_btl_base_module_recv_cb_fn_t cbfunc,
                                  void* cbdata,
                                  ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_finalize( struct mca_btl_base_module_t* btl,
                                  ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_alloc( struct mca_btl_base_module_t* btl,
                               size_t size,
                               ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_free( struct mca_btl_base_module_t* btl,
                              mca_btl_base_descriptor_t* descriptor,
                              ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_prepare_src( struct mca_btl_base_module_t* btl,
                                     struct mca_btl_base_endpoint_t* endpoint,
                                     mca_mpool_base_registration_t* registration,
                                     struct ompi_convertor_t* convertor,
                                     size_t reserve,
                                     size_t* size,
                                     ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_prepare_dst( struct mca_btl_base_module_t* btl,
                                     struct mca_btl_base_endpoint_t* endpoint,
                                     mca_mpool_base_registration_t* registration,
                                     struct ompi_convertor_t* convertor,
                                     size_t reserve,
                                     size_t* size,
                                     ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_send( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              struct mca_btl_base_descriptor_t* descriptor,
                              mca_btl_base_tag_t tag,
                              ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_put( struct mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* endpoint,
                             struct mca_btl_base_descriptor_t* descriptor,
                             ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_get( struct mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* endpoint,
                             struct mca_btl_base_descriptor_t* descriptor,
                             ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}


ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_dump( struct mca_btl_base_module_t* btl,
                              struct mca_btl_base_endpoint_t* endpoint,
                              int verbose,
                              ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}

ompi_crcp_base_btl_state_t*
ompi_crcp_base_none_btl_ft_event(int state,
                                 ompi_crcp_base_btl_state_t* btl_state)
{
    btl_state->error_code = OMPI_SUCCESS;
    return btl_state;
}


/********************
 * Utility functions
 ********************/
int ompi_crcp_base_reboot_pml(ompi_crcp_base_pml_state_t* pml_state) {
    int ret;
    ompi_proc_t** procs;
    size_t nprocs;
    char *error_msg = NULL;
    int return_code = OMPI_SUCCESS;
    ompi_communicator_t *tmp_comm;
    int comm_size = 0;
    uint32_t c = 0;

    opal_output_verbose(5, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML/BML/BTL are rebooting [PML = %s]",
                        mca_pml_base_selected_component.pmlm_version.mca_component_name);

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Disable PML");

    /* Disable the PML */
    if( OMPI_SUCCESS != (ret = MCA_PML_CALL(enable(false)) ) ) {
        error_msg = "PML control failed";
        return_code = ret;
        goto cleanup;
    }

    /* Get all the processes that we know about */
    nprocs = 0;
    if (NULL == (procs = ompi_proc_world(&nprocs))) {
        error_msg = "ompi_proc_world() failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Delete all communicators from the PML");

    /* Get all the communicators that we know about */
    comm_size = ompi_pointer_array_get_size(&ompi_mpi_communicators);
    for( c = 0; c < (uint32_t)comm_size; ++c) {
        tmp_comm = ompi_comm_lookup(c);
        if( ompi_comm_invalid(tmp_comm) ) {
            continue;
        }
        if( OMPI_SUCCESS != (ret = MCA_PML_CALL(del_comm(tmp_comm)) ) ) {
            error_msg = "PML del comm failed";
            return_code = ret;
            goto cleanup;;
        }
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Delete all %d processes from the PML",
                        (int)nprocs);
    /* Delete all the procs */
    if( OMPI_SUCCESS != (ret = MCA_PML_CALL(del_procs(procs, nprocs)) ) ) {
        error_msg = "PML del procs failed";
        return_code = ret;
        goto cleanup;
    }

    /* Shutdown the PML/BML/BTL */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Shutdown the PML");
    mca_pml_base_close();

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Shutdown the BML");
    mca_bml.bml_finalize();

    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_finalize())) {
        error_msg = "PML base_modex_finalize failed";
        return_code = ret;
        goto cleanup;
    }

    /* Refresh the ompi_proc structures & 
     * Since they are pointed to by the communicators then they are updated as well */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Refresh Process List");
    for(c = 0; c < nprocs; ++c) {
        if( procs[c]->proc_modex != NULL ) {
            OBJ_RELEASE(procs[c]->proc_modex);
            procs[c]->proc_modex = NULL;
        }
        if( procs[c]->proc_bml != NULL ) {
            OBJ_RELEASE( procs[c]->proc_bml);
            procs[c]->proc_bml = NULL;
        }
        if( procs[c]->proc_pml != NULL ) {
            free( procs[c]->proc_pml);
            procs[c]->proc_pml = NULL;
        }
        if( procs[c]->proc_hostname != NULL ) {
            free( procs[c]->proc_hostname );
            procs[c]->proc_hostname = NULL;
        }
        /*procs[c]->proc_arch = ompi_mpi_local_arch;*/
        procs[c]->proc_flags = 0;
    }

    /* Restart the PML/BML/BTL */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML base Open");
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        error_msg = "PML base_open failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML init modex");
    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_init())) {
        error_msg = "PML base_modex_init failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML base select");
    if (OMPI_SUCCESS !=
        (ret = mca_pml_base_select(OMPI_ENABLE_PROGRESS_THREADS,
                                   OMPI_ENABLE_MPI_THREADS))) {
        error_msg = "PML base_select failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML modex exchange");
    if (OMPI_SUCCESS != (ret = mca_pml_base_modex_exchange())) {
        error_msg = "PML base_modex_exchange failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Enter Stage Gate 1");
    if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                                       ORTE_PROC_STATE_AT_STG1, 0))) {
        error_msg = "PML Stage Gate 1 SOH failed";
        return_code = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, true,
                                              NULL, orte_gpr.deliver_notify_msg))) {
        error_msg = "PML RML.Xcast(Stage1) failed";
        return_code = ret;
        goto cleanup;
    }

    /* Enable the PML */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML Enable");
    if( OMPI_SUCCESS != (ret = MCA_PML_CALL(enable(true)) ) ) {
        error_msg = "PML control failed";
        return_code = ret;
        goto cleanup;
    }

    /* Add back the processes */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML get new list of processes");
    /* Get all the processes that we know about */
    nprocs = 0;
    if (NULL == (procs = ompi_proc_world(&nprocs))) {
        error_msg = "ompi_proc_world() failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(52, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML Add back the %d processes",
                        (int)nprocs);
    if( OMPI_SUCCESS != (ret = MCA_PML_CALL(add_procs(procs, nprocs)) ) ) {
        error_msg = "PML add procs failed";
        return_code = ret;
        goto cleanup;
    }
    free(procs);

    /* Add back the communicators */
    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML Add back the communicators");
    for( c = 0; c < (uint32_t)comm_size; ++c) {
        tmp_comm = ompi_comm_lookup(c);
        if( ompi_comm_invalid(tmp_comm) ) {
            continue;
        }
        if( OMPI_SUCCESS != (ret = MCA_PML_CALL(add_comm(tmp_comm)) ) ) {
            error_msg = "PML add comm failed";
            return_code = ret;
            goto cleanup;;
        }
    }

    opal_output_verbose(25, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): Enter Stage Gate 2");
    if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name,
                                                       ORTE_PROC_STATE_AT_STG2, 0))) {
        error_msg = "PML Stage Gate 2 SOH  failed";
        return_code = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = orte_rml.xcast(ORTE_PROC_MY_NAME->jobid, false,
                                              NULL, orte_gpr.deliver_notify_msg))) {
        error_msg = "PML RML.Xcast(Stage2) failed";
        return_code = ret;
        goto cleanup;
    }

    opal_output_verbose(5, ompi_crcp_base_output,
                        "crcp:coord: reboot_pml(): PML/BML/BTL have been rebooted [PML = %s]",
                        mca_pml_base_selected_component.pmlm_version.mca_component_name);

 cleanup:
    return return_code;
}
