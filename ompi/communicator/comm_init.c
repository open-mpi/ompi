/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2017 University of Houston. All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 *                         All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * Copyright (c) 2023      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "opal/util/bit_ops.h"
#include "opal/util/info_subscriber.h"
#include "opal/util/string_copy.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/dpm/dpm.h"
#include "ompi/memchecker.h"
#include "ompi/instance/instance.h"

/*
** Table for Fortran <-> C communicator handle conversion
** Also used by P2P code to lookup communicator based
** on cid.
**
*/
opal_pointer_array_t ompi_mpi_communicators = {{0}};
opal_pointer_array_t ompi_comm_f_to_c_table = {{0}};
opal_hash_table_t ompi_comm_hash = {{0}};

ompi_predefined_communicator_t  ompi_mpi_comm_world = {{{{0}}}};
ompi_predefined_communicator_t  ompi_mpi_comm_self = {{{{0}}}};
ompi_predefined_communicator_t  ompi_mpi_comm_null = {{{{0}}}};
ompi_communicator_t  *ompi_mpi_comm_parent = NULL;

static bool ompi_comm_intrinsic_init;

ompi_predefined_communicator_t *ompi_mpi_comm_world_addr =
    &ompi_mpi_comm_world;
ompi_predefined_communicator_t *ompi_mpi_comm_self_addr =
    &ompi_mpi_comm_self;
ompi_predefined_communicator_t *ompi_mpi_comm_null_addr =
    &ompi_mpi_comm_null;

static void ompi_comm_construct(ompi_communicator_t* comm);
static void ompi_comm_destruct(ompi_communicator_t* comm);

OBJ_CLASS_INSTANCE(ompi_communicator_t, opal_infosubscriber_t,
                   ompi_comm_construct,
                   ompi_comm_destruct);

/* This is the counter for the number of communicators, which contain
   process with more than one jobid. This counter is a useful
   shortcut for finalize and abort. */
int ompi_comm_num_dyncomm=0;

static int ompi_comm_finalize (void);

/*
 * Initialize comm world/self/null/parent.
 */
int ompi_comm_init(void)
{
    /* Setup communicator array */
    OBJ_CONSTRUCT(&ompi_mpi_communicators, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_mpi_communicators, 16,
                                                OMPI_FORTRAN_HANDLE_MAX, 64) ) {
        return OMPI_ERROR;
    }

    OBJ_CONSTRUCT(&ompi_comm_hash, opal_hash_table_t);
    if (OPAL_SUCCESS != opal_hash_table_init (&ompi_comm_hash, 1024)) {
        return OMPI_ERROR;
    }

    /* Setup f to c table (we can no longer use the cid as the fortran handle) */
    OBJ_CONSTRUCT(&ompi_comm_f_to_c_table, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init (&ompi_comm_f_to_c_table, 8,
                                                 OMPI_FORTRAN_HANDLE_MAX, 32) ) {
        return OMPI_ERROR;
    }

    /*
     * reserve indices in the F to C table for:
     * MPI_COMM_WORLD
     * MPI_COMM_SELF
     * MPI_COMM_NULL
     */

    if (OPAL_SUCCESS != opal_pointer_array_set_item(&ompi_comm_f_to_c_table,
                                                      0,
                                                      (void *)-1L)) {
        return OMPI_ERROR;
    }

    if (OPAL_SUCCESS != opal_pointer_array_set_item(&ompi_comm_f_to_c_table,
                                                      1,
                                                      (void *)-1L)) {
        return OMPI_ERROR;
    }

    if (OPAL_SUCCESS != opal_pointer_array_set_item(&ompi_comm_f_to_c_table,
                                                      2,
                                                      (void *)-1L)) {
        return OMPI_ERROR;
    }

    /* Setup MPI_COMM_NULL */
    OBJ_CONSTRUCT(&ompi_mpi_comm_null, ompi_communicator_t);
    assert(ompi_mpi_comm_null.comm.c_f_to_c_index == 2);
    ompi_mpi_comm_null.comm.c_local_group  = &ompi_mpi_group_null.group;
    ompi_mpi_comm_null.comm.c_remote_group = &ompi_mpi_group_null.group;
    OBJ_RETAIN(&ompi_mpi_group_null.group);
    OBJ_RETAIN(&ompi_mpi_group_null.group);

    (void) ompi_comm_extended_cid_block_new (&ompi_mpi_comm_world.comm.c_contextidb,
                                             &ompi_mpi_comm_null.comm.c_contextidb, false);
    ompi_mpi_comm_null.comm.c_contextid    = ompi_mpi_comm_null.comm.c_contextidb.block_cid;
    ompi_mpi_comm_null.comm.c_index        = 2;
    ompi_mpi_comm_null.comm.c_my_rank      = MPI_PROC_NULL;

    ompi_mpi_comm_null.comm.error_handler  = &ompi_mpi_errors_are_fatal.eh;
    OBJ_RETAIN( &ompi_mpi_errors_are_fatal.eh );
    opal_pointer_array_set_item (&ompi_mpi_communicators, 2, &ompi_mpi_comm_null);

    /*
     * first three places in ompi_mpi_communicators are reserved for
     * MPI world model communicators MPI_COMM_WORLD, MPI_COMM_SELF, in addition to
     * MPI_COMM_NULL which is needed both for world and sessions models.
     * We conditionally insert ompi_mpi_comm_null into slots 0 and 1.  If MPI_Init was
     * already called then these two slots are already occupied.  If
     * MPI_Init is called after one or more MPI_Session_init, then
     * slots 0 and 1 will be filled in with MPI_COMM_WORLD and MPI_COMM_SELF.
     */
    (void)opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, 0, &ompi_mpi_comm_null);
    (void)opal_pointer_array_test_and_set_item(&ompi_mpi_communicators, 1, &ompi_mpi_comm_null);

    ompi_mpi_comm_null.comm.c_name = strdup ("MPI_COMM_NULL");
    ompi_mpi_comm_null.comm.c_flags |= OMPI_COMM_NAMEISSET | OMPI_COMM_INTRINSIC |
        OMPI_COMM_GLOBAL_INDEX;

    /* Initialize the parent communicator to MPI_COMM_NULL */
    ompi_mpi_comm_parent = &ompi_mpi_comm_null.comm;
    OBJ_RETAIN(&ompi_mpi_comm_null);
    OBJ_RETAIN(&ompi_mpi_group_null.group);

    /* initialize communicator requests (for ompi_comm_idup) */
    ompi_comm_request_init ();

    /* get a reference on the attributes subsys */
    ompi_attr_get_ref();

    ompi_mpi_instance_append_finalize (ompi_comm_finalize);

    return OMPI_SUCCESS;
}

int ompi_comm_init_mpi3 (void)
{
    int ret;
    ompi_group_t *group = NULL;

    /* the intrinsic communicators have been initialized */
    ompi_comm_intrinsic_init = true;

    /* Setup MPI_COMM_WORLD */
    OBJ_CONSTRUCT(&ompi_mpi_comm_world, ompi_communicator_t);
    assert(ompi_mpi_comm_world.comm.c_f_to_c_index == 0);

    ret = ompi_group_from_pset (ompi_mpi_instance_default, "mpi://WORLD", &group);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OMPI_GROUP_SET_INTRINSIC (group);
    ompi_comm_extended_cid_block_initialize (&ompi_mpi_comm_world.comm.c_contextidb, 0, 0, 0);
    ompi_mpi_comm_world.comm.c_contextid = ompi_mpi_comm_world.comm.c_contextidb.block_cid;
    ompi_mpi_comm_world.comm.c_index          = 0;
    ompi_mpi_comm_world.comm.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_world.comm.c_local_group  = group;
    ompi_mpi_comm_world.comm.c_remote_group = group;
    OBJ_RETAIN(ompi_mpi_comm_world.comm.c_remote_group);
    ompi_mpi_comm_world.comm.c_cube_dim     = opal_cube_dim ((int) group->grp_proc_count);
    ompi_mpi_comm_world.comm.error_handler  = ompi_initial_error_handler_eh;
    OBJ_RETAIN( ompi_mpi_comm_world.comm.error_handler );
    OMPI_COMM_SET_PML_ADDED(&ompi_mpi_comm_world.comm);
    opal_pointer_array_set_item (&ompi_mpi_communicators, 0, &ompi_mpi_comm_world);

    ompi_mpi_comm_world.comm.c_name = strdup("MPI_COMM_WORLD");
    ompi_mpi_comm_world.comm.c_flags |= OMPI_COMM_NAMEISSET | OMPI_COMM_INTRINSIC |
        OMPI_COMM_GLOBAL_INDEX;
    ompi_mpi_comm_world.comm.instance = group->grp_instance;

    /* get a reference on the attributes subsys */
    ompi_attr_get_ref();

    /* We have to create a hash (although it is legal to leave this
       filed NULL -- the attribute accessor functions will interpret
       this as "there are no attributes cached on this object")
       because MPI_COMM_WORLD has some predefined attributes. */
    ompi_attr_hash_init(&ompi_mpi_comm_world.comm.c_keyhash);

    /* Check for the binding policy used. We are only interested in
       whether mapby-node has been set right now (could be extended later)
       and only on MPI_COMM_WORLD, since for all other sub-communicators
       it is virtually impossible to identify their layout across nodes
       in the most generic sense. This is used by OMPIO for deciding which
       ranks to use for aggregators
    */
    opal_process_name_t wildcard = {OMPI_PROC_MY_NAME->jobid, OPAL_VPID_WILDCARD};
    char *str=NULL;
    int rc;

    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_MAPBY, &wildcard, &str, PMIX_STRING);
    if ( 0 == rc && NULL != str) {
        if ( strstr ( str, "BYNODE") ) {
            OMPI_COMM_SET_MAPBY_NODE(&ompi_mpi_comm_world.comm);
        }
        if (NULL != str) {
            free(str);
        }
    }
    /* Setup MPI_COMM_SELF */
    OBJ_CONSTRUCT(&ompi_mpi_comm_self, ompi_communicator_t);
    assert(ompi_mpi_comm_self.comm.c_f_to_c_index == 1);

    ret = ompi_group_from_pset (ompi_mpi_instance_default, "mpi://SELF", &group);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    OMPI_GROUP_SET_INTRINSIC (group);

    (void) ompi_comm_extended_cid_block_new (&ompi_mpi_comm_world.comm.c_contextidb,
                                             &ompi_mpi_comm_self.comm.c_contextidb, false);
    ompi_mpi_comm_self.comm.c_contextid = ompi_mpi_comm_self.comm.c_contextidb.block_cid;
    ompi_mpi_comm_self.comm.c_index    = 1;
    ompi_mpi_comm_self.comm.c_my_rank      = group->grp_my_rank;
    ompi_mpi_comm_self.comm.c_local_group  = group;
    ompi_mpi_comm_self.comm.c_remote_group = group;
    OBJ_RETAIN(ompi_mpi_comm_self.comm.c_remote_group);
    ompi_mpi_comm_self.comm.error_handler  = ompi_initial_error_handler_eh;
    OBJ_RETAIN( ompi_mpi_comm_self.comm.error_handler );
    OMPI_COMM_SET_PML_ADDED(&ompi_mpi_comm_self.comm);
    opal_pointer_array_set_item (&ompi_mpi_communicators, 1, &ompi_mpi_comm_self);

    ompi_mpi_comm_self.comm.c_name = strdup("MPI_COMM_SELF");
    ompi_mpi_comm_self.comm.c_flags |= OMPI_COMM_NAMEISSET | OMPI_COMM_INTRINSIC |
        OMPI_COMM_GLOBAL_INDEX;
    ompi_mpi_comm_self.comm.instance = group->grp_instance;

    /* We can set MPI_COMM_SELF's keyhash to NULL because it has no
       predefined attributes.  If a user defines an attribute on
       MPI_COMM_SELF, the keyhash will automatically be created. */
    ompi_mpi_comm_self.comm.c_keyhash = NULL;

    /*
     * finally here we set the predefined attribute keyvals
     */
    ompi_attr_set_predefined_keyvals_for_wm();

    OBJ_RETAIN(&ompi_mpi_errors_are_fatal.eh);
    /* During dyn_init, the comm_parent error handler will be set to the same
     * as comm_world (thus, the initial error handler). */

    return OMPI_SUCCESS;
}

static int ompi_comm_finalize (void)
{
    int max, i;
    ompi_communicator_t *comm;

    /* disconnect all dynamic communicators */
    ompi_dpm_dyn_finalize();

    if (ompi_comm_intrinsic_init) {
        /* tear down MPI-3 predefined communicators (not initialized unless using MPI_Init) */
        OBJ_DESTRUCT( &ompi_mpi_comm_self );
        ompi_attr_delete_predefined_keyvals_for_wm();
        /* Destroy the keyhash even is user defined attributes are still attached. */
        OBJ_DESTRUCT(ompi_mpi_comm_world.comm.c_keyhash);
        ompi_mpi_comm_world.comm.c_keyhash = NULL;
        OBJ_DESTRUCT( &ompi_mpi_comm_world );

        ompi_comm_intrinsic_init = false;
    }

    /* Shut down the parent communicator, if it exists */
    if( ompi_mpi_comm_parent != &ompi_mpi_comm_null.comm ) {
        /* Note that we pass ompi_mpi_comm_parent here
           (vs. &ompi_mpi_comm_parent) because it is of type
           (ompi_communicator_t*), *NOT* (ompi_communicator_t).  This
           is because a parent communicator is created dynamically
           during init, and we just set this pointer to it.  Hence, we
           just pass in the pointer here. */
        OBJ_DESTRUCT (ompi_mpi_comm_parent);

        /* Please note, that the we did increase the reference count
           for ompi_mpi_comm_null, ompi_mpi_group_null, and
           ompi_mpi_errors_are_fatal in ompi_comm_init because of
           ompi_mpi_comm_parent.  In case a
           parent communicator is really created, the ref. counters
           for these objects are decreased again by one. However, in a
           static scenario, we should ideally decrease the ref. counter
           for these objects by one here. The problem just is, that
           if the app had a parent_comm, and this has been freed/disconnected,
           ompi_comm_parent points again to ompi_comm_null, the reference count
           for these objects has not been increased again.
           So the point is, if ompi_mpi_comm_parent == &ompi_mpi_comm_null
           we do not know whether we have to decrease the ref count for
           those three objects or not. Since this is a constant, non-increasing
           amount of memory, we stick with the current solution for now,
           namely don't do anything.
        */
    }

    /* Shut down MPI_COMM_NULL */
    OBJ_DESTRUCT( &ompi_mpi_comm_null );

    /* Check whether we have some communicators left */
    max = opal_pointer_array_get_size(&ompi_mpi_communicators);
    for ( i=3; i<max; i++ ) {
        comm = (ompi_communicator_t *)opal_pointer_array_get_item(&ompi_mpi_communicators, i);
        if ( NULL != comm ) {
            /* Communicator has not been freed before finalize */
            OBJ_RELEASE(comm);
            comm=(ompi_communicator_t *)opal_pointer_array_get_item(&ompi_mpi_communicators, i);
            if ( NULL != comm ) {
                /* Still here ? */
                if ( !OMPI_COMM_IS_EXTRA_RETAIN(comm)) {

                    /* For communicator that have been marked as "extra retain", we do not further
                     * enforce to decrease the reference counter once more. These "extra retain"
                     * communicators created e.g. by the hierarch or inter module did increase
                     * the reference count by one more than other communicators, on order to
                     * allow for deallocation with the parent communicator. Note, that
                     * this only occurs if the cid of the local_comm is lower than of its
                     * parent communicator. Read the comment in comm_activate for
                     * a full explanation.
                     */
                    if ( ompi_debug_show_handle_leaks && !(OMPI_COMM_IS_FREED(comm)) ){
                        opal_output(0,"WARNING: MPI_Comm still allocated in MPI_Finalize\n");
                        ompi_comm_dump ( comm);
                        OBJ_RELEASE(comm);
                    }
                }
            }
        }
    }

    OBJ_DESTRUCT (&ompi_mpi_communicators);
    OBJ_DESTRUCT (&ompi_comm_hash);
    OBJ_DESTRUCT (&ompi_comm_f_to_c_table);

    /* finalize communicator requests */
    ompi_comm_request_fini ();

    /* release a reference to the attributes subsys */
    return ompi_attr_put_ref();
}

/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/* static functions */

static void ompi_comm_construct(ompi_communicator_t* comm)
{
    int idx;
    comm->c_name         = NULL;
    comm->c_index        = MPI_UNDEFINED;
    comm->c_flags        = 0;
    comm->c_my_rank      = 0;
    comm->c_cube_dim     = 0;
    comm->c_local_group  = NULL;
    comm->c_remote_group = NULL;
    comm->error_handler  = NULL;
    comm->c_pml_comm     = NULL;
    comm->c_topo         = NULL;
    comm->c_coll         = NULL;
    comm->c_nbc_tag      = MCA_COLL_BASE_TAG_NONBLOCKING_BASE;
    comm->instance       = NULL;

    /*
     * magic numerology - see TOPDIR/ompi/include/mpif-values.pl
     */
    idx = (comm == (ompi_communicator_t*)ompi_mpi_comm_world_addr) ? 0 :
              (comm == (ompi_communicator_t*)ompi_mpi_comm_self_addr) ? 1 :
                  (comm == (ompi_communicator_t*)ompi_mpi_comm_null_addr) ? 2 : -1;
    if (-1 == idx) {
        comm->c_f_to_c_index = opal_pointer_array_add(&ompi_comm_f_to_c_table,
                                                      comm);
    } else {
        opal_pointer_array_set_item(&ompi_comm_f_to_c_table, idx, comm);
        comm->c_f_to_c_index = idx;
    }

    /* A keyhash will be created if/when an attribute is cached on
       this communicator */
    comm->c_keyhash      = NULL;
    comm->errhandler_type = OMPI_ERRHANDLER_TYPE_COMM;
    comm->error_handler  = &ompi_mpi_errors_are_fatal.eh;
#ifdef OMPI_WANT_PERUSE
    comm->c_peruse_handles = NULL;
#endif
    OBJ_CONSTRUCT(&comm->c_lock, opal_mutex_t);

#if OPAL_ENABLE_FT_MPI
    comm->any_source_enabled  = true;
    comm->any_source_offset   = 0;
    comm->num_acked           = 0;
    comm->comm_revoked        = false;
    comm->coll_revoked        = false;
    comm->c_epoch             = 0;
    comm->agreement_specific  = NULL;
#endif
}

static void ompi_comm_destruct(ompi_communicator_t* comm)
{
    /* Note that the attributes were already released on this
       communicator in ompi_comm_free() (i.e., from MPI_COMM_FREE /
       MPI_COMM_DISCONNECT).  See the lengthy comment in
       communicator/comm.c in ompi_comm_free() for the reasons why. */

    /* Release the collective module */

    if ( NULL != comm->c_coll ) {
        mca_coll_base_comm_unselect(comm);
    }

    /* Tell the PML that this communicator is done.
       MCA_PML_CALL(add_comm()) was called explicitly in
       ompi_comm_init() when setting up COMM_WORLD and COMM_SELF; it's
       called in ompi_comm_set() for all others.  This means that all
       communicators must be destroyed before the PML shuts down.

       Also -- do not invoke the pml_del_comm if the corresponding
       pml_add_comm was never invoked.  This can happen in an error
       situation where, for example, attributes do not copy properly
       from one communicator to another and we end up destroying the
       new communication while propagating the error up the stack.  We
       want to make it all the way up the stack to invoke the MPI
       error, not cause a seg fault in pml_del_comm because it was
       never pml_add_com'ed. */

    if ( MPI_COMM_NULL != comm && OMPI_COMM_IS_PML_ADDED(comm) ) {
        MCA_PML_CALL(del_comm (comm));
    }

    /* Release topology module */
    if (NULL != comm->c_topo) {
        OBJ_RELEASE(comm->c_topo);
        comm->c_topo = NULL;
    }

    if (NULL != comm->c_local_group) {
        OBJ_RELEASE ( comm->c_local_group );
        comm->c_local_group = NULL;
        if ( OMPI_COMM_IS_INTRA(comm) ) {
            /* We have to decrement the ref count on the remote group
               even if it is identical to the local one in case of
               intra-comm */
            OBJ_RELEASE ( comm->c_remote_group );
            comm->c_remote_group = NULL;
        }
    }

    if (NULL != comm->c_remote_group) {
        OBJ_RELEASE ( comm->c_remote_group );
        comm->c_remote_group = NULL;
    }

    if (NULL != comm->error_handler) {
        OBJ_RELEASE ( comm->error_handler );
        comm->error_handler = NULL;
    }

    if (NULL != comm->c_name) {
        free (comm->c_name);
        comm->c_name = NULL;
    }

#if OPAL_ENABLE_FT_MPI
    if( NULL != comm->agreement_specific ) {
        OBJ_RELEASE( comm->agreement_specific );
    }
#endif  /* OPAL_ENABLE_FT_MPI */

    /* mark this cid as available */
    if ( MPI_UNDEFINED != (int)comm->c_index &&
         NULL != opal_pointer_array_get_item(&ompi_mpi_communicators,
                                             comm->c_index)) {
        opal_pointer_array_set_item ( &ompi_mpi_communicators,
                                      comm->c_index, NULL);
        if (!OMPI_COMM_IS_GLOBAL_INDEX(comm)) {
            opal_hash_table_remove_value_ptr (&ompi_comm_hash, &comm->c_contextid,
                                              sizeof (comm->c_contextid));
        }
    }

    /* reset the ompi_comm_f_to_c_table entry */
    if ( MPI_UNDEFINED != comm->c_f_to_c_index &&
         NULL != opal_pointer_array_get_item(&ompi_comm_f_to_c_table,
                                             comm->c_f_to_c_index)) {
        opal_pointer_array_set_item ( &ompi_comm_f_to_c_table,
                                      comm->c_f_to_c_index, NULL);
    }

    OBJ_DESTRUCT(&comm->c_lock);
}

#define OMPI_COMM_SET_INFO_FN(name, flag)       \
    static const char *ompi_comm_set_ ## name (opal_infosubscriber_t *obj, const char *key, const char *value) \
    {                                                                   \
        ompi_communicator_t *comm = (ompi_communicator_t *) obj;        \
                                                                        \
        if (opal_str_to_bool(value)) {                                  \
            comm->c_assertions |= flag;                                 \
        } else {                                                        \
            comm->c_assertions &= ~flag;                                \
        }                                                               \
                                                                        \
        return OMPI_COMM_CHECK_ASSERT(comm, flag) ? "true" : "false";   \
    }

OMPI_COMM_SET_INFO_FN(no_any_source, OMPI_COMM_ASSERT_NO_ANY_SOURCE)
OMPI_COMM_SET_INFO_FN(no_any_tag, OMPI_COMM_ASSERT_NO_ANY_TAG)
OMPI_COMM_SET_INFO_FN(allow_overtake, OMPI_COMM_ASSERT_ALLOW_OVERTAKE)
OMPI_COMM_SET_INFO_FN(exact_length, OMPI_COMM_ASSERT_EXACT_LENGTH)
OMPI_COMM_SET_INFO_FN(lazy_barrier, OMPI_COMM_ASSERT_LAZY_BARRIER)
OMPI_COMM_SET_INFO_FN(active_poll, OMPI_COMM_ASSERT_ACTIVE_POLL)

void ompi_comm_assert_subscribe (ompi_communicator_t *comm, int32_t assert_flag)
{
    switch (assert_flag) {
    case OMPI_COMM_ASSERT_NO_ANY_SOURCE:
        opal_infosubscribe_subscribe (&comm->super, "mpi_assert_no_any_source", "false", ompi_comm_set_no_any_source);
        break;
    case OMPI_COMM_ASSERT_NO_ANY_TAG:
        opal_infosubscribe_subscribe (&comm->super, "mpi_assert_no_any_tag", "false", ompi_comm_set_no_any_tag);
        break;
    case OMPI_COMM_ASSERT_ALLOW_OVERTAKE:
        opal_infosubscribe_subscribe (&comm->super, "mpi_assert_allow_overtaking", "false", ompi_comm_set_allow_overtake);
        break;
    case OMPI_COMM_ASSERT_EXACT_LENGTH:
        opal_infosubscribe_subscribe (&comm->super, "mpi_assert_exact_length", "false", ompi_comm_set_exact_length);
        break;
    case OMPI_COMM_ASSERT_LAZY_BARRIER:
        opal_infosubscribe_subscribe (&comm->super, "ompi_assert_lazy_barrier", "false", ompi_comm_set_lazy_barrier);
        break;
    case OMPI_COMM_ASSERT_ACTIVE_POLL:
        opal_infosubscribe_subscribe (&comm->super, "ompi_assert_active_poll", "true", ompi_comm_set_active_poll);
        break;
    }
}
