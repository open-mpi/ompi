/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2025      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mpi/tool/mpit-internal.h"

opal_mutex_t ompi_mpit_big_lock = OPAL_MUTEX_STATIC_INIT;

volatile uint32_t ompi_mpit_init_count = 0;

void ompi_mpit_lock (void)
{
    opal_mutex_lock (&ompi_mpit_big_lock);
}

void ompi_mpit_unlock (void)
{
    opal_mutex_unlock (&ompi_mpit_big_lock);
}

static MPI_Datatype mca_to_mpi_datatypes[MCA_BASE_VAR_TYPE_MAX] = {
    [MCA_BASE_VAR_TYPE_INT] = MPI_INT,
    [MCA_BASE_VAR_TYPE_UNSIGNED_INT] = MPI_UNSIGNED,
    [MCA_BASE_VAR_TYPE_UNSIGNED_LONG] = MPI_UNSIGNED_LONG,
    [MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG] = MPI_UNSIGNED_LONG_LONG,

#if SIZEOF_SIZE_T == SIZEOF_UNSIGNED_INT
    [MCA_BASE_VAR_TYPE_SIZE_T] = MPI_UNSIGNED,
#elif SIZEOF_SIZE_T == SIZEOF_UNSIGNED_LONG
    [MCA_BASE_VAR_TYPE_SIZE_T] = MPI_UNSIGNED_LONG,
#elif SIZEOF_SIZE_T == SIZEOF_LONG_LONG
    [MCA_BASE_VAR_TYPE_SIZE_T] = MPI_UNSIGNED_LONG_LONG,
#else
    [MCA_BASE_VAR_TYPE_SIZE_T] = NULL,
#endif

    [MCA_BASE_VAR_TYPE_STRING] = MPI_CHAR,
    [MCA_BASE_VAR_TYPE_VERSION_STRING] = MPI_CHAR,
    [MCA_BASE_VAR_TYPE_BOOL] = MPI_C_BOOL,
    [MCA_BASE_VAR_TYPE_DOUBLE] = MPI_DOUBLE,
    [MCA_BASE_VAR_TYPE_LONG] = MPI_LONG,
    [MCA_BASE_VAR_TYPE_INT32_T] = MPI_INT32_T,
    [MCA_BASE_VAR_TYPE_UINT32_T] = MPI_UINT32_T,
    [MCA_BASE_VAR_TYPE_INT64_T] = MPI_INT64_T,
    [MCA_BASE_VAR_TYPE_UINT64_T] = MPI_UINT64_T,
};

int ompit_var_type_to_datatype (mca_base_var_type_t type, MPI_Datatype *datatype)
{
    if (!datatype) {
        return OMPI_SUCCESS;
    }

    *datatype = mca_to_mpi_datatypes[type];
    assert (*datatype);

    return OMPI_SUCCESS;
}

int ompit_opal_to_mpit_error (int rc)
{
    if (rc >= 0) {
        /* Already an MPI error (always >= 0) */
        return rc;
    }

    switch (rc) {
    case OPAL_ERR_OUT_OF_RESOURCE:
        return MPI_T_ERR_MEMORY;
    case OPAL_ERR_VALUE_OUT_OF_BOUNDS:
    case OPAL_ERR_NOT_BOUND:
        return MPI_T_ERR_INVALID_HANDLE;
    default:
        return MPI_T_ERR_INVALID;
    }
}

/*
 * Check whether a MPI object is valid or not.
 * If invalid return true, otherwise false.
 */
bool ompit_obj_invalid(void *obj_handle)
{
    bool ret = true; /* by default return obj is invalid */
    opal_object_t *opal_obj = NULL;
    opal_class_t *opal_class;
    const char *obj_name = NULL;

    if (NULL == obj_handle) {
        goto fn_exit;
    }

    /* we are actually evaulating a pointer to an OMPI MPI opaque handle */
    opal_obj = *(opal_object_t **)obj_handle;

    /*
     * this should have already been checked by the caller but we do it here too
     */
    if (NULL == opal_obj) {
        goto fn_exit;
    }

    opal_class = opal_obj->obj_class;
    if (NULL == opal_class) {
        goto fn_exit;
    }

    obj_name = opal_class->cls_name;
    if (NULL == obj_name) {
        goto fn_exit;
    }

    /*
     * An MPIT C or P var or event can in principle be bound to any of these MPI object types
     * This code is meant to address, at least in part, the text in section 15.3.2 in the MPI 4.1
     * standard concerning binding one of the T things to an MPI object.
     */
    if (0 == strncmp(obj_name, "ompi_communicator_t", strlen("ompi_communicator_t"))) {
        ompi_communicator_t *comm = (ompi_communicator_t *)obj_handle;
        ret = ompi_comm_invalid(comm);
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_win_t", strlen("ompi_win_t"))) {
        ompi_win_t *win = (ompi_win_t *)obj_handle;
        ret = ompi_win_invalid(win);
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_file_t", strlen("ompi_file_t"))) {
        ompi_file_t *file = (ompi_file_t *)obj_handle;
        ret = ompi_file_invalid(file);
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_instance_t", strlen("ompi_instance_t"))) {
        ompi_instance_t *instance = (ompi_instance_t *)obj_handle;
        ret = ompi_instance_invalid(instance);
        goto fn_exit;
    }

    /*
     * following object types don't seem to have robust validity checks so just
     * do a smoke test for use of NULL objects.
     */
    if (0 == strncmp(obj_name, "ompi_info_t", strlen("ompi_info_t"))) {
        ompi_info_t *info = (ompi_info_t *)obj_handle;
        ret = (MPI_INFO_NULL == info) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_datatype_t", strlen("ompi_datatype_t"))) {
        ompi_datatype_t *datatype = (ompi_datatype_t *)obj_handle;
        ret = (MPI_DATATYPE_NULL == datatype) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_request_t", strlen("ompi_request_t"))) {
        ompi_request_t *request = (ompi_request_t *)obj_handle;
        ret = (MPI_REQUEST_NULL == request) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_errhandler_t", strlen("ompi_errhandler_t"))) {
        ompi_errhandler_t *errhandler = (ompi_errhandler_t *)obj_handle;
        ret = (MPI_ERRHANDLER_NULL == errhandler) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_opt_t", strlen("ompi_op_t"))) {
        ompi_op_t *op = (ompi_op_t *)obj_handle;
        ret = (MPI_OP_NULL == op) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_message_t", strlen("ompi_message_t"))) {
        ompi_message_t *message = (ompi_message_t *)obj_handle;
        ret = (MPI_MESSAGE_NULL == message) ? true : false;
        goto fn_exit;
    }

    if (0 == strncmp(obj_name, "ompi_group_t", strlen("ompi_group_t"))) {
        ompi_group_t *group = (ompi_group_t *)obj_handle;
        ret = (MPI_GROUP_NULL == group) ? true : false;
        goto fn_exit;
    }

fn_exit:
    return ret;
}

