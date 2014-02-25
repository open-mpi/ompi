/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/win/win.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/constants.h"
#include "ompi/attribute/attribute.h"
#include "ompi/group/group.h"
#include "ompi/info/info.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/osc.h"


/*
 * Table for Fortran <-> C communicator handle conversion.  Note that
 * these are not necessarily global.
 */
opal_pointer_array_t ompi_mpi_windows; 

ompi_predefined_win_t ompi_mpi_win_null;
ompi_predefined_win_t *ompi_mpi_win_null_addr = &ompi_mpi_win_null;

static void ompi_win_construct(ompi_win_t *win);
static void ompi_win_destruct(ompi_win_t *win);

OBJ_CLASS_INSTANCE(ompi_win_t, opal_object_t,
                   ompi_win_construct, ompi_win_destruct);

int
ompi_win_init(void)
{
    /* setup window Fortran array */
    OBJ_CONSTRUCT(&ompi_mpi_windows, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_mpi_windows, 0,
                                                OMPI_FORTRAN_HANDLE_MAX, 64) ) {
        return OMPI_ERROR;
    }

    /* Setup MPI_WIN_NULL */
    OBJ_CONSTRUCT(&ompi_mpi_win_null.win, ompi_win_t);
    ompi_mpi_win_null.win.w_flags = OMPI_WIN_INVALID;
    ompi_mpi_win_null.win.w_group = &ompi_mpi_group_null.group;
    OBJ_RETAIN(&ompi_mpi_group_null);
    ompi_win_set_name(&ompi_mpi_win_null.win, "MPI_WIN_NULL");
    opal_pointer_array_set_item(&ompi_mpi_windows, 0, &ompi_mpi_win_null.win);

    return OMPI_SUCCESS;
}


int
ompi_win_finalize(void)
{
    OBJ_DESTRUCT(&ompi_mpi_win_null.win);
    OBJ_DESTRUCT(&ompi_mpi_windows);

    return OMPI_SUCCESS;
}

static ompi_win_t *
alloc_window(struct ompi_communicator_t *comm)
{
    ompi_win_t *win;
    ompi_group_t *group;

    /* create the object */
    win = OBJ_NEW(ompi_win_t);
    if (NULL == win) return NULL;

    /* setup data that is independent of osc component */
    group = comm->c_local_group;
    OBJ_RETAIN(group);
    ompi_group_increment_proc_count(group);
    win->w_group = group;

    return win;
}

static int
config_window(void *base, size_t size, int disp_unit,
              int flavor, int model, ompi_win_t *win)
{
    int ret;

    ret = ompi_attr_set_c(WIN_ATTR, win, &win->w_keyhash, 
                          MPI_WIN_BASE, base, true);
    if (OMPI_SUCCESS != ret) return ret;

    ret = ompi_attr_set_fortran_mpi2(WIN_ATTR, win, 
                                     &win->w_keyhash, 
                                     MPI_WIN_SIZE, size, true);
    if (OMPI_SUCCESS != ret) return ret;

    ret = ompi_attr_set_fortran_mpi2(WIN_ATTR, win, 
                                     &win->w_keyhash, 
                                     MPI_WIN_DISP_UNIT, disp_unit,
                                     true);
    if (OMPI_SUCCESS != ret) return ret;

    ret = ompi_attr_set_fortran_mpi2(WIN_ATTR, win,
                                     &win->w_keyhash,
                                     MPI_WIN_CREATE_FLAVOR, flavor, true);
    if (OMPI_SUCCESS != ret) return ret;

    ret = ompi_attr_set_fortran_mpi2(WIN_ATTR, win,
                                     &win->w_keyhash,
                                     MPI_WIN_MODEL, model, true);
    if (OMPI_SUCCESS != ret) return ret;

    win->w_f_to_c_index = opal_pointer_array_add(&ompi_mpi_windows, win);
    if (-1 == win->w_f_to_c_index) return OMPI_ERR_OUT_OF_RESOURCE;

    return OMPI_SUCCESS;
}

int
ompi_win_create(void *base, size_t size, 
                int disp_unit, ompi_communicator_t *comm,
                ompi_info_t *info,
                ompi_win_t** newwin)
{
    ompi_win_t *win;
    int model;
    int ret;

    win = alloc_window(comm);
    if (NULL == win) return OMPI_ERR_OUT_OF_RESOURCE;

    ret = ompi_osc_base_select(win, &base, size, disp_unit, comm, info, MPI_WIN_FLAVOR_CREATE, &model);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    ret = config_window(base, size, disp_unit, MPI_WIN_FLAVOR_CREATE, model, win);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    *newwin = win;

    return OMPI_SUCCESS;
}


int
ompi_win_allocate(size_t size, int disp_unit, ompi_info_t *info,
                  ompi_communicator_t *comm, void *baseptr, ompi_win_t **newwin)
{
    ompi_win_t *win;
    int model;
    int ret;
    void *base;

    win = alloc_window(comm);
    if (NULL == win) return OMPI_ERR_OUT_OF_RESOURCE;

    ret = ompi_osc_base_select(win, &base, size, disp_unit, comm, info, MPI_WIN_FLAVOR_ALLOCATE, &model);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    ret = config_window(base, size, disp_unit, MPI_WIN_FLAVOR_ALLOCATE, model, win);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    *((void**) baseptr) = base;
    *newwin = win;

    return OMPI_SUCCESS;
}


int
ompi_win_allocate_shared(size_t size, int disp_unit, ompi_info_t *info,
                         ompi_communicator_t *comm, void *baseptr, ompi_win_t **newwin)
{
    ompi_win_t *win;
    int model;
    int ret;
    void *base;

    win = alloc_window(comm);
    if (NULL == win) return OMPI_ERR_OUT_OF_RESOURCE;

    ret = ompi_osc_base_select(win, &base, size, disp_unit, comm, info, MPI_WIN_FLAVOR_SHARED, &model);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    ret = config_window(base, size, disp_unit, MPI_WIN_FLAVOR_SHARED, model, win);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    *((void**) baseptr) = base;
    *newwin = win;

    return OMPI_SUCCESS;
}


int
ompi_win_create_dynamic(ompi_info_t *info, ompi_communicator_t *comm, ompi_win_t **newwin)
{
    ompi_win_t *win;
    int model;
    int ret;

    win = alloc_window(comm);
    if (NULL == win) return OMPI_ERR_OUT_OF_RESOURCE;

    ret = ompi_osc_base_select(win, MPI_BOTTOM, 0, 1, comm, info, MPI_WIN_FLAVOR_DYNAMIC, &model);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    ret = config_window(MPI_BOTTOM, 0, 1, MPI_WIN_FLAVOR_DYNAMIC, model, win);
    if (OMPI_SUCCESS != ret) {
        OBJ_RELEASE(win);
        return ret;
    }

    *newwin = win;

    return OMPI_SUCCESS;
}


int
ompi_win_free(ompi_win_t *win)
{
    int ret = win->w_osc_module->osc_free(win);

    if (-1 != win->w_f_to_c_index) {
        opal_pointer_array_set_item(&ompi_mpi_windows,
                                    win->w_f_to_c_index,
                                    NULL);
    }

    if (OMPI_SUCCESS == ret) {
        OBJ_RELEASE(win);
    }

    return ret;
}


int
ompi_win_set_name(ompi_win_t *win, const char *win_name)
{
    OPAL_THREAD_LOCK(&(win->w_lock));
    memset(win->w_name, 0, MPI_MAX_OBJECT_NAME);
    strncpy(win->w_name, win_name, MPI_MAX_OBJECT_NAME);
    win->w_name[MPI_MAX_OBJECT_NAME - 1] = 0;
    OPAL_THREAD_UNLOCK(&(win->w_lock));

    return OMPI_SUCCESS;
}


int
ompi_win_get_name(ompi_win_t *win, char *win_name, int *length)
{
    OPAL_THREAD_LOCK(&(win->w_lock));
    strncpy(win_name, win->w_name, MPI_MAX_OBJECT_NAME);
    *length = (int)strlen(win->w_name);
    OPAL_THREAD_UNLOCK(&(win->w_lock));

    return OMPI_SUCCESS;
}


int
ompi_win_group(ompi_win_t *win, ompi_group_t **group) {
    OBJ_RETAIN(win->w_group);
    ompi_group_increment_proc_count(win->w_group);
    *group = win->w_group;

    return OMPI_SUCCESS;
}


static void
ompi_win_construct(ompi_win_t *win)
{
    OBJ_CONSTRUCT(&win->w_lock, opal_mutex_t);
    win->w_name[0] = '\0';
    win->w_group = NULL;
    win->w_keyhash = NULL;
    win->w_f_to_c_index = 0;

    /* every new window defaults to MPI_ERRORS_ARE_FATAL (MPI-2 6.6.1,
       pg. 137) */
    OBJ_RETAIN(&ompi_mpi_errors_are_fatal.eh);
    win->error_handler = &ompi_mpi_errors_are_fatal.eh;
    win->errhandler_type = OMPI_ERRHANDLER_TYPE_WIN;

    win->w_flags = 0;
    win->w_osc_module = NULL;
}


static void
ompi_win_destruct(ompi_win_t *win)
{
    if (NULL != win->w_keyhash) {
        ompi_attr_delete_all(WIN_ATTR, win, win->w_keyhash);
        OBJ_RELEASE(win->w_keyhash);
    }

    if (NULL != win->error_handler) {
        OBJ_RELEASE(win->error_handler);
    }

    if (NULL != win->w_group) {
        ompi_group_decrement_proc_count(win->w_group);
        OBJ_RELEASE(win->w_group);
    }

    OBJ_DESTRUCT(&win->w_lock);
}
