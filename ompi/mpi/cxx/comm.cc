// -*- c++ -*-
// 
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

// do not include ompi_config.h because it kills the free/malloc defines
#include "mpi.h"
#include "ompi/mpi/cxx/mpicxx.h"
#include "opal/threads/mutex.h"

#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/errhandler/errhandler.h"

static void cxx_comm_keyval_destructor(int keyval);

//
// These functions are all not inlined because they need to use locks to
// protect the handle maps and it would be bad to have those in headers
// because that would require that we always install the lock headers.
// Instead we take the function call hit (we're locking - who cares about
// a function call.  And these aren't exactly the performance critical
// functions) and make everyone's life easier.
//


// construction
MPI::Comm::Comm()
{
}

// copy
MPI::Comm::Comm(const Comm_Null& data) : Comm_Null(data) 
{
}


void
MPI::Comm::Set_errhandler(const MPI::Errhandler& errhandler)
{
    my_errhandler = (MPI::Errhandler *)&errhandler;
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Comm::mpi_comm_err_map[mpi_comm] = this;
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
    (void)MPI_Errhandler_set(mpi_comm, errhandler);
}


//JGS I took the const out because it causes problems when trying to
//call this function with the predefined NULL_COPY_FN etc.
int
MPI::Comm::do_create_keyval(MPI_Comm_copy_attr_function* c_copy_fn,
                            MPI_Comm_delete_attr_function* c_delete_fn,
                            Copy_attr_function* cxx_copy_fn,
                            Delete_attr_function* cxx_delete_fn,
                            void* extra_state)
{
    int keyval, ret, count = 0;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t delete_fn;
    Copy_attr_function *cxx_pair_copy = NULL;
    Delete_attr_function *cxx_pair_delete = NULL;

    // We do not call MPI_Comm_create_keyval() here because we need to
    // pass in a special destructor to the backend keyval creation
    // that gets invoked when the keyval's reference count goes to 0
    // and is finally destroyed (i.e., clean up some caching/lookup
    // data here in the C++ bindings layer).  This destructor is
    // *only* used in the C++ bindings, so it's not set by the C
    // MPI_Comm_create_keyval().  Hence, we do all the work here (and
    // ensure to set the destructor atomicly when the keyval is
    // created).

    // Error check.  Must have exactly 2 non-NULL function pointers.
    if (NULL != c_copy_fn) {
        copy_fn.attr_communicator_copy_fn = 
            (MPI_Comm_internal_copy_attr_function*) c_copy_fn;
        ++count;
    }
    if (NULL != c_delete_fn) {
        delete_fn.attr_communicator_delete_fn = c_delete_fn;
        ++count;
    }
    if (NULL != cxx_copy_fn) {
        copy_fn.attr_communicator_copy_fn =
            (MPI_Comm_internal_copy_attr_function*) ompi_mpi_cxx_comm_copy_attr_intercept;
        cxx_pair_copy = cxx_copy_fn;
        ++count;
    }
    if (NULL != cxx_delete_fn) {
        delete_fn.attr_communicator_delete_fn =
            ompi_mpi_cxx_comm_delete_attr_intercept;
        cxx_pair_delete = cxx_delete_fn;
        ++count;
    }
    if (2 != count) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                      "MPI::Comm::Create_keyval");
    }

    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, delete_fn,
                                  &keyval, extra_state, 0,
                                  cxx_comm_keyval_destructor);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    keyval_pair_t* copy_and_delete = 
        new keyval_pair_t(cxx_pair_copy, cxx_pair_delete);
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Comm::mpi_comm_keyval_fn_map[keyval] = copy_and_delete;
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
    return keyval;
}

// This function is called back out of the keyval destructor in the C
// layer when the keyval is not be used by any attributes anymore,
// anywhere.  So we can definitely safely remove the entry for this
// keyval from the C++ map.
static void cxx_comm_keyval_destructor(int keyval) 
{
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    if (MPI::Comm::mpi_comm_keyval_fn_map.end() !=
        MPI::Comm::mpi_comm_keyval_fn_map.find(keyval) &&
        NULL != MPI::Comm::mpi_comm_keyval_fn_map[keyval]) {
        delete MPI::Comm::mpi_comm_keyval_fn_map[keyval];
        MPI::Comm::mpi_comm_keyval_fn_map.erase(keyval);
    }
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}

