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
// Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

// do not include ompi_config.h because it kills the free/malloc defines
#include "mpi.h"
#include "ompi/mpi/cxx/mpicxx.h"

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"

#include "ompi/communicator/communicator.h"
#include "ompi/attribute/attribute.h"
#include "ompi/errhandler/errhandler.h"

// Struct to make a linked list of keyval intercept data
struct keyval_intercept_data_item_t {
    opal_list_item_t super;
    int kid_keyval;
    MPI::Comm::keyval_intercept_data_t *kid_data;
};

// We are explicitly *not* using the STL here (just for the sake of
// avoiding using the STL; e.g., Solaris has 2 STL's -- which one
// should OMPI use?  What if OMPI uses one and the MPI app wants to
// use the other?), so use the C++-like opal_list_t stuff.
OBJ_CLASS_DECLARATION(keyval_intercept_data_item_t);
OBJ_CLASS_INSTANCE(keyval_intercept_data_item_t, opal_list_item_t, NULL, NULL);

// List to hold the cxx_extra_state structs that are new'ed when C++
// keyvals are created
static opal_list_t cxx_extra_states;
// Whether or not cxx_extra_states has been initialized yet
static volatile bool cxx_extra_states_init = false;
// Will be set to 1 by the thread who is actually doing the initialization
static volatile int32_t cxx_extra_states_init_thread = 0;
// Lock to protect cxx_extra_states from being accessed by multiple
// threads at the same time
opal_mutex_t MPI::Comm::cxx_extra_states_lock;

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


// This function is called back out of the keyval destructor in the C
// layer when the keyval is not be used by any attributes anymore,
// anywhere.  So we can definitely safely remove the entry for this
// keyval's C++ intercept extra_state from the list
static void cxx_comm_keyval_destructor(int keyval) 
{
    opal_list_item_t *item;
    keyval_intercept_data_item_t *kid;

    // Search the list until we find the item with the same keyval
    for (item = opal_list_get_first(&cxx_extra_states);
         opal_list_get_end(&cxx_extra_states) != item; 
         item = opal_list_get_next(item)) {
        kid = (keyval_intercept_data_item_t *) item;
        if (kid->kid_keyval == keyval) {
            delete kid->kid_data;
            opal_list_remove_item(&cxx_extra_states, item);
            OBJ_RELEASE(item);
            break;
        }
    }
}


//JGS I took the const out because it causes problems when trying to
//call this function with the predefined NULL_COPY_FN etc.
int
MPI::Comm::do_create_keyval(MPI_Comm_copy_attr_function* c_copy_fn,
                            MPI_Comm_delete_attr_function* c_delete_fn,
                            Copy_attr_function* cxx_copy_fn,
                            Delete_attr_function* cxx_delete_fn,
                            void* extra_state, int &keyval)
{
    int ret, count = 0;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t delete_fn;
    keyval_intercept_data_t *cxx_extra_state;

    // If both the callbacks are C, then do the simple thing -- no
    // need for all the C++ machinery.
    if (NULL != c_copy_fn && NULL != c_delete_fn) {
        copy_fn.attr_communicator_copy_fn = 
            (MPI_Comm_internal_copy_attr_function*) c_copy_fn;
        delete_fn.attr_communicator_delete_fn = c_delete_fn;
        ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, delete_fn,
                                      &keyval, extra_state, 0, NULL);
        if (MPI_SUCCESS != ret) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                          "MPI::Comm::Create_keyval");
        }
    }

    // If either callback is C++, then we have to use the C++
    // callbacks for both, because we have to generate a new
    // extra_state.  And since we only get one extra_state (i.e., we
    // don't get one extra_state for the copy callback and another
    // extra_state for the delete callback), we have to use the C++
    // callbacks for both (and therefore translate the C++-special
    // extra_state into the user's original extra_state).
    cxx_extra_state = new keyval_intercept_data_t;
    if (NULL == cxx_extra_state) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM, 
                                      "MPI::Comm::Create_keyval");
    }
    cxx_extra_state->c_copy_fn = c_copy_fn;
    cxx_extra_state->cxx_copy_fn = cxx_copy_fn;
    cxx_extra_state->c_delete_fn = c_delete_fn;
    cxx_extra_state->cxx_delete_fn = cxx_delete_fn;

    // Error check.  Must have exactly 2 non-NULL function pointers.
    if (NULL != c_copy_fn) {
        ++count;
    }
    if (NULL != c_delete_fn) {
        ++count;
    }
    if (NULL != cxx_copy_fn) {
        ++count;
    }
    if (NULL != cxx_delete_fn) {
        ++count;
    }
    if (2 != count) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                      "MPI::Comm::Create_keyval");
    }

    // We do not call MPI_Comm_create_keyval() here because we need to
    // pass in a special destructor to the backend keyval creation
    // that gets invoked when the keyval's reference count goes to 0
    // and is finally destroyed (i.e., clean up some caching/lookup
    // data here in the C++ bindings layer).  This destructor is
    // *only* used in the C++ bindings, so it's not set by the C
    // MPI_Comm_create_keyval().  Hence, we do all the work here (and
    // ensure to set the destructor atomicly when the keyval is
    // created).

    copy_fn.attr_communicator_copy_fn =
        (MPI_Comm_internal_copy_attr_function*) 
        ompi_mpi_cxx_comm_copy_attr_intercept;
    delete_fn.attr_communicator_delete_fn =
        ompi_mpi_cxx_comm_delete_attr_intercept;
    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, delete_fn,
                                  &keyval, cxx_extra_state, 0,
                                  cxx_comm_keyval_destructor);
    if (OMPI_SUCCESS != ret) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret,
                                      "MPI::Comm::Create_keyval");
    }

    // Ensure to initialize the list safely
    if (opal_atomic_cmpset_32(&cxx_extra_states_init_thread, 0, 1)) {
        OBJ_CONSTRUCT(&cxx_extra_states, opal_list_t);
        OBJ_CONSTRUCT(&cxx_extra_states_lock, opal_mutex_t);
        cxx_extra_states_init = true;
    } else {
        while (!cxx_extra_states_init) {
#if defined(__WINDOWS__)
            SwitchToThread();
#else
            sched_yield();
#endif  /* defined(__WINDOWS__) */
        }
    }

    // Put this cxx_extra_state in a place where the
    // cxx_comm_keyval_destructor can find it based on the keyval
    // (because that's all the cxx_comm_keyval_destructor gets as an
    // argument)
    keyval_intercept_data_item_t *kid = OBJ_NEW(keyval_intercept_data_item_t);
    kid->kid_keyval = keyval;
    kid->kid_data = cxx_extra_state;
    OPAL_THREAD_LOCK(&cxx_extra_states_lock);
    opal_list_append(&cxx_extra_states, &kid->super);
    OPAL_THREAD_UNLOCK(&cxx_extra_states_lock);

    return MPI_SUCCESS;
}

