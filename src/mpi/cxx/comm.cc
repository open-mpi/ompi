// -*- c++ -*-
// 
// $HEADER$
//

#include "ompi_config.h"
#include "mpi/cxx/mpicxx.h"
#include "threads/mutex.h"

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
    if (mpi_comm_map_mutex == NULL)
        mpi_comm_map_mutex = OBJ_NEW(ompi_mutex_t);
    if (mpi_err_map_mutex == NULL)
        mpi_err_map_mutex = OBJ_NEW(ompi_mutex_t);
    if (key_fn_map_mutex == NULL)
        key_fn_map_mutex = OBJ_NEW(ompi_mutex_t);
}

// copy
MPI::Comm::Comm(const Comm_Null& data) : Comm_Null(data) 
{
    if (mpi_comm_map_mutex == NULL)
        mpi_comm_map_mutex = OBJ_NEW(ompi_mutex_t);
    if (mpi_err_map_mutex == NULL)
        mpi_err_map_mutex = OBJ_NEW(ompi_mutex_t);
    if (key_fn_map_mutex == NULL)
        key_fn_map_mutex = OBJ_NEW(ompi_mutex_t);
}


void
MPI::Comm::Free(void) 
{
    MPI_Comm save = mpi_comm;
    (void)MPI_Comm_free(&mpi_comm);
    
    OMPI_THREAD_LOCK(mpi_comm_map_mutex);
    if (MPI::Comm::mpi_comm_map[save] != 0)
        delete MPI::Comm::mpi_comm_map[save];
    MPI::Comm::mpi_comm_map.erase(save);
    OMPI_THREAD_UNLOCK(mpi_comm_map_mutex);
}


void
MPI::Comm::Set_errhandler(const MPI::Errhandler& errhandler)
{
    my_errhandler = (MPI::Errhandler *)&errhandler;
    OMPI_THREAD_LOCK(mpi_err_map_mutex);
    MPI::Comm::mpi_err_map[mpi_comm] = this;
    OMPI_THREAD_UNLOCK(mpi_err_map_mutex);
    (void)MPI_Errhandler_set(mpi_comm, errhandler);
}


//JGS I took the const out because it causes problems when trying to
//call this function with the predefined NULL_COPY_FN etc.
int
MPI::Comm::Create_keyval(MPI::Comm::_MPI2CPP_COPYATTRFN_* comm_copy_attr_fn,
                         MPI::Comm::_MPI2CPP_DELETEATTRFN_* comm_delete_attr_fn,
                         void* extra_state)
{
    int keyval;
    (void)MPI_Keyval_create(copy_attr_intercept, delete_attr_intercept,
                            &keyval, extra_state);
    key_pair_t* copy_and_delete = 
        new key_pair_t(comm_copy_attr_fn, comm_delete_attr_fn); 
    OMPI_THREAD_LOCK(key_fn_map_mutex);
    MPI::Comm::key_fn_map[keyval] = copy_and_delete;
    OMPI_THREAD_UNLOCK(key_fn_map_mutex);
    return keyval;
}


void
MPI::Comm::Free_keyval(int& comm_keyval)
{
    int save = comm_keyval;
    (void)MPI_Keyval_free(&comm_keyval);
    OMPI_THREAD_LOCK(key_fn_map_mutex);
    if (MPI::Comm::key_fn_map[save] != 0)
        delete MPI::Comm::key_fn_map[save];
    MPI::Comm::key_fn_map.erase(save);
    OMPI_THREAD_UNLOCK(key_fn_map_mutex);
}


void
MPI::Comm::Set_attr(int comm_keyval, const void* attribute_val) const
{
    CommType type;
    int status;
    
    (void)MPI_Comm_test_inter(mpi_comm, &status);
    if (status) {
        type = eIntercomm;
    }
    else {
        (void)MPI_Topo_test(mpi_comm, &status);    
        if (status == MPI_CART)
            type = eCartcomm;
        else if (status == MPI_GRAPH)
            type = eGraphcomm;
        else
            type = eIntracomm;
    }
    OMPI_THREAD_LOCK(mpi_comm_map_mutex);
    if (MPI::Comm::mpi_comm_map[mpi_comm] == 0) {
        comm_pair_t* comm_type = new comm_pair_t((Comm*) this, type);
        MPI::Comm::mpi_comm_map[mpi_comm] = comm_type;
    }
    OMPI_THREAD_UNLOCK(mpi_comm_map_mutex);
    (void)MPI_Attr_put(mpi_comm, comm_keyval, (void*) attribute_val);
}
