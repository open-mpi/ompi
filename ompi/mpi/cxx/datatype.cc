// -*- c++ -*-
// 
// Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
//                         reserved. 
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


void
MPI::Datatype::Free()
{
    MPI_Datatype save = mpi_datatype;
    (void)MPI_Type_free(&mpi_datatype);

    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Datatype::mpi_type_map.erase(save);
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}


int
MPI::Datatype::Create_keyval(MPI::Datatype::Copy_attr_function* type_copy_attr_fn,
                             MPI::Datatype::Delete_attr_function* type_delete_attr_fn, 
                             void* extra_state)
{
  int keyval;
  (void) MPI_Type_create_keyval(ompi_mpi_cxx_type_copy_attr_intercept,
                                ompi_mpi_cxx_type_delete_attr_intercept,
                                &keyval, extra_state);
  key_pair_t* copy_and_delete = 
      new key_pair_t(type_copy_attr_fn, type_delete_attr_fn); 
  OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
  MPI::Datatype::mpi_type_key_fn_map[keyval] = copy_and_delete;
  OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
  return keyval;
}


void
MPI::Datatype::Free_keyval(int& type_keyval)
{
    int save = type_keyval;
    (void) MPI_Type_free_keyval(&type_keyval);
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Datatype::mpi_type_key_fn_map.erase(save);
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}


void
MPI::Datatype::Set_attr(int type_keyval, const void* attribute_val)
{
  (void) MPI_Type_set_attr(mpi_datatype, type_keyval, const_cast<void *>(attribute_val));
  OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
  if (MPI::Datatype::mpi_type_map[mpi_datatype] == 0) {
      MPI::Datatype::mpi_type_map[mpi_datatype] = (Datatype*) this;
  }
  OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}
