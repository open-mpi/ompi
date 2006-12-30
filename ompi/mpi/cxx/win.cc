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
MPI::Win::Free()
{
    MPI_Win save = mpi_win;
    (void) MPI_Win_free(&mpi_win);
    
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Win::mpi_win_map.erase(save);
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}
 
void 
MPI::Win::Set_errhandler(const MPI::Errhandler& errhandler)
{
    my_errhandler = (MPI::Errhandler *)&errhandler;
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::Win::mpi_win_map[mpi_win] = this;
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
    (void)MPI_Win_set_errhandler(mpi_win, errhandler);
}

int
MPI::Win::Create_keyval(MPI::Win::Copy_attr_function* win_copy_attr_fn, 
                        MPI::Win::Delete_attr_function* win_delete_attr_fn, 
                        void* extra_state)
{
  int keyval;
  (void) MPI_Win_create_keyval(ompi_mpi_cxx_win_copy_attr_intercept,
                               ompi_mpi_cxx_win_delete_attr_intercept,
                               &keyval, extra_state);
  key_pair_t* copy_and_delete = 
      new key_pair_t(win_copy_attr_fn, win_delete_attr_fn); 
  OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
  MPI::Win::mpi_win_key_fn_map[keyval] = copy_and_delete;
  OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
  return keyval;
}


void 
MPI::Win::Free_keyval(int& win_keyval)
{
  int save = win_keyval;
  (void) MPI_Win_free_keyval(&win_keyval);
  OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
  MPI::Win::mpi_win_key_fn_map.erase(save);
  OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}


void 
MPI::Win::Set_attr(int win_keyval, const void* attribute_val) 
{
  (void) MPI_Win_set_attr(mpi_win, win_keyval, const_cast<void *>(attribute_val));
  OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
  if (MPI::Win::mpi_win_map[mpi_win] == 0) {
      MPI::Win::mpi_win_map[mpi_win] = (Win*) this;
  }
  OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}
