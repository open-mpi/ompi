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
MPI::File::Close() 
{
    MPI_File save = mpi_file;
    (void) MPI_File_close(&mpi_file);

    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::File::mpi_file_map.erase(save);
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
}

  
void
MPI::File::Set_errhandler(const MPI::Errhandler& errhandler)
{
    my_errhandler = (MPI::Errhandler *)&errhandler;
    OPAL_THREAD_LOCK(MPI::mpi_map_mutex);
    MPI::File::mpi_file_map[mpi_file] = this;
    OPAL_THREAD_UNLOCK(MPI::mpi_map_mutex);
    (void)MPI_File_set_errhandler(mpi_file, errhandler);
}
