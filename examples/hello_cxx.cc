//
// Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
//
// Sample MPI "hello world" application in C++
//
// NOTE: The MPI C++ bindings were deprecated in MPI-2.2 and removed
// from the standard in MPI-3.  Open MPI still provides C++ MPI
// bindings, but they are no longer built by default (and may be
// removed in a future version of Open MPI).  You must
// --enable-mpi-cxx when configuring Open MPI to enable the MPI C++
// bindings.
//

#include "mpi.h"
#include <iostream>

int main(int argc, char **argv)
{
    int rank, size, len;
    char version[MPI_MAX_LIBRARY_VERSION_STRING];

    MPI::Init();
    rank = MPI::COMM_WORLD.Get_rank();
    size = MPI::COMM_WORLD.Get_size();
    MPI_Get_library_version(version, &len);
    std::cout << "Hello, world!  I am " << rank << " of " << size
              << "(" << version << ", " << len << ")" << std::endl;
    MPI::Finalize();

    return 0;
}
