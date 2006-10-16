//
// Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
//
// Sample MPI "hello world" application in C++
//

// MPI's C++ bindings unfortunately redefined some POSIX constants
// (SEEK_SET and friends).  Fortunately, most versions of <iostream>
// and <stdio.h> are sane enough that if you include "mpi.h" first,
// they won't redefine the problematic values.  Note, however, that
// this means that you should not make calls to fseek(3) (and friends)
// in a C++ source file that includes <mpi.h>!

#include <iostream>
#include "mpi.h"

int main(int argc, char **argv)
{
    int rank, size;

    MPI::Init();
    rank = MPI::COMM_WORLD.Get_rank();
    size = MPI::COMM_WORLD.Get_size();
    std::cout << "Hello, world!  I am " << rank << " of " << size << std::endl;
    MPI::Finalize();

    return 0;
}
