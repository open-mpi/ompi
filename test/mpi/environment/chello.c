/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <mpi.h>
#include <stdio.h>

int
main(int argc, char* argv[])
{
  MPI_Init(&argc, &argv);

  printf("Hello, World\n");

  MPI_Finalize();

  return 0;
}
