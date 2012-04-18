/**
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * send_t.c - tests mpi_send variants
 *
 * Actual tests do not use MPI, they just test parameters and types.
 */

#include <stdio.h>

#define MPI_COMM_WORLD 0
#define MPI_COMM_SELF  1

#define MPI_INTEGER   7
#define MPI_INTEGER1  8


void mpi_send_1di(int* a, int* count, int* datatype,
		  int* dest, int* tag, int* comm, int* ierr)
{
  int i;

  *ierr = 0;
  for (i = 0; i < *count; i++) {
    if (a[i] != 10 - i) {
      printf("a[%d] = %d\n", i, a[i]);
      *ierr = 1;
    }
  }
  if (*count != 10) *ierr = 1;
  if (*datatype != MPI_INTEGER) *ierr = 1;
  if (*dest != 59) *ierr = 1;
  if (*tag != 999) *ierr = 1;
  if (*comm != MPI_COMM_WORLD) *ierr = 1;
}


void mpi_send_1di1(char* a, int* count, int* datatype,
		   int* dest, int* tag, int* comm, int* ierr)
{
  int i;

  *ierr = 0;
  for (i = 0; i < *count; i++) {
    if (a[i] != i+1) {
      printf("a[%d] = %d\n", i, a[i]);
      *ierr = 1;
    }
  }
  if (*count != 10) *ierr = 1;
  if (*datatype != MPI_INTEGER1) *ierr = 1;
  if (*dest != 59) *ierr = 1;
  if (*tag != 999) *ierr = 1;
  if (*comm != MPI_COMM_SELF) *ierr = 1;
}

