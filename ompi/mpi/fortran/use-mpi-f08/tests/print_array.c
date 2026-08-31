/* SPDX-License-Identifier: BSD-3-Clause-Open-MPI */
#include <stdio.h>

void print_array(int * A, int count)
{
   int i;
   for (i = 0; i < count; i++) {
      printf("%d ", A[i]);
   }
   printf("\n");
}
