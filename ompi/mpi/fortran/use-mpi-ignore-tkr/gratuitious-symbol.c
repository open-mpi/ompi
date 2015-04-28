/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Declare the function below so that compilers don't complain about
   the lack of prototype. */
int ompi_mpi_ignore_tkr_module_abi_symbol(void);

/*
 * This function is located in libmpi_usempi_ignore_tkr.la.
 *
 * This function's only purpose in life is to be called from the dummy
 * library libmpi_usempi.la (that is created in this directory -- see
 * ompi/mpi/fortran/README-v1.8-ABI.txt for details) to establish a
 * dependency between libmpi_usempi.la and
 * libmpi_usempi_ignore_tkr.la.
 */
int ompi_mpi_ignore_tkr_module_abi_symbol(void)
{
    return -1;
}
