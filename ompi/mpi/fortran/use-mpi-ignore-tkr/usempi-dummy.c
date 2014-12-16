/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * This function is located in the dummy library libmpi_usempi.la.
 *
 * All it does is call a dummy function in libmpi_usempi_ignore_tkr.la
 * in order to establish a dependency between these two libraries in
 * the shared library case.
 *
 * See ompi/mpi/fortran/README-v1.8-ABI.txt for details.
 */
int ompi_mpi_tkr_module_abi_symbol(void)
{
    /* Call a dummy function in libmpi_usempi_ignore_tkr.la */
    extern int ompi_mpi_ignore_tkr_module_abi_symbol(void);
    return ompi_mpi_ignore_tkr_module_abi_symbol();
}
