/*
 * Copyright (c) 2019      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

// This dummy function is required for the following reason.
//
//   - The libmpiext_shortfloat_c.la file must be built because the
//     OMPI_EXT_MAKE_LISTS macro in the config/ompi_ext.m4 file adds
//     the file to the list of the OMPI_MPIEXT_C_LIBS output variable
//     and the ompi/Makefile.am file uses the output variable.
//   - The ar command of OS X refuses to create an archive file which
//     does not contain any object files.
//
// See https://github.com/open-mpi/ompi/pull/6205#issuecomment-466363071

void mpiext_shortfloat_dummy(void);

void mpiext_shortfloat_dummy() {
}
