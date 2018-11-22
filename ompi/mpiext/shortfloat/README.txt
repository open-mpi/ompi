Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.

$COPYRIGHT$

This extension provides additional MPI datatypes MPIX_SHORT_FLOAT,
MPIX_C_SHORT_FLOAT_COMPLEX, and MPIX_CXX_SHORT_FLOAT_COMPLEX, which
are proposed with the MPI_ prefix in June 2017 for proposal in the
MPI 4.0 standard. As of February 2019, it is not accepted yet.

  https://github.com/mpi-forum/mpi-issues/issues/65

Each MPI datatype corresponds to the C/C++ type 'short float', the C type
'short float _Complex', and the C++ type 'std::complex<short float>',
respectively.

This extension is enabled only if the C compiler supports 'short float'
or '_Float16', or the '--enable-alt-short-float=TYPE' option is passed
to the configure script.
