# Open MPI extension: shortfloat

Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.

This extension provides additional MPI datatypes `MPIX_SHORT_FLOAT`,
`MPIX_C_SHORT_FLOAT_COMPLEX`, and `MPIX_CXX_SHORT_FLOAT_COMPLEX`,
which are proposed with the `MPI_` prefix in June 2017 for proposal in
the MPI 4.0 standard. As of February 2019, it is not accepted yet.

See https://github.com/mpi-forum/mpi-issues/issues/65 for moe details

Each MPI datatype corresponds to the C/C++ type `short float`, the C
type `short float _Complex`, and the C++ type `std::complex<short
float>`, respectively.

In addition, this extension provides a datatype `MPIX_C_FLOAT16` for
the C type `_Float16`, which is defined in ISO/IEC JTC 1/SC 22/WG 14
N1945 (ISO/IEC TS 18661-3:2015). This name and meaning are same as
that of MPICH.  See https://github.com/pmodels/mpich/pull/3455.

This extension is enabled only if the C compiler supports `short float`
or `_Float16`, or the `--enable-alt-short-float=TYPE` option is passed
to the Open MPI `configure` script.

NOTE: The Clang 6.0.x and 7.0.x compilers support the `_Float16` type
(via software emulation), but require an additional linker flag to
function properly.  If you wish to enable Clang 6.0.x or 7.0.x's
software emulation of `_Float16`, use the following CLI options to Open
MPI configure script:

```
./configure \
        LDFLAGS=--rtlib=compiler-rt \
        --with-wrapper-ldflags=--rtlib=compiler-rt ...
```
