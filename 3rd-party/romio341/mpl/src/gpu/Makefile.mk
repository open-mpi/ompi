##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

lib@MPLLIBNAME@_la_SOURCES += src/gpu/mpl_gpu_common.c

if MPL_HAVE_CUDA
lib@MPLLIBNAME@_la_SOURCES += src/gpu/mpl_gpu_cuda.c
else
if MPL_HAVE_ZE
lib@MPLLIBNAME@_la_SOURCES += src/gpu/mpl_gpu_ze.c
else
lib@MPLLIBNAME@_la_SOURCES += src/gpu/mpl_gpu_fallback.c
endif
endif
