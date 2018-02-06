/*
 * Copyright © 2013-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_CUDA_CUDA_RUNTIME_API_H
#define HWLOC_PORT_CUDA_CUDA_RUNTIME_API_H

/* we need to replace any CUDA-related #define that configure may have put in private/autogen/config.h */
#ifndef HWLOC_CONFIGURE_H
#error cuda_runtime_api.h must be included after private/autogen/config.h
#endif
#undef HWLOC_HAVE_CUDA_L2CACHESIZE
#define HWLOC_HAVE_CUDA_L2CACHESIZE 1

typedef unsigned cudaError_t;

struct cudaDeviceProp {
  char name[256];
  int pciDomainID;
  int pciBusID;
  int pciDeviceID;
  size_t totalGlobalMem;
  size_t sharedMemPerBlock;
  int major;
  int minor;
  int l2CacheSize;
  int multiProcessorCount;
};

cudaError_t cudaGetDeviceProperties(struct cudaDeviceProp *, int);
cudaError_t cudaGetDeviceCount(int *);

#endif /* HWLOC_PORT_CUDA_CUDA_RUNTIME_API_H */
