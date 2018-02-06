/*
 * Copyright © 2013-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_OPENCL_CL_CL_EXT_H
#define HWLOC_PORT_OPENCL_CL_CL_EXT_H

typedef char cl_char;
typedef unsigned long cl_ulong;
typedef unsigned int cl_uint;
typedef int cl_int;
typedef unsigned int cl_device_type;
typedef unsigned int cl_platform_info;
typedef unsigned int cl_device_info;

struct _cl_platform_id;
typedef struct _cl_platform_id * cl_platform_id;
struct _cl_device_id;
typedef struct _cl_device_id * cl_device_id;

#define CL_SUCCESS 0

cl_int clGetPlatformIDs(cl_uint, cl_platform_id *, cl_uint *);
cl_int clGetDeviceIDs(cl_platform_id, cl_device_type, cl_uint, cl_device_id *, cl_uint *);
cl_int clGetDeviceInfo(cl_device_id, cl_device_info, size_t, void *, size_t *);
cl_int clGetPlatformInfo(cl_platform_id, cl_platform_info, size_t, void *, size_t *);

#define CL_DEVICE_TYPE_CPU                          (1 << 1)
#define CL_DEVICE_TYPE_GPU                          (1 << 2)
#define CL_DEVICE_TYPE_ACCELERATOR                  (1 << 3)
#define CL_DEVICE_TYPE_CUSTOM                       (1 << 4)
#define CL_DEVICE_TYPE_ALL                          0xFFFFFFFF

#define CL_PLATFORM_NAME                            0x0902
#define CL_DEVICE_TYPE                              0x1000
#define CL_DEVICE_MAX_COMPUTE_UNITS                 0x1002
#define CL_DEVICE_GLOBAL_MEM_SIZE                   0x101F
#define CL_DEVICE_NAME                              0x102B
#define CL_DEVICE_VENDOR                            0x102C
#define CL_DEVICE_PLATFORM                          0x1031

#define CL_DEVICE_TOPOLOGY_AMD                      0x4037
typedef union {
  struct { cl_uint type; cl_uint data[5]; } raw;
  struct { cl_uint type; cl_char unused[17]; cl_char bus; cl_char device; cl_char function; } pcie;
} cl_device_topology_amd;
#define CL_DEVICE_TOPOLOGY_TYPE_PCIE_AMD            1

#define CL_DEVICE_BOARD_NAME_AMD                    0x4038

#endif /* HWLOC_PORT_OPENCL_CL_CL_EXT_H */
