#ifndef _BTL_PCIE_DDRIVER_H
#define _BTL_PCIE_DDRIVER_H

typedef struct DD_adapter_handle
{
  int local_sma_size;
  int remote_sma_size;
  void* local_sma_address;
  void* remote_sma_address;
  int fd;
  void *cmd_block;
} DD_adapter_handle;

#include <asm/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

#include <linux/axon_ioctl.h>

#endif
