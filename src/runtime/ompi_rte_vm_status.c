/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file **/


#include "ompi_config.h"
#include <stdio.h>
#include <string.h>

#include "include/constants.h"

#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/bufpack.h"

#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "runtime/runtime.h"


ompi_rte_vm_status_t
*ompi_rte_get_vm_status(mca_ns_base_cellid_t cellid, char *nodename)
{
    char *tokens[3];
    ompi_registry_value_t *value;
    ompi_rte_vm_status_t *stat_ptr;
    ompi_list_t *returned_list;
    
    tokens[0] = ompi_name_server.convert_cellid_to_string(cellid);
    tokens[1] = strdup(nodename);
    tokens[2] = NULL;
    
    returned_list = ompi_registry.get(OMPI_REGISTRY_XAND, OMPI_RTE_VM_STATUS_SEGMENT, tokens);
    
    free(tokens[0]);
    free(tokens[1]);
    
    if (NULL != (value = (ompi_registry_value_t*)ompi_list_remove_first(returned_list))) {
        stat_ptr = ompi_rte_unpack_vm_status(value);
        OBJ_RELEASE(value);
        OBJ_RELEASE(returned_list);
        return stat_ptr;
    }
    
    return NULL;
}


int ompi_rte_set_vm_status(ompi_rte_vm_status_t *status)
{
    char *tokens[3];
    void *addr;
    int size;
    ompi_buffer_t buffer;
    
    tokens[0] = ompi_name_server.convert_cellid_to_string(status->cell);
    tokens[1] = strdup(status->nodename);
    tokens[2] = NULL;
    
    /* create the buffer to store the status information */
    ompi_buffer_init(&buffer, 0);
    ompi_pack(buffer, &status->cell, 1, OMPI_CELLID);
    ompi_pack_string(buffer, status->nodename);
    ompi_pack(buffer, &status->num_cpus, 1, OMPI_INT16);
    ompi_pack(buffer, &status->mem_size, 1, OMPI_INT32);
    ompi_pack_string(buffer, status->arch);
    ompi_pack_string(buffer, status->op_sys);
    ompi_pack_string(buffer, status->release);
    ompi_pack_string(buffer, status->user);
    ompi_pack_string(buffer, status->group);
    ompi_pack(buffer, &status->permission, 1, OMPI_INT32);
    ompi_pack(buffer, &status->state, 1, OMPI_NODE_STATE);

    /* peek the buffer and resulting size */
    ompi_buffer_get(buffer, &addr, &size);

    ompi_registry.put(OMPI_REGISTRY_XAND | OMPI_REGISTRY_OVERWRITE,
             OMPI_RTE_VM_STATUS_SEGMENT, tokens, addr, size);

    /* cleanup */
    free(tokens[0]);
    free(tokens[1]);
    ompi_buffer_free(buffer);

    return OMPI_SUCCESS;
}


ompi_rte_vm_status_t
*ompi_rte_unpack_vm_status(ompi_registry_value_t *value)
{
    ompi_buffer_t buffer;
    ompi_rte_vm_status_t *stat_ptr;
    
    stat_ptr = (ompi_rte_vm_status_t*)malloc(sizeof(ompi_rte_vm_status_t));
    
    /* transfer ownership of registry object to buffer and unpack */
    ompi_buffer_init_preallocated(&buffer, value->object, value->object_size);
    
    ompi_unpack(buffer, &stat_ptr->cell, 1, OMPI_CELLID);
    ompi_unpack_string(buffer, &stat_ptr->nodename);
    ompi_unpack(buffer, &stat_ptr->num_cpus, 1, OMPI_INT16);
    ompi_unpack(buffer, &stat_ptr->mem_size, 1, OMPI_INT32);
    ompi_unpack_string(buffer, &stat_ptr->arch);
    ompi_unpack_string(buffer, &stat_ptr->op_sys);
    ompi_unpack_string(buffer, &stat_ptr->release);
    ompi_unpack_string(buffer, &stat_ptr->user);
    ompi_unpack_string(buffer, &stat_ptr->group);
    ompi_unpack(buffer, &stat_ptr->permission, 1, OMPI_INT32);
    ompi_unpack(buffer, &stat_ptr->state, 1, OMPI_NODE_STATE);
    
    return stat_ptr;
}

int ompi_rte_vm_register(void)
{
    ompi_rte_vm_status_t status;
    int ret_code=OMPI_SUCCESS;
        
    status.cell = ompi_name_server.get_cellid(ompi_rte_get_self());
    status.nodename = strdup(ompi_system_info.nodename);
    status.arch = strdup(ompi_system_info.machine);
    status.op_sys = strdup(ompi_system_info.sysname);
    asprintf(&status.release, "%s %s", ompi_system_info.release,
                                        ompi_system_info.version);
    status.state = OMPI_NODE_UP;

#ifdef BPROC
    bproc_node_info_t info;
    int node;
    
    node = bproc_currnode();
    bproc_nodeinfo(node, &info);
    status.permission = (uint32_t)info.mode;
    asprintf(&status.user, "%0X", (int)info.user);
    asprintf(&status.group, "%0X", (int)info.group);
    status.node_address = info.addr;
    
    status.num_cpus = 2;
    status.mem_size = 1000;
    
#else
    status.permission = 0;
    status.user = strdup(ompi_system_info.user);
    status.group = strdup("unknown");
    status.node_address = 0;
    
    status.num_cpus = 1;
    status.mem_size = 500;
#endif  /* BPROC */

    ret_code = ompi_rte_set_vm_status(&status);
    
    /* ensure that segment ownership is set to "everyone" */
    ompi_registry.assign_ownership(OMPI_RTE_VM_STATUS_SEGMENT, MCA_NS_BASE_JOBID_MAX);

    return ret_code;
}



/* under windows, there is a SystemInfo structure that contains following info:
 * Private Declare Sub GetSystemInfo Lib "kernel32" (lpSystemInfo _
As SYSTEM_INFO)

Private Type SYSTEM_INFO
        dwOemID As Long
        dwPageSize As Long
        lpMinimumApplicationAddress As Long
        lpMaximumApplicationAddress As Long
        dwActiveProcessorMask As Long
        dwNumberOfProcessors As Long
        dwProcessorType As Long
        dwAllocationGranularity As Long
        dwReserved As Long
End Type

Public Enum etProcessorType
    PROCESSOR_INTEL_386 = 386
    PROCESSOR_INTEL_486 = 486
    PROCESSOR_INTEL_PENTIUM = 586
    PROCESSOR_MIPS_R4000 = 4000
    PROCESSOR_ALPHA_21064 = 21064
End Enum

Private m_typSystemInfo As SYSTEM_INFO

* Under Linux and BSD, get it from the /proc/cpuinfo file. There will be info for each cpu in node.


* Under IRIX, sysmp() is supposed to gather various multiprocessor-related functionality. 
* The most commonly-used request is PGSIZE, which returns the memory page size. There are 
* also requests to get well-known kernel structure offsets in /dev/kmem (KERNADDR), or the 
* number of available processors (NPROCS). All of the requests are defined in 
* IRIX's <sys/sysmp.h>.


Not sure what to do about Mac yet...
*/
