/*
 * $HEADER$
 */
/** @file **/

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"

#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "util/pack.h"

#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "runtime/runtime.h"

int ompi_vm_register()
{
    ompi_buffer_t buffer;
    int ret_code;
    int32_t num;
    char *keys[2];

    if (OMPI_SUCCESS != ompi_buffer_init(&buffer, 0)) {
	ret_code = OMPI_ERROR;
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(buffer, ompi_system_info.nodename)) {
	ret_code = OMPI_ERROR;
	goto ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(buffer, ompi_process_info.name, 1, OMPI_NAME)) {
	ret_code = OMPI_ERROR;
	goto ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(buffer, mca_oob_get_contact_info())) {
	ret_code = OMPI_ERROR;
	goto ERROR;
    }

    if (0 == strncmp(ompi_system_info.sysname, "Darwin", strlen("Darwin"))) {
	num = 1;
    } else if (0 == strncmp(ompi_system_info.sysname, "Linux", strlen("Linux"))) {
	/* get it from proc/cpuinfo */
	num = 1;
    } else {
	num = 1;
    }

    if (OMPI_SUCCESS != ompi_pack(buffer, &num, 1, OMPI_INT32)) {
	ret_code = OMPI_ERROR;
	goto ERROR;
    }

    keys[0] = ompi_name_server.get_proc_name_string(ompi_process_info.name);
    keys[1] = NULL;

    ret_code = ompi_registry.put(OMPI_REGISTRY_XAND, "ompi-vm", keys, buffer, sizeof(buffer));

 ERROR:
    ompi_buffer_free(buffer);
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


* Under IRIX, sysmp() is supposed to gather various multiprocessor-related functionality. The most commonly-used request is PGSIZE, which returns the memory page size. There are also requests to get well-known kernel structure offsets in /dev/kmem (KERNADDR), or the number of available processors (NPROCS). All of the requests are defined in IRIX's <sys/sysmp.h>.


Not sure what to do about Mac yet...
*/
