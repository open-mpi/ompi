

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
