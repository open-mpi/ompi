/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/* To try to get all declarations duplicated below.  */
#define _WIN32_WINNT 0x0601

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <windows.h>

#ifndef HAVE_KAFFINITY
typedef ULONG_PTR KAFFINITY, *PKAFFINITY;
#endif

#ifndef HAVE_PROCESSOR_CACHE_TYPE
typedef enum _PROCESSOR_CACHE_TYPE {
  CacheUnified,
  CacheInstruction,
  CacheData,
  CacheTrace
} PROCESSOR_CACHE_TYPE;
#endif

#ifndef CACHE_FULLY_ASSOCIATIVE
#define CACHE_FULLY_ASSOCIATIVE 0xFF
#endif

#ifndef MAXIMUM_PROC_PER_GROUP /* missing in MinGW */
#define MAXIMUM_PROC_PER_GROUP 64
#endif

#ifndef HAVE_CACHE_DESCRIPTOR
typedef struct _CACHE_DESCRIPTOR {
  BYTE Level;
  BYTE Associativity;
  WORD LineSize;
  DWORD Size; /* in bytes */
  PROCESSOR_CACHE_TYPE Type;
} CACHE_DESCRIPTOR, *PCACHE_DESCRIPTOR;
#endif

#ifndef HAVE_LOGICAL_PROCESSOR_RELATIONSHIP
typedef enum _LOGICAL_PROCESSOR_RELATIONSHIP {
  RelationProcessorCore,
  RelationNumaNode,
  RelationCache,
  RelationProcessorPackage,
  RelationGroup,
  RelationAll = 0xffff
} LOGICAL_PROCESSOR_RELATIONSHIP;
#else /* HAVE_LOGICAL_PROCESSOR_RELATIONSHIP */
#  ifndef HAVE_RELATIONPROCESSORPACKAGE
#    define RelationProcessorPackage 3
#    define RelationGroup 4
#    define RelationAll 0xffff
#  endif /* HAVE_RELATIONPROCESSORPACKAGE */
#endif /* HAVE_LOGICAL_PROCESSOR_RELATIONSHIP */

#ifndef HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION
typedef struct _SYSTEM_LOGICAL_PROCESSOR_INFORMATION {
  ULONG_PTR ProcessorMask;
  LOGICAL_PROCESSOR_RELATIONSHIP Relationship;
  _ANONYMOUS_UNION
  union {
    struct {
      BYTE flags;
    } ProcessorCore;
    struct {
      DWORD NodeNumber;
    } NumaNode;
    CACHE_DESCRIPTOR Cache;
    ULONGLONG Reserved[2];
  } DUMMYUNIONNAME;
} SYSTEM_LOGICAL_PROCESSOR_INFORMATION, *PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
#endif

/* Extended interface, for group support */

#ifndef HAVE_GROUP_AFFINITY
typedef struct _GROUP_AFFINITY {
  KAFFINITY Mask;
  WORD Group;
  WORD Reserved[3];
} GROUP_AFFINITY, *PGROUP_AFFINITY;
#endif

#ifndef HAVE_PROCESSOR_RELATIONSHIP
typedef struct _PROCESSOR_RELATIONSHIP {
  BYTE Flags;
  BYTE Reserved[21];
  WORD GroupCount;
  GROUP_AFFINITY GroupMask[ANYSIZE_ARRAY];
} PROCESSOR_RELATIONSHIP, *PPROCESSOR_RELATIONSHIP;
#endif

#ifndef HAVE_NUMA_NODE_RELATIONSHIP
typedef struct _NUMA_NODE_RELATIONSHIP {
  DWORD NodeNumber;
  BYTE Reserved[20];
  GROUP_AFFINITY GroupMask;
} NUMA_NODE_RELATIONSHIP, *PNUMA_NODE_RELATIONSHIP;
#endif

#ifndef HAVE_CACHE_RELATIONSHIP
typedef struct _CACHE_RELATIONSHIP {
  BYTE Level;
  BYTE Associativity;
  WORD LineSize;
  DWORD CacheSize;
  PROCESSOR_CACHE_TYPE Type;
  BYTE Reserved[20];
  GROUP_AFFINITY GroupMask;
} CACHE_RELATIONSHIP, *PCACHE_RELATIONSHIP;
#endif

#ifndef HAVE_PROCESSOR_GROUP_INFO
typedef struct _PROCESSOR_GROUP_INFO {
  BYTE MaximumProcessorCount;
  BYTE ActiveProcessorCount;
  BYTE Reserved[38];
  KAFFINITY ActiveProcessorMask;
} PROCESSOR_GROUP_INFO, *PPROCESSOR_GROUP_INFO;
#endif

#ifndef HAVE_GROUP_RELATIONSHIP
typedef struct _GROUP_RELATIONSHIP {
  WORD MaximumGroupCount;
  WORD ActiveGroupCount;
  ULONGLONG Reserved[2];
  PROCESSOR_GROUP_INFO GroupInfo[ANYSIZE_ARRAY];
} GROUP_RELATIONSHIP, *PGROUP_RELATIONSHIP;
#endif

#ifndef HAVE_SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX
typedef struct _SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX {
  LOGICAL_PROCESSOR_RELATIONSHIP Relationship;
  DWORD Size;
  _ANONYMOUS_UNION
  union {
    PROCESSOR_RELATIONSHIP Processor;
    NUMA_NODE_RELATIONSHIP NumaNode;
    CACHE_RELATIONSHIP Cache;
    GROUP_RELATIONSHIP Group;
    /* Odd: no member to tell the cpu mask of the package... */
  } DUMMYUNIONNAME;
} SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX, *PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX;
#endif

#ifndef HAVE_PSAPI_WORKING_SET_EX_BLOCK
typedef union _PSAPI_WORKING_SET_EX_BLOCK {
  ULONG_PTR Flags;
  struct {
    unsigned Valid  :1;
    unsigned ShareCount  :3;
    unsigned Win32Protection  :11;
    unsigned Shared  :1;
    unsigned Node  :6;
    unsigned Locked  :1;
    unsigned LargePage  :1;
  };
} PSAPI_WORKING_SET_EX_BLOCK;
#endif

#ifndef HAVE_PSAPI_WORKING_SET_EX_INFORMATION
typedef struct _PSAPI_WORKING_SET_EX_INFORMATION {
  PVOID VirtualAddress;
  PSAPI_WORKING_SET_EX_BLOCK VirtualAttributes;
} PSAPI_WORKING_SET_EX_INFORMATION;
#endif

#ifndef HAVE_PROCESSOR_NUMBER
typedef struct _PROCESSOR_NUMBER {
  WORD Group;
  BYTE Number;
  BYTE Reserved;
} PROCESSOR_NUMBER, *PPROCESSOR_NUMBER;
#endif

/* Function pointers */

typedef WORD (WINAPI *PFN_GETACTIVEPROCESSORGROUPCOUNT)(void);
static PFN_GETACTIVEPROCESSORGROUPCOUNT GetActiveProcessorGroupCountProc;

static unsigned long nr_processor_groups = 1;

typedef WORD (WINAPI *PFN_GETACTIVEPROCESSORCOUNT)(WORD);
static PFN_GETACTIVEPROCESSORCOUNT GetActiveProcessorCountProc;

typedef DWORD (WINAPI *PFN_GETCURRENTPROCESSORNUMBER)(void);
static PFN_GETCURRENTPROCESSORNUMBER GetCurrentProcessorNumberProc;

typedef VOID (WINAPI *PFN_GETCURRENTPROCESSORNUMBEREX)(PPROCESSOR_NUMBER);
static PFN_GETCURRENTPROCESSORNUMBEREX GetCurrentProcessorNumberExProc;

typedef BOOL (WINAPI *PFN_GETLOGICALPROCESSORINFORMATION)(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION Buffer, PDWORD ReturnLength);
static PFN_GETLOGICALPROCESSORINFORMATION GetLogicalProcessorInformationProc;

typedef BOOL (WINAPI *PFN_GETLOGICALPROCESSORINFORMATIONEX)(LOGICAL_PROCESSOR_RELATIONSHIP relationship, PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX Buffer, PDWORD ReturnLength);
static PFN_GETLOGICALPROCESSORINFORMATIONEX GetLogicalProcessorInformationExProc;

typedef BOOL (WINAPI *PFN_SETTHREADGROUPAFFINITY)(HANDLE hThread, const GROUP_AFFINITY *GroupAffinity, PGROUP_AFFINITY PreviousGroupAffinity);
static PFN_SETTHREADGROUPAFFINITY SetThreadGroupAffinityProc;

typedef BOOL (WINAPI *PFN_GETTHREADGROUPAFFINITY)(HANDLE hThread, PGROUP_AFFINITY GroupAffinity);
static PFN_GETTHREADGROUPAFFINITY GetThreadGroupAffinityProc;

typedef BOOL (WINAPI *PFN_GETNUMAAVAILABLEMEMORYNODE)(UCHAR Node, PULONGLONG AvailableBytes);
static PFN_GETNUMAAVAILABLEMEMORYNODE GetNumaAvailableMemoryNodeProc;

typedef BOOL (WINAPI *PFN_GETNUMAAVAILABLEMEMORYNODEEX)(USHORT Node, PULONGLONG AvailableBytes);
static PFN_GETNUMAAVAILABLEMEMORYNODEEX GetNumaAvailableMemoryNodeExProc;

typedef LPVOID (WINAPI *PFN_VIRTUALALLOCEXNUMA)(HANDLE hProcess, LPVOID lpAddress, SIZE_T dwSize, DWORD flAllocationType, DWORD flProtect, DWORD nndPreferred);
static PFN_VIRTUALALLOCEXNUMA VirtualAllocExNumaProc;

typedef BOOL (WINAPI *PFN_VIRTUALFREEEX)(HANDLE hProcess, LPVOID lpAddress, SIZE_T dwSize, DWORD dwFreeType);
static PFN_VIRTUALFREEEX VirtualFreeExProc;

typedef BOOL (WINAPI *PFN_QUERYWORKINGSETEX)(HANDLE hProcess, PVOID pv, DWORD cb);
static PFN_QUERYWORKINGSETEX QueryWorkingSetExProc;

static void hwloc_win_get_function_ptrs(void)
{
  static int done = 0;
  if (!done) {
    HMODULE kernel32;

    kernel32 = LoadLibrary("kernel32.dll");
    if (kernel32) {
      GetActiveProcessorGroupCountProc =
	(PFN_GETACTIVEPROCESSORGROUPCOUNT) GetProcAddress(kernel32, "GetActiveProcessorGroupCount");
      GetActiveProcessorCountProc =
	(PFN_GETACTIVEPROCESSORCOUNT) GetProcAddress(kernel32, "GetActiveProcessorCount");
      GetLogicalProcessorInformationProc =
	(PFN_GETLOGICALPROCESSORINFORMATION) GetProcAddress(kernel32, "GetLogicalProcessorInformation");
      GetCurrentProcessorNumberProc =
	(PFN_GETCURRENTPROCESSORNUMBER) GetProcAddress(kernel32, "GetCurrentProcessorNumber");
      GetCurrentProcessorNumberExProc =
	(PFN_GETCURRENTPROCESSORNUMBEREX) GetProcAddress(kernel32, "GetCurrentProcessorNumberEx");
      SetThreadGroupAffinityProc =
	(PFN_SETTHREADGROUPAFFINITY) GetProcAddress(kernel32, "SetThreadGroupAffinity");
      GetThreadGroupAffinityProc =
	(PFN_GETTHREADGROUPAFFINITY) GetProcAddress(kernel32, "GetThreadGroupAffinity");
      GetNumaAvailableMemoryNodeProc =
	(PFN_GETNUMAAVAILABLEMEMORYNODE) GetProcAddress(kernel32, "GetNumaAvailableMemoryNode");
      GetNumaAvailableMemoryNodeExProc =
	(PFN_GETNUMAAVAILABLEMEMORYNODEEX) GetProcAddress(kernel32, "GetNumaAvailableMemoryNodeEx");
      GetLogicalProcessorInformationExProc =
	(PFN_GETLOGICALPROCESSORINFORMATIONEX)GetProcAddress(kernel32, "GetLogicalProcessorInformationEx");
      VirtualAllocExNumaProc =
	(PFN_VIRTUALALLOCEXNUMA) GetProcAddress(kernel32, "K32QueryWorkingSetEx");
      VirtualAllocExNumaProc =*
	(PFN_VIRTUALALLOCEXNUMA) GetProcAddress(kernel32, "VirtualAllocExNuma");
      VirtualFreeExProc =
	(PFN_VIRTUALFREEEX) GetProcAddress(kernel32, "VirtualFreeEx");
    }

    if (GetActiveProcessorGroupCountProc)
      nr_processor_groups = GetActiveProcessorGroupCountProc();

    if (!VirtualAllocExNumaProc) {
      HMODULE psapi = LoadLibrary("psapi.dll");
      if (psapi)
        VirtualAllocExNumaProc = (PFN_VIRTUALALLOCEXNUMA) GetProcAddress(psapi, "QueryWorkingSetEx");
    }

    done = 1;
  }
}

/*
 * ULONG_PTR and DWORD_PTR are 64/32bits depending on the arch
 * while bitmaps use unsigned long (always 32bits)
 */

static void hwloc_bitmap_from_ULONG_PTR(hwloc_bitmap_t set, ULONG_PTR mask)
{
#if SIZEOF_VOID_P == 8
  hwloc_bitmap_from_ulong(set, mask & 0xffffffff);
  hwloc_bitmap_set_ith_ulong(set, 1, mask >> 32);
#else
  hwloc_bitmap_from_ulong(set, mask);
#endif
}

static void hwloc_bitmap_from_ith_ULONG_PTR(hwloc_bitmap_t set, unsigned i, ULONG_PTR mask)
{
#if SIZEOF_VOID_P == 8
  hwloc_bitmap_from_ith_ulong(set, 2*i, mask & 0xffffffff);
  hwloc_bitmap_set_ith_ulong(set, 2*i+1, mask >> 32);
#else
  hwloc_bitmap_from_ith_ulong(set, i, mask);
#endif
}

static void hwloc_bitmap_set_ith_ULONG_PTR(hwloc_bitmap_t set, unsigned i, ULONG_PTR mask)
{
#if SIZEOF_VOID_P == 8
  hwloc_bitmap_set_ith_ulong(set, 2*i, mask & 0xffffffff);
  hwloc_bitmap_set_ith_ulong(set, 2*i+1, mask >> 32);
#else
  hwloc_bitmap_set_ith_ulong(set, i, mask);
#endif
}

static ULONG_PTR hwloc_bitmap_to_ULONG_PTR(hwloc_const_bitmap_t set)
{
#if SIZEOF_VOID_P == 8
  ULONG_PTR up = hwloc_bitmap_to_ith_ulong(set, 1);
  up <<= 32;
  up |= hwloc_bitmap_to_ulong(set);
  return up;
#else
  return hwloc_bitmap_to_ulong(set);
#endif
}

static ULONG_PTR hwloc_bitmap_to_ith_ULONG_PTR(hwloc_const_bitmap_t set, unsigned i)
{
#if SIZEOF_VOID_P == 8
  ULONG_PTR up = hwloc_bitmap_to_ith_ulong(set, 2*i+1);
  up <<= 32;
  up |= hwloc_bitmap_to_ith_ulong(set, 2*i);
  return up;
#else
  return hwloc_bitmap_to_ith_ulong(set, i);
#endif
}

/* convert set into index+mask if all set bits are in the same ULONG.
 * otherwise return -1.
 */
static int hwloc_bitmap_to_single_ULONG_PTR(hwloc_const_bitmap_t set, unsigned *index, ULONG_PTR *mask)
{
  unsigned first_ulp, last_ulp;
  if (hwloc_bitmap_weight(set) == -1)
    return -1;
  first_ulp = hwloc_bitmap_first(set) / (sizeof(ULONG_PTR)*8);
  last_ulp = hwloc_bitmap_last(set) / (sizeof(ULONG_PTR)*8);
  if (first_ulp != last_ulp)
    return -1;
  *mask = hwloc_bitmap_to_ith_ULONG_PTR(set, first_ulp);
  *index = first_ulp;
  return 0;
}

/**************************************************************
 * hwloc PU numbering with respect to Windows processor groups
 *
 * Everywhere below we reserve 64 physical indexes per processor groups because that's
 * the maximum (MAXIMUM_PROC_PER_GROUP). Windows may actually use less bits than that
 * in some groups (either to avoid splitting NUMA nodes across groups, or because of OS
 * tweaks such as "bcdedit /set groupsize 8") but we keep some unused indexes for simplicity.
 * That means PU physical indexes and cpusets may be non-contigous.
 * That also means hwloc_fallback_nbprocessors() below must return the last PU index + 1
 * instead the actual number of processors.
 */

/********************
 * last_cpu_location
 */

static int
hwloc_win_get_thisthread_last_cpu_location(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_cpuset_t set, int flags __hwloc_attribute_unused)
{
  assert(GetCurrentProcessorNumberExProc || (GetCurrentProcessorNumberProc && nr_processor_groups == 1));

  if (nr_processor_groups > 1 || !GetCurrentProcessorNumberProc) {
    PROCESSOR_NUMBER num;
    GetCurrentProcessorNumberExProc(&num);
    hwloc_bitmap_from_ith_ULONG_PTR(set, num.Group, ((ULONG_PTR)1) << num.Number);
    return 0;
  }

  hwloc_bitmap_from_ith_ULONG_PTR(set, 0, ((ULONG_PTR)1) << GetCurrentProcessorNumberProc());
  return 0;
}

/* TODO: hwloc_win_get_thisproc_last_cpu_location() using
 * CreateToolhelp32Snapshot(), Thread32First/Next()
 * th.th32OwnerProcessID == GetCurrentProcessId() for filtering within process
 * OpenThread(THREAD_SET_INFORMATION|THREAD_QUERY_INFORMATION, FALSE, te32.th32ThreadID) to get a handle.
 */


/******************************
 * set cpu/membind for threads
 */

/* TODO: SetThreadIdealProcessor{,Ex} */

static int
hwloc_win_set_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t thread, hwloc_const_bitmap_t hwloc_set, int flags)
{
  DWORD_PTR mask;
  unsigned group;

  if (flags & HWLOC_CPUBIND_NOMEMBIND) {
    errno = ENOSYS;
    return -1;
  }

  if (hwloc_bitmap_to_single_ULONG_PTR(hwloc_set, &group, &mask) < 0) {
    errno = ENOSYS;
    return -1;
  }

  assert(nr_processor_groups == 1 || SetThreadGroupAffinityProc);

  if (nr_processor_groups > 1) {
    GROUP_AFFINITY aff;
    memset(&aff, 0, sizeof(aff)); /* we get Invalid Parameter error if Reserved field isn't cleared */
    aff.Group = group;
    aff.Mask = mask;
    if (!SetThreadGroupAffinityProc(thread, &aff, NULL))
      return -1;

  } else {
    /* SetThreadAffinityMask() only changes the mask inside the current processor group */
    /* The resulting binding is always strict */
    if (!SetThreadAffinityMask(thread, mask))
      return -1;
  }
  return 0;
}

/* TODO: SetThreadGroupAffinity to get affinity */

static int
hwloc_win_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_win_set_thread_cpubind(topology, GetCurrentThread(), hwloc_set, flags);
}

static int
hwloc_win_set_thisthread_membind(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  int ret;
  hwloc_cpuset_t cpuset;

  if ((policy != HWLOC_MEMBIND_DEFAULT && policy != HWLOC_MEMBIND_BIND)
      || flags & HWLOC_MEMBIND_NOCPUBIND) {
    errno = ENOSYS;
    return -1;
  }

  cpuset = hwloc_bitmap_alloc();
  hwloc_cpuset_from_nodeset(topology, cpuset, nodeset);
  ret = hwloc_win_set_thisthread_cpubind(topology, cpuset, flags & HWLOC_MEMBIND_STRICT?HWLOC_CPUBIND_STRICT:0);
  hwloc_bitmap_free(cpuset);
  return ret;
}


/******************************
 * get cpu/membind for threads
 */

  static int
hwloc_win_get_thread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_thread_t thread, hwloc_cpuset_t set, int flags __hwloc_attribute_unused)
{
  GROUP_AFFINITY aff;

  assert(GetThreadGroupAffinityProc);

  if (!GetThreadGroupAffinityProc(thread, &aff))
    return -1;
  hwloc_bitmap_from_ith_ULONG_PTR(set, aff.Group, aff.Mask);
  return 0;
}

static int
hwloc_win_get_thisthread_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_cpuset_t set, int flags __hwloc_attribute_unused)
{
  return hwloc_win_get_thread_cpubind(topology, GetCurrentThread(), set, flags);
}

static int
hwloc_win_get_thisthread_membind(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags)
{
  int ret;
  hwloc_cpuset_t cpuset = hwloc_bitmap_alloc();
  ret = hwloc_win_get_thread_cpubind(topology, GetCurrentThread(), cpuset, flags);
  if (!ret) {
    *policy = HWLOC_MEMBIND_BIND;
    hwloc_cpuset_to_nodeset(topology, cpuset, nodeset);
  }
  hwloc_bitmap_free(cpuset);
  return ret;
}


/********************************
 * set cpu/membind for processes
 */

static int
hwloc_win_set_proc_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_pid_t proc, hwloc_const_bitmap_t hwloc_set, int flags)
{
  DWORD_PTR mask;

  assert(nr_processor_groups == 1);

  if (flags & HWLOC_CPUBIND_NOMEMBIND) {
    errno = ENOSYS;
    return -1;
  }

  /* TODO: SetThreadGroupAffinity() for all threads doesn't enforce the whole process affinity,
   * maybe because of process-specific resource locality */
  /* TODO: if we are in a single group (check with GetProcessGroupAffinity()),
   * SetProcessAffinityMask() changes the binding within that same group.
   */
  /* TODO: NtSetInformationProcess() works very well for binding to any mask in a single group,
   * but it's an internal routine.
   */
  /* TODO: checks whether hwloc-bind.c needs to pass INHERIT_PARENT_AFFINITY to CreateProcess() instead of execvp(). */

  /* The resulting binding is always strict */
  mask = hwloc_bitmap_to_ULONG_PTR(hwloc_set);
  if (!SetProcessAffinityMask(proc, mask))
    return -1;
  return 0;
}

static int
hwloc_win_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_win_set_proc_cpubind(topology, GetCurrentProcess(), hwloc_set, flags);
}

static int
hwloc_win_set_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  int ret;
  hwloc_cpuset_t cpuset;

  if ((policy != HWLOC_MEMBIND_DEFAULT && policy != HWLOC_MEMBIND_BIND)
      || flags & HWLOC_MEMBIND_NOCPUBIND) {
    errno = ENOSYS;
    return -1;
  }

  cpuset = hwloc_bitmap_alloc();
  hwloc_cpuset_from_nodeset(topology, cpuset, nodeset);
  ret = hwloc_win_set_proc_cpubind(topology, pid, cpuset, flags & HWLOC_MEMBIND_STRICT?HWLOC_CPUBIND_STRICT:0);
  hwloc_bitmap_free(cpuset);
  return ret;
}

static int
hwloc_win_set_thisproc_membind(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  return hwloc_win_set_proc_membind(topology, GetCurrentProcess(), nodeset, policy, flags);
}


/********************************
 * get cpu/membind for processes
 */

static int
hwloc_win_get_proc_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_pid_t proc, hwloc_bitmap_t hwloc_set, int flags)
{
  DWORD_PTR proc_mask, sys_mask;

  assert(nr_processor_groups == 1);

  if (flags & HWLOC_CPUBIND_NOMEMBIND) {
    errno = ENOSYS;
    return -1;
  }

  /* TODO: if we are in a single group (check with GetProcessGroupAffinity()),
   * GetProcessAffinityMask() gives the mask within that group.
   */
  /* TODO: if we are in multiple groups, GetProcessGroupAffinity() gives their IDs,
   * but we don't know their masks.
   */
  /* TODO: GetThreadGroupAffinity() for all threads can be smaller than the whole process affinity,
   * maybe because of process-specific resource locality.
   */

  if (!GetProcessAffinityMask(proc, &proc_mask, &sys_mask))
    return -1;
  hwloc_bitmap_from_ULONG_PTR(hwloc_set, proc_mask);
  return 0;
}

static int
hwloc_win_get_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags)
{
  int ret;
  hwloc_cpuset_t cpuset = hwloc_bitmap_alloc();
  ret = hwloc_win_get_proc_cpubind(topology, pid, cpuset, flags & HWLOC_MEMBIND_STRICT?HWLOC_CPUBIND_STRICT:0);
  if (!ret) {
    *policy = HWLOC_MEMBIND_BIND;
    hwloc_cpuset_to_nodeset(topology, cpuset, nodeset);
  }
  hwloc_bitmap_free(cpuset);
  return ret;
}

static int
hwloc_win_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_cpuset, int flags)
{
  return hwloc_win_get_proc_cpubind(topology, GetCurrentProcess(), hwloc_cpuset, flags);
}

static int
hwloc_win_get_thisproc_membind(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags)
{
  return hwloc_win_get_proc_membind(topology, GetCurrentProcess(), nodeset, policy, flags);
}


/************************
 * membind alloc/free
 */

static void *
hwloc_win_alloc(hwloc_topology_t topology __hwloc_attribute_unused, size_t len) {
  return VirtualAlloc(NULL, len, MEM_COMMIT|MEM_RESERVE, PAGE_EXECUTE_READWRITE);
}

static void *
hwloc_win_alloc_membind(hwloc_topology_t topology __hwloc_attribute_unused, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags) {
  int node;

  switch (policy) {
    case HWLOC_MEMBIND_DEFAULT:
    case HWLOC_MEMBIND_BIND:
      break;
    default:
      errno = ENOSYS;
      return hwloc_alloc_or_fail(topology, len, flags);
  }

  if (flags & HWLOC_MEMBIND_STRICT) {
    errno = ENOSYS;
    return NULL;
  }

  if (hwloc_bitmap_weight(nodeset) != 1) {
    /* Not a single node, can't do this */
    errno = EXDEV;
    return hwloc_alloc_or_fail(topology, len, flags);
  }

  node = hwloc_bitmap_first(nodeset);
  return VirtualAllocExNumaProc(GetCurrentProcess(), NULL, len, MEM_COMMIT|MEM_RESERVE, PAGE_EXECUTE_READWRITE, node);
}

static int
hwloc_win_free_membind(hwloc_topology_t topology __hwloc_attribute_unused, void *addr, size_t len __hwloc_attribute_unused) {
  if (!addr)
    return 0;
  if (!VirtualFreeExProc(GetCurrentProcess(), addr, 0, MEM_RELEASE))
    return -1;
  return 0;
}


/**********************
 * membind for areas
 */

static int
hwloc_win_get_area_membind(hwloc_topology_t topology __hwloc_attribute_unused, const void *addr, size_t len, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags)
{
  SYSTEM_INFO SystemInfo;
  DWORD page_size;
  uintptr_t start;
  unsigned nb;

  GetSystemInfo(&SystemInfo);
  page_size = SystemInfo.dwPageSize;

  start = (((uintptr_t) addr) / page_size) * page_size;
  nb = (unsigned)((((uintptr_t) addr + len - start) + page_size - 1) / page_size);

  if (!nb)
    nb = 1;

  {
    PSAPI_WORKING_SET_EX_INFORMATION *pv;
    unsigned i;

    pv = calloc(nb, sizeof(*pv));

    for (i = 0; i < nb; i++)
      pv[i].VirtualAddress = (void*) (start + i * page_size);
    if (!QueryWorkingSetExProc(GetCurrentProcess(), pv, nb * sizeof(*pv))) {
      free(pv);
      return -1;
    }
    *policy = HWLOC_MEMBIND_BIND;
    if (flags & HWLOC_MEMBIND_STRICT) {
      unsigned node = pv[0].VirtualAttributes.Node;
      for (i = 1; i < nb; i++) {
	if (pv[i].VirtualAttributes.Node != node) {
	  errno = EXDEV;
          free(pv);
	  return -1;
	}
      }
      hwloc_bitmap_only(nodeset, node);
      free(pv);
      return 0;
    }
    hwloc_bitmap_zero(nodeset);
    for (i = 0; i < nb; i++)
      hwloc_bitmap_set(nodeset, pv[i].VirtualAttributes.Node);
    free(pv);
    return 0;
  }
}


/*************************
 * discovery
 */

static int
hwloc_look_windows(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  hwloc_bitmap_t groups_pu_set = NULL;
  SYSTEM_INFO SystemInfo;
  DWORD length;

  hwloc_win_get_function_ptrs();

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return 0;

  hwloc_alloc_obj_cpusets(topology->levels[0][0]);

  GetSystemInfo(&SystemInfo);

  if (!GetLogicalProcessorInformationExProc && GetLogicalProcessorInformationProc) {
      PSYSTEM_LOGICAL_PROCESSOR_INFORMATION procInfo;
      unsigned id;
      unsigned i;
      struct hwloc_obj *obj;
      hwloc_obj_type_t type;

      length = 0;
      procInfo = NULL;

      while (1) {
	if (GetLogicalProcessorInformationProc(procInfo, &length))
	  break;
	if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
	  return -1;
	procInfo = realloc(procInfo, length);
      }

      assert(!length || procInfo);

      for (i = 0; i < length / sizeof(*procInfo); i++) {

        /* Ignore unknown caches */
	if (procInfo->Relationship == RelationCache
		&& procInfo->Cache.Type != CacheUnified
		&& procInfo->Cache.Type != CacheData
		&& procInfo->Cache.Type != CacheInstruction)
	  continue;

	id = -1;
	switch (procInfo[i].Relationship) {
	  case RelationNumaNode:
	    type = HWLOC_OBJ_NUMANODE;
	    id = procInfo[i].NumaNode.NodeNumber;
	    break;
	  case RelationProcessorPackage:
	    type = HWLOC_OBJ_PACKAGE;
	    break;
	  case RelationCache:
	    type = HWLOC_OBJ_CACHE;
	    break;
	  case RelationProcessorCore:
	    type = HWLOC_OBJ_CORE;
	    break;
	  case RelationGroup:
	  default:
	    type = HWLOC_OBJ_GROUP;
	    break;
	}

	obj = hwloc_alloc_setup_object(type, id);
        obj->cpuset = hwloc_bitmap_alloc();
	hwloc_debug("%s#%u mask %lx\n", hwloc_obj_type_string(type), id, procInfo[i].ProcessorMask);
	/* ProcessorMask is a ULONG_PTR */
	hwloc_bitmap_set_ith_ULONG_PTR(obj->cpuset, 0, procInfo[i].ProcessorMask);
	hwloc_debug_2args_bitmap("%s#%u bitmap %s\n", hwloc_obj_type_string(type), id, obj->cpuset);

	switch (type) {
	  case HWLOC_OBJ_NUMANODE:
	    {
	      ULONGLONG avail;
	      obj->nodeset = hwloc_bitmap_alloc();
	      hwloc_bitmap_set(obj->nodeset, id);
	      if ((GetNumaAvailableMemoryNodeExProc && GetNumaAvailableMemoryNodeExProc(id, &avail))
	       || (GetNumaAvailableMemoryNodeProc && GetNumaAvailableMemoryNodeProc(id, &avail)))
		obj->memory.local_memory = avail;
	      obj->memory.page_types_len = 2;
	      obj->memory.page_types = malloc(2 * sizeof(*obj->memory.page_types));
	      memset(obj->memory.page_types, 0, 2 * sizeof(*obj->memory.page_types));
	      obj->memory.page_types_len = 1;
	      obj->memory.page_types[0].size = SystemInfo.dwPageSize;
#ifdef HAVE__SC_LARGE_PAGESIZE
	      obj->memory.page_types_len++;
	      obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
	      break;
	    }
	  case HWLOC_OBJ_CACHE:
	    obj->attr->cache.size = procInfo[i].Cache.Size;
	    obj->attr->cache.associativity = procInfo[i].Cache.Associativity == CACHE_FULLY_ASSOCIATIVE ? -1 : procInfo[i].Cache.Associativity ;
	    obj->attr->cache.linesize = procInfo[i].Cache.LineSize;
	    obj->attr->cache.depth = procInfo[i].Cache.Level;
	    switch (procInfo->Cache.Type) {
	      case CacheUnified:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
		break;
	      case CacheData:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
		break;
	      case CacheInstruction:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
		break;
	      default:
		hwloc_free_unlinked_object(obj);
		continue;
	    }
	    break;
	  case HWLOC_OBJ_GROUP:
	    obj->attr->group.depth = procInfo[i].Relationship == RelationGroup;
	    break;
	  default:
	    break;
	}
	hwloc_insert_object_by_cpuset(topology, obj);
      }

      free(procInfo);
  }

  if (GetLogicalProcessorInformationExProc) {
      PSYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX procInfoTotal, procInfo;

      unsigned id;
      struct hwloc_obj *obj;
      hwloc_obj_type_t type;

      length = 0;
      procInfoTotal = NULL;

      while (1) {
	if (GetLogicalProcessorInformationExProc(RelationAll, procInfoTotal, &length))
	  break;
	if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
	  return -1;
        procInfoTotal = realloc(procInfoTotal, length);
      }

      for (procInfo = procInfoTotal;
	   (void*) procInfo < (void*) ((uintptr_t) procInfoTotal + length);
	   procInfo = (void*) ((uintptr_t) procInfo + procInfo->Size)) {
        unsigned num, i;
        GROUP_AFFINITY *GroupMask;

        /* Ignore unknown caches */
	if (procInfo->Relationship == RelationCache
		&& procInfo->Cache.Type != CacheUnified
		&& procInfo->Cache.Type != CacheData
		&& procInfo->Cache.Type != CacheInstruction)
	  continue;

	id = -1;
	switch (procInfo->Relationship) {
	  case RelationNumaNode:
	    type = HWLOC_OBJ_NUMANODE;
            num = 1;
            GroupMask = &procInfo->NumaNode.GroupMask;
	    id = procInfo->NumaNode.NodeNumber;
	    break;
	  case RelationProcessorPackage:
	    type = HWLOC_OBJ_PACKAGE;
            num = procInfo->Processor.GroupCount;
            GroupMask = procInfo->Processor.GroupMask;
	    break;
	  case RelationCache:
	    type = HWLOC_OBJ_CACHE;
            num = 1;
            GroupMask = &procInfo->Cache.GroupMask;
	    break;
	  case RelationProcessorCore:
	    type = HWLOC_OBJ_CORE;
            num = procInfo->Processor.GroupCount;
            GroupMask = procInfo->Processor.GroupMask;
	    break;
	  case RelationGroup:
	    /* So strange an interface... */
	    for (id = 0; id < procInfo->Group.ActiveGroupCount; id++) {
              KAFFINITY mask;
	      obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, id);
	      obj->cpuset = hwloc_bitmap_alloc();
	      mask = procInfo->Group.GroupInfo[id].ActiveProcessorMask;
	      hwloc_debug("group %u %d cpus mask %lx\n", id,
                  procInfo->Group.GroupInfo[id].ActiveProcessorCount, mask);
	      /* KAFFINITY is ULONG_PTR */
	      hwloc_bitmap_set_ith_ULONG_PTR(obj->cpuset, id, mask);
	      hwloc_debug_2args_bitmap("group %u %d bitmap %s\n", id, procInfo->Group.GroupInfo[id].ActiveProcessorCount, obj->cpuset);

	      /* save the set of PUs so that we can create them at the end */
	      if (!groups_pu_set)
		groups_pu_set = hwloc_bitmap_alloc();
	      hwloc_bitmap_or(groups_pu_set, groups_pu_set, obj->cpuset);

	      hwloc_insert_object_by_cpuset(topology, obj);
	    }
	    continue;
	  default:
	    /* Don't know how to get the mask.  */
            hwloc_debug("unknown relation %d\n", procInfo->Relationship);
	    continue;
	}

	obj = hwloc_alloc_setup_object(type, id);
        obj->cpuset = hwloc_bitmap_alloc();
        for (i = 0; i < num; i++) {
          hwloc_debug("%s#%u %d: mask %d:%lx\n", hwloc_obj_type_string(type), id, i, GroupMask[i].Group, GroupMask[i].Mask);
	  /* GROUP_AFFINITY.Mask is KAFFINITY, which is ULONG_PTR */
	  hwloc_bitmap_set_ith_ULONG_PTR(obj->cpuset, GroupMask[i].Group, GroupMask[i].Mask);
        }
	hwloc_debug_2args_bitmap("%s#%u bitmap %s\n", hwloc_obj_type_string(type), id, obj->cpuset);

	switch (type) {
	  case HWLOC_OBJ_NUMANODE:
	    {
	      ULONGLONG avail;
	      obj->nodeset = hwloc_bitmap_alloc();
	      hwloc_bitmap_set(obj->nodeset, id);
	      if ((GetNumaAvailableMemoryNodeExProc && GetNumaAvailableMemoryNodeExProc(id, &avail))
	       || (GetNumaAvailableMemoryNodeProc && GetNumaAvailableMemoryNodeProc(id, &avail)))
	        obj->memory.local_memory = avail;
	      obj->memory.page_types = malloc(2 * sizeof(*obj->memory.page_types));
	      memset(obj->memory.page_types, 0, 2 * sizeof(*obj->memory.page_types));
	      obj->memory.page_types_len = 1;
	      obj->memory.page_types[0].size = SystemInfo.dwPageSize;
#ifdef HAVE__SC_LARGE_PAGESIZE
	      obj->memory.page_types_len++;
	      obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
	      break;
	    }
	  case HWLOC_OBJ_CACHE:
	    obj->attr->cache.size = procInfo->Cache.CacheSize;
	    obj->attr->cache.associativity = procInfo->Cache.Associativity == CACHE_FULLY_ASSOCIATIVE ? -1 : procInfo->Cache.Associativity ;
	    obj->attr->cache.linesize = procInfo->Cache.LineSize;
	    obj->attr->cache.depth = procInfo->Cache.Level;
	    switch (procInfo->Cache.Type) {
	      case CacheUnified:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
		break;
	      case CacheData:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
		break;
	      case CacheInstruction:
		obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
		break;
	      default:
		hwloc_free_unlinked_object(obj);
		continue;
	    }
	    break;
	  default:
	    break;
	}
	hwloc_insert_object_by_cpuset(topology, obj);
      }
      free(procInfoTotal);
  }

  if (groups_pu_set) {
    /* the system supports multiple Groups.
     * PU indexes may be discontiguous, especially if Groups contain less than 64 procs.
     */
    hwloc_obj_t obj;
    unsigned idx;
    hwloc_bitmap_foreach_begin(idx, groups_pu_set) {
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, idx);
      obj->cpuset = hwloc_bitmap_alloc();
      hwloc_bitmap_only(obj->cpuset, idx);
      hwloc_debug_1arg_bitmap("cpu %u has cpuset %s\n",
			      idx, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    } hwloc_bitmap_foreach_end();
    hwloc_bitmap_free(groups_pu_set);
  } else {
    /* no processor groups */
    SYSTEM_INFO sysinfo;
    hwloc_obj_t obj;
    unsigned idx;
    GetSystemInfo(&sysinfo);
    for(idx=0; idx<32; idx++)
      if (sysinfo.dwActiveProcessorMask & (((DWORD_PTR)1)<<idx)) {
	obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, idx);
	obj->cpuset = hwloc_bitmap_alloc();
	hwloc_bitmap_only(obj->cpuset, idx);
	hwloc_debug_1arg_bitmap("cpu %u has cpuset %s\n",
				idx, obj->cpuset);
	hwloc_insert_object_by_cpuset(topology, obj);
      }
  }

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Windows");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology, NULL);
  return 1;
}

void
hwloc_set_windows_hooks(struct hwloc_binding_hooks *hooks,
			struct hwloc_topology_support *support)
{
  hwloc_win_get_function_ptrs();

  if (GetCurrentProcessorNumberExProc || (GetCurrentProcessorNumberProc && nr_processor_groups == 1))
    hooks->get_thisthread_last_cpu_location = hwloc_win_get_thisthread_last_cpu_location;

  if (nr_processor_groups == 1) {
    hooks->set_proc_cpubind = hwloc_win_set_proc_cpubind;
    hooks->get_proc_cpubind = hwloc_win_get_proc_cpubind;
    hooks->set_thisproc_cpubind = hwloc_win_set_thisproc_cpubind;
    hooks->get_thisproc_cpubind = hwloc_win_get_thisproc_cpubind;
    hooks->set_proc_membind = hwloc_win_set_proc_membind;
    hooks->get_proc_membind = hwloc_win_get_proc_membind;
    hooks->set_thisproc_membind = hwloc_win_set_thisproc_membind;
    hooks->get_thisproc_membind = hwloc_win_get_thisproc_membind;
  }
  if (nr_processor_groups == 1 || SetThreadGroupAffinityProc) {
    hooks->set_thread_cpubind = hwloc_win_set_thread_cpubind;
    hooks->set_thisthread_cpubind = hwloc_win_set_thisthread_cpubind;
    hooks->set_thisthread_membind = hwloc_win_set_thisthread_membind;
  }
  if (GetThreadGroupAffinityProc) {
    hooks->get_thread_cpubind = hwloc_win_get_thread_cpubind;
    hooks->get_thisthread_cpubind = hwloc_win_get_thisthread_cpubind;
    hooks->get_thisthread_membind = hwloc_win_get_thisthread_membind;
  }

  if (VirtualAllocExNumaProc) {
    hooks->alloc_membind = hwloc_win_alloc_membind;
    hooks->alloc = hwloc_win_alloc;
    hooks->free_membind = hwloc_win_free_membind;
    support->membind->bind_membind = 1;
  }

  if (QueryWorkingSetExProc)
    hooks->get_area_membind = hwloc_win_get_area_membind;
}

static struct hwloc_backend *
hwloc_windows_component_instantiate(struct hwloc_disc_component *component,
				    const void *_data1 __hwloc_attribute_unused,
				    const void *_data2 __hwloc_attribute_unused,
				    const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_windows;
  return backend;
}

static struct hwloc_disc_component hwloc_windows_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "windows",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_windows_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_windows_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_windows_disc_component
};

unsigned
hwloc_fallback_nbprocessors(struct hwloc_topology *topology) {
  int n;
  SYSTEM_INFO sysinfo;

  /* by default, ignore groups (return only the number in the current group) */
  GetSystemInfo(&sysinfo);
  n = sysinfo.dwNumberOfProcessors; /* FIXME could be non-contigous, rather return a mask from dwActiveProcessorMask? */

  hwloc_win_get_function_ptrs();

  if (nr_processor_groups > 1) {
    /* assume n-1 groups are complete, since that's how we store things in cpusets */
    if (GetActiveProcessorCountProc)
      n = MAXIMUM_PROC_PER_GROUP*(nr_processor_groups-1)
	+ GetActiveProcessorCountProc((WORD)nr_processor_groups-1);
    else
      n = MAXIMUM_PROC_PER_GROUP*nr_processor_groups;
  }

  if (n >= 1)
    topology->support.discovery->pu = 1;
  else
    n = 1;
  return n;
}
