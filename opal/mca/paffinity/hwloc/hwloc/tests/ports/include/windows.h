/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_WINDOWS_H
#define HWLOC_PORT_WINDOWS_H

#include <inttypes.h>

#define DECLARE_HANDLE(n) typedef struct n##__ {int i;} *n
DECLARE_HANDLE(HINSTANCE);
typedef HINSTANCE HMODULE;
typedef int WINBOOL, BOOL;
typedef int64_t LONGLONG;
typedef uint64_t DWORDLONG;
typedef DWORDLONG ULONGLONG, *PULONGLONG;
typedef unsigned char BYTE, UCHAR;
typedef unsigned short WORD, USHORT;
typedef unsigned long ULONG_PTR, DWORD_PTR, DWORD, *PDWORD, *PDWORD_PTR;
typedef const char *LPCSTR;
typedef int (*FARPROC)();
typedef void *PVOID;

// This is to cope with linux using integers for hwloc_pid_t and hwloc_thread_t
//typedef PVOID HANDLE;
typedef int HANDLE;

#ifdef __GNUC__
#define _ANONYMOUS_UNION __extension__
#define _ANONYMOUS_STRUCT __extension__
#else
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#endif /* __GNUC__ */
#define DUMMYUNIONNAME
#define WINAPI

#define ANYSIZE_ARRAY 1

#define ERROR_INSUFFICIENT_BUFFER 122L

WINAPI HINSTANCE LoadLibrary(LPCSTR);
WINAPI void *GetProcAddress(HINSTANCE, LPCSTR);
WINAPI DWORD GetLastError(void);

DWORD_PTR WINAPI SetThreadAffinityMask(HANDLE hThread, DWORD_PTR dwThreadAffinityMask);
BOOL WINAPI SetProcessAffinityMask(HANDLE hProcess, DWORD_PTR dwProcessAffinityMask);
BOOL WINAPI GetProcessAffinityMask(HANDLE hProcess, PDWORD_PTR lpProcessAffinityMask, PDWORD_PTR lpSystemAffinityMask);

HANDLE WINAPI GetCurrentThread(void);
HANDLE WINAPI GetCurrentProcess(void);

BOOL GetNumaAvailableMemoryNode(UCHAR Node, PULONGLONG AvailableBytes);


#endif /* HWLOC_PORT_WINDOWS_H */
