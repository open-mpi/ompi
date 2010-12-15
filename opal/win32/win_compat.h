/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_WIN_COMPAT_H
#define OMPI_WIN_COMPAT_H

/**
 * don't complain about all the deprecated functions.
 */
#define _CRT_SECURE_NO_DEPRECATE

/**
 * Allow usage of some recent functions (such as SwitchToThread)
 *  0x0400 - for SwitchToThread
 *  0x0500 - for using Event Objects
 */
#define _WIN32_WINNT 0x0502

/**
 * Windows does not define the exact same names in stat.h
 * Redefine them to our names.
 * Supposedly, links are not available
 */
#define S_IFLNK        0xFFFF          /* identifies the file as a symbolic link */
#define S_IXUSR        _S_IEXEC        /* execute/search permission, owner */
#define S_IRUSR        _S_IREAD        /* read permission, owner */
#define S_IWUSR        _S_IWRITE       /* write permission, owner */

/**
 * Define it in order to get access to the "secure" version of rand.
 */
#define _CRT_RAND_S

/* It is always better to include windows.h with the lean and mean option. 
   So, include it with that option and then include some which are required 
   for us in ompi. Note: this file is included only on windows */

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif  /* WIN32_LEAN_AND_MEAN */
#ifndef VC_EXTRALEAN
#define VC_EXTRALEAN
#endif  /* VC_EXTRALEAN */
#include <windows.h>

/* FD_SETSIZE determines how many sockets windows can select() on. If not defined 
   before including winsock2.h, it is defined to be 64. We are going to go ahead and
   make it 1024 for now. PLEASE CHECK IF THIS IS RIGHT */
#define FD_SETSIZE 1024

/* other utility header files */
#include <shellapi.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <process.h>
#include <signal.h>
#include <conio.h>
#include <fcntl.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

/**
 * For all file io operations
 */
#include <direct.h>
#include <io.h>

#include <stdlib.h>
/* for alloca */
#include <malloc.h>

#if defined(OMPI_BUILDING) && OMPI_BUILDING
#include "opal/win32/ompi_uio.h"
#include "opal/win32/ompi_time.h"
#include "opal/win32/ompi_utsname.h"
#include "opal/win32/ompi_util.h"
#include "opal/win32/ompi_misc.h"
#include "opal/win32/ompi_inet.h"
#include "opal/win32/ompi_socket.h"
#endif

#define MAXPATHLEN _MAX_PATH
#define MAXHOSTNAMELEN _MAX_PATH
#define PATH_MAX _MAX_PATH
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
typedef unsigned short mode_t;
typedef long ssize_t;
typedef DWORD in_port_t;
typedef char* caddr_t;
typedef unsigned int uint;

/* Defines for the access functions */
#define F_OK  0x00
#define R_OK  0x04
#define W_OK  0x02
#define X_OK  R_OK  /* no execution right on Windows */
#define S_IRWXU (_S_IREAD | _S_IWRITE | _S_IEXEC)

#define WTERMSIG(EXIT_CODE)    (1)
#define WIFEXITED(EXIT_CODE)   (1)
#define WEXITSTATUS(EXIT_CODE) (EXIT_CODE)
#define WIFSIGNALED(EXIT_CODE) (0)
#define WIFSTOPPED(EXIT_CODE)  (0)
#define WSTOPSIG(EXIT_CODE)    (11)

/**
 * Microsoft compiler complain about non conformance of the default UNIX function.
 * Non conformance to the POSIX standard, and they suggest to use the version
 * starting with an _ instead. So, in order to keep cl.exe happy (and quiet) we can
 * use the followings defines.
 */
#define getpid                    _getpid
#define strdup                    _strdup
#define putenv                    _putenv
#define getcwd                    _getcwd
#define mkdir(PATH, MODE)         _mkdir((PATH))
#define rmdir                     _rmdir
#define chdir                     _chdir
#define chmod                     _chmod
#define access                    _access
#define open                      _open
#define close                     _close
#define unlink                    _unlink
#define dup2                      _dup2
#define dup                       _dup
#define write                     _write 
#define read                      _read 
#define fileno                    _fileno 
#define isatty                    _isatty 
#define execvp                    _execvp
#define pipe(array_fd)            _pipe(array_fd, 1024, O_BINARY )
#define S_ISDIR(STAT_MODE)        ((STAT_MODE) & _S_IFDIR)
#define S_ISREG(STAT_MODE)        ((STAT_MODE) & _S_IFREG)
#define strncasecmp               _strnicmp
#define strcasecmp                _stricmp
#define umask                     _umask
#define getch                     _getch
#define random                    rand
#define strtok_r                  strtok_s
#define srand48                   srand
#define lrand48                   rand
#define nanosleep(tp, rem)        Sleep(*tp.tv_sec*1000+*tp.tv_nsec/1000000)

#ifndef UINT32_MAX
#define UINT32_MAX            _UI32_MAX
#endif
#ifndef UINT32_MIN
#define UINT32_MIN            _UI32_MIN
#endif
#ifndef INT32_MAX
#define INT32_MAX             _I32_MAX
#endif
#ifndef INT32_MIN
#define INT32_MIN             _I32_MIN
#endif
#ifndef UINT16_MIN
#define UINT16_MIN            _UI16_MIN
#endif
#ifndef UINT16_MAX
#define UINT16_MAX            _UI16_MAX
#endif
#ifndef INT16_MIN
#define INT16_MIN             _I16_MIN
#endif
#ifndef INT16_MAX
#define INT16_MAX             _I16_MAX
#endif
#ifndef UINT8_MAX
#define UINT8_MAX             _UI8_MAX
#endif
#ifndef UINT8_MIN
#define UINT8_MIN             _UI8_MIN
#endif

/* Make sure we let the compiler know that we support __func__ */
#if !defined(HAVE_DECL___FUNC__)
#define HAVE_DECL___FUNC__ 1
#endif

/* Microsoft claim that strdup is deprecated and that we should use _strdup. */
/*#define strdup _strdup*/
/*#define strncpy strncpy_s*/
/*#define sprintf sprintf_s*/

/* Ugly signal mapping since windows doesn't support the full spectrum
 * just a very small subset... :/
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vclib/html/_crt_raise.asp
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dnucmg/html/UCMGch09.asp
 */
#define SIGCHLD SIGILL
#define SIGKILL WM_QUIT
#define SIGPIPE 13
#define SIGCONT 18
#define SIGSTOP 19
#define SIGTSTP 20

/* Note: 
 *   The two defines below are likely to break the orte_wait
 *   functionality. The proper method of replacing these bits
 *   of functionality is left for further investigated.
 */
#define WUNTRACED 0
#define WNOHANG   0

#define sigset_t int
#define in_addr_t uint32_t

/* Need to define _Bool here for different version of VS. 
   The definition in opal_config_bottom.h won't help, 
   as long as we have a mixed C and C++ projects in one solution. */
#if defined(_MSC_VER) && _MSC_VER < 1600
#define _Bool BOOL
#else
#define _Bool bool
#endif

/*
 * No syslog.h on Windows, but these have to be defined somehow.
 * There could also be a notifier component later for Windows.
 */
#define LOG_EMERG   0
#define LOG_ALERT   1
#define LOG_CRIT    2
#define LOG_ERR     3
#define LOG_WARNING 4
#define LOG_NOTICE  5
#define LOG_INFO    6
#define LOG_DEBUG   7


/*
 * Mask these to Windows equivalents
 */
#define bzero(p, l) memset(p, 0, l)
#define bcopy(s, t, l) memmove(t, s, l)

#endif /* compat */
