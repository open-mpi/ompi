/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_WIN_COMPAT_H
#define OMPI_WIN_COMPAT_H

/* It is always better to include windows.h with the lean and mean option. 
   So, include it with that option and then include some which are required 
   for us in ompi. Note: this file is included only on windows */

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif

/* other utility header files */
#include <cderr.h>
#include <dde.h>
#include <ddeml.h>
#include <dlgs.h>
#include <imm.h>
#include <lzexpand.h>
#include <mmsystem.h>
#include <nb30.h>
#include <rpc.h>
#include <shellapi.h>
#include <winperf.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <process.h>
#include <io.h>
#include "win32/ompi_uio.h"
#include "win32/ompi_time.h"
#include "win32/ompi_utsname.h"
#include "win32/ompi_util.h"
#include "win32/ompi_misc.h"
#include "win32/ompi_get_error.h"
#include "util/printf.h"

#define MAXPATHLEN MAX_PATH
#define MAXHOSTNAMELEN MAX_PATH
typedef unsigned short mode_t;
typedef long ssize_t;
typedef DWORD in_port_t;

/* Anju: some random #defines which I know offhand, but need to configure it */
#define OMPI_ALIGNMENT_CXX_BOOL OMPI_ALIGNMENT_INT
#define SIZEOF_BOOL SIZEOF_INT
#define getpid _getpid
#define getcwd _getcwd
#define mkdir _mkdir

#define UINT32_MAX _UI32_MAX

#endif /* compat */
