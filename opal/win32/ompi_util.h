/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
  
#ifndef OMPI_UTIL_H
#define OMPI_UTIL_H

static __inline int getpagesize(void)
{
    SYSTEM_INFO sys_info;

    GetSystemInfo(&sys_info);
    return (int)sys_info.dwPageSize;
}

/**
 * Case insensitive comparaison of strings.
 */
static __inline int strncasecmp( const char *s1, const char *s2, int n)
{
    return _strnicmp( s1, s2, (size_t)n );
}

static __inline int strcasecmp(char *s1, char *s2)
{
    return _stricmp(s1, s2);
}

#endif
