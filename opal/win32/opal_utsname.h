/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
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

#ifndef OPAL_UTSNAME_H
#define OPAL_UTSNAME_H

#include "opal_config.h"

#define OPAL_UTSNAME_LEN 64

struct utsname {
    char sysname[OPAL_UTSNAME_LEN];
    char nodename[OPAL_UTSNAME_LEN];
    char release[OPAL_UTSNAME_LEN];
    char version[OPAL_UTSNAME_LEN];
    char machine[OPAL_UTSNAME_LEN];
};

BEGIN_C_DECLS
    OPAL_DECLSPEC int uname(struct utsname *un);
END_C_DECLS

#endif /* OPAL_UTSNAME_H */
