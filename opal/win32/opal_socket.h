/*
 * Copyright (c) 2010      High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_SOCKET_H
#define OPAL_SOCKET_H

#include "opal_config.h"

#ifndef OPAL_WIN_COMPAT_H
#error This file is supposed to be included only from win_compat.h
#endif  /* OPAL_WIN_COMPAT_H */


BEGIN_C_DECLS

OPAL_DECLSPEC int create_socketpair(int d, int type, int protocol, int sv[2]);

END_C_DECLS

#endif /* OPAL_SOCKET_H */
