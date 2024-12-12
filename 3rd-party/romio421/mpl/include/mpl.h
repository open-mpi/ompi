/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_H_INCLUDED
#define MPL_H_INCLUDED

#include "mpl_base.h"
#include "mpl_err.h"
/* must come before mpltrmem.h */
#include "mpl_valgrind.h"
#include "mpl_argstr.h"
#include "mpl_arg_serial.h"
#include "mpl_atomic.h"
#include "mpl_str.h"
#include "mpl_trmem.h"
#include "mpl_env.h"
#include "mpl_sock.h"
#include "mpl_sockaddr.h"
#include "mpl_msg.h"
#include "mpl_iov.h"
#include "mpl_bt.h"
#include "mpl_thread.h"
#include "mpl_timer.h"
#include "mpl_yield.h"
#include "mpl_dbg.h"
#include "mpl_shm.h"
#include "mpl_math.h"
#include "mpl_proc_mutex.h"
#include "mpl_gpu.h"
#include "mpl_gavl.h"
#include "mpl_initlock.h"
#include "mpl_misc.h"

int MPL_rankmap_str_to_array(char *mapping, int sz, int *out_nodemap);
int MPL_rankmap_array_to_str(int *nodemap, int sz, char **out_mapping_str);

#endif /* MPL_H_INCLUDED */
