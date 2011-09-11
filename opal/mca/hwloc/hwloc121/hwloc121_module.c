/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 */
#include "opal_config.h"
#include "opal/constants.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif


#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/queue.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WIN32
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "opal/class/opal_object.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/fd.h"
#include "opal/mca/base/mca_base_param.h"

#include "hwloc121.h"
#include "opal/mca/hwloc/base/base.h"

