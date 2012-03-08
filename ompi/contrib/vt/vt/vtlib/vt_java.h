/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_JAVA_H
#define _VT_JAVA_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_jvmti.h"

typedef struct
{
  jvmtiEnv*     jvmti;
  jint          jvmti_version;
  jrawMonitorID lock;
  jboolean      vm_is_started;
  jboolean      vm_is_initialized;
  jboolean      vm_is_dead;

} VTJVMAgent;

EXTERN void vt_java_get_thread_name(jvmtiEnv* jvmti, jthread thread,
                                    char* tname, int maxlen);

EXTERN void vt_java_check_error(jvmtiEnv* jvmti, jvmtiError error,
                                const char* str);

EXTERN VTJVMAgent* vt_jvmti_agent;

#endif /* _VT_JAVA_H */



















