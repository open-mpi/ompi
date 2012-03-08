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

#include "vt_cupti.h"       /* Support for CUPTI */
#include "vt_gpu.h"         /* common for GPU */

/*
 * Handles errors returned from CUPTI function calls.
 * 
 * @param ecode the CUDA driver API error code
 * @param msg a message to get more detailed information about the error
 * @param the corresponding file
 * @param the line the error occurred
 */
void vt_cupti_handleError(CUptiResult err, const char* msg,
                          const char *file, const int line)
{
  const char *errstr;
  
  if(msg != NULL) vt_cntl_msg(1, msg);
  
  cuptiGetResultString(err, &errstr);
  
  if(vt_gpu_error){
    vt_error_msg("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }else{
    vt_warning("[CUPTI] %s:%d:'%s'", file, line, errstr);
  }
}
