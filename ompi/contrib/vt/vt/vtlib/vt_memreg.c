/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_memreg.h"

#include "vt_trc.h"

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int vt_mem_regid[VT__MEM_REGID_NUM];

void vt_mem_register()
{
  uint32_t fid;

  fid = vt_def_file("MEM");
  
  vt_mem_regid[VT__MEM_MALLOC] =
    vt_def_region("malloc", fid, VT_NO_LNO, VT_NO_LNO, "MEM", VT_FUNCTION);
  vt_mem_regid[VT__MEM_REALLOC] =
    vt_def_region("realloc", fid, VT_NO_LNO, VT_NO_LNO, "MEM", VT_FUNCTION);
  vt_mem_regid[VT__MEM_FREE] =
    vt_def_region("free", fid, VT_NO_LNO, VT_NO_LNO, "MEM", VT_FUNCTION);
}
