/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <errno.h>
#include <sched.h>

#include "vt_error.h"
#include "vt_getcpu.h"
#include "vt_trc.h"

#include "otf.h"

/* counter id */
uint32_t vt_getcpu_cid = 0;

void vt_getcpu_init()
{
  vt_assert(vt_misc_cgid != 0);

  /* write counter definition */
  vt_getcpu_cid = vt_def_counter("CPU_ID",
                                 OTF_COUNTER_TYPE_ABS|OTF_COUNTER_SCOPE_NEXT,
                                 vt_misc_cgid, "#");
}

void vt_getcpu_finalize()
{
}

void vt_getcpu_read(uint32_t* value, uint8_t* changed)
{
  int cpuid;
  *changed = 0;

  if ( (cpuid = sched_getcpu()) == -1 )
    vt_error_msg("sched_getcpu: %s", strerror(errno));

  if( (uint32_t)cpuid != *value )
  {
    *value = (uint32_t)cpuid;
    *changed = 1;
  }
}
