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

#include "vt_comp.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if (defined (VT_OMPI) || defined (VT_OMP))
#  include <omp.h>
#endif

struct s1 {
	long l1;
        long l2;
	double d1;
	double d2;
        long isseen;
	char *c;
	void *p1;
	long lineno;
	void *p2;
	struct s1 *p3;
        int fid;
        int rid;
	char *file;
	char *rout;
};

static int   rou_init = 1;  /* initialization necessary ? */

/*
 *-----------------------------------------------------------------------------
 * called during program initialization
 *-----------------------------------------------------------------------------
 */

void __rouinit() {
  int i;

  VT_MEMHOOKS_OFF();

  /* open trace file */
  vt_open();

  if (rou_init)
    {
      rou_init = 0;
    }

  VT_MEMHOOKS_ON();
}

/*
 *-----------------------------------------------------------------------------
 * called at the beginning of each profiled routine
 *----------------------------------------------------------------------------- */

void ___rouent2(struct s1 *p) {
  uint64_t time;

  if (rou_init)
    {
      rou_init = 0;
      __rouinit();
    }

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  if (!p->isseen)
    {
      char* rname =  p->rout;
      char* modpos;

      /* fix opari output file names */
      if ( (modpos = strstr(p->file, ".mod.")) != NULL )
        {
          strcpy(modpos, modpos+4);
        }
      
#if (defined (VT_OMPI)  || defined (VT_OMP))

      if (omp_in_parallel())
	{
#pragma omp critical (vt_comp_pgi_1)
	  {
	    if (!p->isseen)
	      {	
		p->fid    = vt_def_file(p->file);
		p->rid    = vt_def_region(rname, p->fid, p->lineno, VT_NO_LNO, VT_DEF_GROUP, VT_FUNCTION);
		p->isseen = 1;
	      }
	  }
	}
      else
	{
	  p->fid    = vt_def_file(p->file);
	  p->rid    = vt_def_region(rname, p->fid, p->lineno, VT_NO_LNO, VT_DEF_GROUP, VT_FUNCTION);
	  p->isseen = 1;
	}
#else
 
      p->fid    = vt_def_file(p->file);
      p->rid    = vt_def_region(rname, p->fid, p->lineno, VT_NO_LNO, VT_DEF_GROUP, VT_FUNCTION);
      p->isseen = 1;

#endif
    }
      
  /* write enter trace record */
  vt_enter(&time, p->rid);  

  VT_MEMHOOKS_ON();
}

/*
 *-----------------------------------------------------------------------------
 * called at the end of each profiled routine
 *-----------------------------------------------------------------------------
 */

void ___rouret2(void) {
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();
  vt_exit(&time);

  VT_MEMHOOKS_ON();
}

void ___linent2(void *l) {
}
