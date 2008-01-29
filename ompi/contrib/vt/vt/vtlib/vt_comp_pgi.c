/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
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
#include <omp.h>
#define VT_MY_THREAD   omp_get_thread_num() 
#define VT_NUM_THREADS omp_get_max_threads() 
#else
#define VT_MY_THREAD   0
#define VT_NUM_THREADS 1 
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

static long* cstsizev;
static int   rou_init = 1;  /* initialization necessary ? */

/*
 *-----------------------------------------------------------------------------
 * called during program termination
 *-----------------------------------------------------------------------------
 */

void __rouexit() {
  uint64_t time;

  VT_MEMHOOKS_OFF();

  /* write pending exits */
  while(cstsizev[VT_MY_THREAD] > 0)
    {
      cstsizev[VT_MY_THREAD]--; 
      time = vt_pform_wtime();
      vt_exit(&time);
    }

  /* free call-stack-size vector */
  free(cstsizev);

  /* close trace file */
  vt_close();
}

/*
 *-----------------------------------------------------------------------------
 * called during program initialization
 *-----------------------------------------------------------------------------
 */

void __rouinit() {
  int i;

  VT_MEMHOOKS_OFF();

  /* call-stack initialization */
  cstsizev = (long*)calloc(VT_NUM_THREADS, sizeof(long)); 
  for (i = 0; i < VT_NUM_THREADS; i++)
    cstsizev[i] = 0;
 
  /* open trace file */
  vt_open();

  if (rou_init)
    {
      rou_init = 0;
      vt_comp_finalize = &__rouexit;
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

  /* -- return, if tracing is disabled? -- */
  if ( !VT_IS_TRACE_ON() ) return;

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
      
  /* increment call-stack size */
  cstsizev[VT_MY_THREAD]++; 
  
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

  /* -- return, if tracing is disabled? -- */
  if ( !VT_IS_TRACE_ON() ) return;

  VT_MEMHOOKS_OFF();

  /* decrement call-stack size */
  cstsizev[VT_MY_THREAD]--; 
  
  time = vt_pform_wtime();
  vt_exit(&time);

  VT_MEMHOOKS_ON();
}

void ___linent2(void *l) {
}
