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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>

#include "pomp_lib.h"

#include "vt_defs.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_pomp.h"
#include "vt_omplock.h"
#include "vt_ompreg.h"
#include "vt_trc.h"

/*
 * Global variables
 */

int pomp_initialized = 0;
int pomp_tracing = 0;

void POMP_Finalize() {
  static int pomp_finalize_called = 0;

  if ( ! pomp_finalize_called ) {
    pomp_finalize_called = 1;
    VT_MEMHOOKS_OFF();
    vt_close();
  }
}

void POMP_Init() {
  static int pomp_init_called = 0;
  static struct VTRegDescr rd_data_table[1000];
#if (defined (VT_OMPI) || defined (VT_OMP))
  char anno_rname[256]; /* annotated region name */
#endif
  int i;
  uint8_t rtype = VT_UNKNOWN;
  char* rname = "";
  const char* rdesc;

  if ( ! pomp_init_called ) {
    pomp_init_called = 1;

    VT_MEMHOOKS_OFF();

    vt_open();
    atexit(POMP_Finalize);

    /* register wrapper functions for OpenMP API */
#if (defined (VT_OMPI) || defined (VT_OMP))
    vt_omp_register();
#endif

    for(i = 0; i < POMP_MAX_ID; ++i) {
      if ( pomp_rd_table[i] ) {
        struct VTRegDescr* data = &rd_data_table[i];
        struct ompregdescr* r    = pomp_rd_table[i];      
        r->data = data;
	rdesc   = "OMP";

        /* -- register file --*/
        data->fid   = vt_def_file(r->file_name);
        data->begln = r->begin_first_line;
        data->endln = r->end_last_line;
	data->sbrid = VT_NO_ID;

        if (strcmp(r->name, "region") == 0)  {
          rtype = VT_USER_REGION;
          rname = r->sub_name;
	  rdesc   = VT_DEF_GROUP;
#if (defined (VT_OMPI) || defined (VT_OMP))
        } else if (strcmp(r->name, "atomic") == 0) {
          rtype = VT_OMP_ATOMIC;
          rname = "!$omp atomic";
        } else if (strcmp(r->name, "barrier") == 0) {
          rtype = VT_OMP_BARRIER;
          rname = "!$omp barrier";
        } else if (strcmp(r->name, "critical") == 0) {
          rtype = VT_OMP_CRITICAL;
          rname = "!$omp critical";
          sprintf(anno_rname, "%s @%s:%d", "!$omp critical sblock",
                basename(r->file_name), r->begin_first_line+1);
          data->sbrid = vt_def_region(anno_rname,
			               data->fid, r->begin_last_line+1,
                                       r->end_first_line-1, "OMP",
         			       VT_OMP_CRITICAL_SBLOCK);
	  if ( r->sub_name[0] ) {
            rname = (char*)malloc(24 + strlen(r->sub_name));
	    sprintf(rname, "!$omp critical %s", r->sub_name);
	    data->brid = vt_lock_init(r->sub_name);
	  } else {
	    data->brid = vt_lock_init("default");
          }
        } else if (strcmp(r->name, "do") == 0) {
          rtype = VT_OMP_LOOP;
          rname = "!$omp do";
        } else if (strcmp(r->name, "flush") == 0) {
          rtype = VT_OMP_FLUSH;
          rname = "!$omp flush";
        } else if (strcmp(r->name, "for") == 0) {
          rtype = VT_OMP_LOOP;
          rname = "!$omp for";
        } else if (strcmp(r->name, "function") == 0)  {
          rtype = VT_FUNCTION;
          rname = r->sub_name;
	  rdesc   = VT_DEF_GROUP;
        } else if (strcmp(r->name, "master") == 0) {
          rtype = VT_OMP_MASTER;
          rname = "!$omp master";
        } else if (strcmp(r->name, "parallel") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
        } else if (strcmp(r->name, "paralleldo") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp do",
                basename(r->file_name), r->begin_first_line);
          data->sbrid = vt_def_region(anno_rname,  data->fid, data->begln,
                                       data->endln, "OMP", VT_OMP_LOOP);
        } else if (strcmp(r->name, "parallelfor") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp for",
                basename(r->file_name), r->begin_first_line);
          data->sbrid = vt_def_region(anno_rname,  data->fid, data->begln,
                                       data->endln, "OMP", VT_OMP_LOOP);
        } else if (strcmp(r->name, "parallelsections") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp sections",
                basename(r->file_name), r->begin_first_line);
          data->sbrid = vt_def_region(anno_rname, data->fid, data->begln,
                                       data->endln, "OMP", VT_OMP_SECTIONS);
        } else if (strcmp(r->name, "parallelworkshare") == 0)  {
          rtype = VT_OMP_PARALLEL;
          rname = "!$omp parallel";
          sprintf(anno_rname, "%s @%s:%d", "!$omp workshare",
                basename(r->file_name), r->begin_first_line);
          data->sbrid = vt_def_region(anno_rname, data->fid, data->begln,
                                       data->endln, "OMP", VT_OMP_WORKSHARE);
        } else if (strcmp(r->name, "sections") == 0) {
          rtype = VT_OMP_SECTIONS;
          rname = "!$omp sections";
          sprintf(anno_rname, "%s @%s:%d", "!$omp section",
                basename(r->file_name), r->begin_last_line);
          data->sbrid = vt_def_region(anno_rname,
			               data->fid, r->begin_last_line,
                                       r->end_first_line, "OMP",
         			       VT_OMP_SECTION);
        } else if (strcmp(r->name, "section") == 0) {
          /* NOT DEFINED BY POMP YET */
          /* rtype = VT_OMP_SECTION; */
          /* rname = "!$omp section"; */
        } else if (strcmp(r->name, "single") == 0) {
          rtype = VT_OMP_SINGLE;
          rname = "!$omp single";
          sprintf(anno_rname, "%s @%s:%d", "!$omp single sblock",
                basename(r->file_name), r->begin_last_line+1);
          data->sbrid = vt_def_region(anno_rname,
			               data->fid, r->begin_last_line+1,
                                       r->end_first_line-1, "OMP",
         			       VT_OMP_SINGLE_SBLOCK);
        } else if (strcmp(r->name, "workshare") == 0) {
          rtype = VT_OMP_WORKSHARE;
          rname = "!$omp workshare";
#endif
        } else {
          rtype = VT_UNKNOWN;
          rname = r->name;
        }

#if (defined (VT_OMPI) || defined (VT_OMP))
        if (strcmp(rdesc, "OMP") == 0) {
            sprintf(anno_rname, "%s @%s:%d", rname,
                basename(r->file_name), r->begin_first_line);
            rname = anno_rname;
        }
#endif

        /* -- register region -- */
        data->rid = vt_def_region(rname,  data->fid, data->begln,
                                   data->endln, rdesc, rtype);

#if (defined (VT_OMPI) || defined (VT_OMP))
        if (rtype == VT_OMP_PARALLEL ||
            rtype == VT_OMP_LOOP     ||
            rtype == VT_OMP_SECTIONS ||
            rtype == VT_OMP_SINGLE   ||
            rtype == VT_OMP_WORKSHARE) {
          /* -- register implicit barrier -- */
          rname = "!$omp ibarrier";
          sprintf(anno_rname, "%s @%s:%d", rname,
                basename(r->file_name), r->end_last_line);
          data->brid = vt_def_region(anno_rname,
                                      data->fid, data->endln, data->endln,
                                      "OMP", VT_OMP_IBARRIER);
        } else
#endif
          data->brid = VT_NO_ID;
      }
    }
    pomp_tracing = 1;

    VT_MEMHOOKS_ON();
  }
}

void POMP_Off() {
  pomp_tracing = 0;
}

void POMP_On() {
  pomp_tracing = 1;
}

static uint32_t main_rid = VT_NO_ID;

void POMP_Begin(struct ompregdescr* r) {
  struct VTRegDescr* data = (struct VTRegDescr*)(r->data);

  if ( main_rid == VT_NO_ID ) main_rid = data->rid;
  if ( IS_POMP_TRACE_ON )
  {
    uint64_t time;
    VT_MEMHOOKS_OFF();
    time = vt_pform_wtime();
    vt_enter(&time, data->rid);
    VT_MEMHOOKS_ON();
  }
}

void POMP_End(struct ompregdescr* r) {
  struct VTRegDescr* data = (struct VTRegDescr*)(r->data);

  if ( IS_POMP_TRACE_ON )
  {
    uint64_t time;
    VT_MEMHOOKS_OFF();
    time = vt_pform_wtime();
    vt_exit(&time);
    VT_MEMHOOKS_ON();
  }
  if ( data->rid == main_rid ) POMP_Finalize();
}
