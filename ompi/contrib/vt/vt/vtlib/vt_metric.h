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

#ifndef _VT_METRIC_H
#define _VT_METRIC_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"

/* maximum number of events */
#define VT_METRIC_MAXNUM 20

struct vt_metv;

typedef enum {
    VT_METMAP_UNKNOWN=0x0,
    VT_METMAP_MEASURE=0x1,
    VT_METMAP_AGGROUP=0x2,
    VT_METMAP_COMPOSE=0x4,
    VT_METMAP_COMPUTE=0x8,
    VT_METMAP_INVALID=0x10
} vt_metmap_t;

typedef struct vt_metricmap_t {
  vt_metmap_t             type;
  char*              event_name;
  char*              alias_name;
  struct vt_metricmap_t*  next;
} vt_metricmap_t;

/* read metric mapping definitions into vt_metricmap vector */
EXTERN vt_metricmap_t* vt_metricmap_init(const vt_metmap_t match);
EXTERN void             vt_metricmap_dump(vt_metricmap_t* map);
EXTERN void             vt_metricmap_free(vt_metricmap_t* map);

EXTERN int              vt_metric_open(void);                       /* returns number of counters */ 
EXTERN void             vt_metric_close(void);
EXTERN void             vt_metric_thread_init(int (*id_fn)(void));  /* supply omp_get_thread_num() as argument */
EXTERN void             vt_metric_thread_fini(void);                /* unregister thread */

EXTERN int              vt_metric_num(void);                        /* returns number of counters */ 
EXTERN uint64_t         vt_metric_clckrt(void);                     /* returns clock rate in Hz */ 
EXTERN uint64_t         vt_metric_real_cyc(void);                   /* returns total number of cycles */
EXTERN uint64_t         vt_metric_real_usec(void);                  /* returns total number of usecs */

/* for i provide a value between 0 and vt_metric_num() - 1 */

EXTERN const char*      vt_metric_name(int i);
EXTERN const char*      vt_metric_descr(int i);
EXTERN int              vt_metric_dtype(int i);
EXTERN int              vt_metric_mode(int i);
EXTERN int              vt_metric_iv(int i);

/* create and free per-thread counter sets */

EXTERN struct vt_metv* vt_metric_create(void);
EXTERN void             vt_metric_free(struct vt_metv* metv);

/* reads values of counters relative to the time of vt_metric_open() */
EXTERN void             vt_metric_read(struct vt_metv* metv, uint64_t values[]);

#endif
