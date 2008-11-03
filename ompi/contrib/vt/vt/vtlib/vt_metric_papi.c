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

#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_metric.h"

#include <papi.h>

#ifndef TIMER_PAPI_REAL_CYC
#  define TIMER_PAPI_REAL_CYC 10
#endif
#ifndef TIMER_PAPI_REAL_USEC
#  define TIMER_PAPI_REAL_USEC 11
#endif

struct metric
{
  char* name;
  char  descr[PAPI_HUGE_STR_LEN];
  int   papi_code;
};

struct vt_metv
{
  int EventSet;
};

/* global variables */
static struct metric* metricv[VT_METRIC_MAXNUM];
static int    nmetrics = 0;

static void metricv_add(char* name, int code)
{
    if (nmetrics >= VT_METRIC_MAXNUM)
        vt_error_msg("Number of counters exceeds VampirTrace allowed maximum of %d\n",
                      VT_METRIC_MAXNUM);
    else {
        metricv[nmetrics] = (struct metric*)malloc(sizeof(struct metric));
        metricv[nmetrics]->name = strdup(name);
        metricv[nmetrics]->descr[0] = '\0';
        metricv[nmetrics]->papi_code = code;
        nmetrics++;
    }
}

/* PAPI-specific error message */

static void vt_metric_error(int errcode, char *note)
{
  char   errstring[PAPI_MAX_STR_LEN];

  PAPI_perror(errcode, errstring, PAPI_MAX_STR_LEN);
  if (errcode == PAPI_ESYS) {
      strncat(errstring, ": ", PAPI_MAX_STR_LEN-strlen(errstring));
      strncat(errstring, strerror(errno), PAPI_MAX_STR_LEN-strlen(errstring));
  }
  vt_error_msg("%s: %s (fatal)", note?note:"PAPI", errstring);
}

/* PAPI-specific warning message */

static void vt_metric_warning(int errcode, char *note)
{
  char   errstring[PAPI_MAX_STR_LEN];

  PAPI_perror(errcode, errstring, PAPI_MAX_STR_LEN);
  if (errcode == PAPI_ESYS) {
      strncat(errstring, ": ", PAPI_MAX_STR_LEN-strlen(errstring));
      strncat(errstring, strerror(errno), PAPI_MAX_STR_LEN-strlen(errstring));
  }
  vt_warning("%s: %s (ignored)", note?note:"PAPI", errstring);
}

/* get metric descriptions */

static void vt_metric_descriptions(void)
{
    int i, j, k, retval;
    PAPI_event_info_t info;

    for (i=0; i < nmetrics; i++) {
        memset(&info, 0, sizeof(PAPI_event_info_t));
        retval = PAPI_get_event_info(metricv[i]->papi_code, &info);
        if (retval != PAPI_OK)
            vt_metric_error(retval, "PAPI_get_event_info");

        if (strcmp(info.long_descr, metricv[i]->name) != 0) {
            strncpy(metricv[i]->descr, info.long_descr, sizeof(metricv[i]->descr));
              
            /* tidy description if necessary */
            j=strlen(metricv[i]->descr)-1;
            if (metricv[i]->descr[j] == '\n') metricv[i]->descr[j]='\0';
            j=strlen(metricv[i]->descr)-1;
            if (metricv[i]->descr[j] != '.')
               strncat(metricv[i]->descr, ".", sizeof(metricv[i]->descr));
        }

        if (metricv[i]->papi_code & PAPI_PRESET_MASK) { /* PAPI preset */
            char *postfix_chp = info.postfix;
            char derive_ch = strcmp(info.derived,"DERIVED_SUB")?'+':'-';
            strncat(metricv[i]->descr, " [ ", sizeof(metricv[i]->descr));
            strncat(metricv[i]->descr, info.name[0], sizeof(metricv[i]->descr));
            for (k=1; k<(int)info.count; k++) {
                char op[4];
                postfix_chp = postfix_chp?strpbrk(++postfix_chp, "+-*/"):NULL;
                sprintf(op, " %c ", (postfix_chp?*postfix_chp:derive_ch));
                strncat(metricv[i]->descr, op, sizeof(metricv[i]->descr));
                strncat(metricv[i]->descr, info.name[k], sizeof(metricv[i]->descr));
            }
            strncat(metricv[i]->descr, " ]", sizeof(metricv[i]->descr));
            if (strcmp(info.symbol, metricv[i]->name) != 0) { /* add preset name */
                strncat(metricv[i]->descr, " = ", sizeof(metricv[i]->descr)); 
                strncat(metricv[i]->descr, info.symbol, sizeof(metricv[i]->descr));
            }
        }

        /*printf("Metric %d: <%s>\n<<%s>>\n", i, metricv[i]->name, metricv[i]->descr);*/
    }
}

/* test whether requested event combination valid */

static void vt_metric_test(void)
{
  int i;
  int retval;
  int EventSet = PAPI_NULL;
  
  /* create event set */
  retval = PAPI_create_eventset(&EventSet);
  if ( retval != PAPI_OK)
    vt_metric_error(retval, "PAPI_create_eventset");

  for ( i = 0; i < nmetrics; i++ )
    {
      /* add event to event set */
      retval = PAPI_add_event(EventSet, metricv[i]->papi_code);
      if ( retval != PAPI_OK ) {
        char errstring[PAPI_MAX_STR_LEN];
        sprintf(errstring, "PAPI_add_event(%d:\"%s\")", i, metricv[i]->name);
        vt_metric_error(retval, errstring);
      }
      vt_cntl_msg("Event %s added to event set", metricv[i]->name);
    }
  retval = PAPI_cleanup_eventset(EventSet);
  if ( retval != PAPI_OK )
    vt_metric_error(retval, "PAPI_cleanup_eventset");

  retval = PAPI_destroy_eventset(&EventSet);
  if ( retval != PAPI_OK )
    vt_metric_error(retval, "PAPI_destroy_eventset");

  vt_cntl_msg("Event set tested OK");
}

/* define and set PAPI metrics based on VT_METRICS specification */

int vt_metric_open()
{
    int retval;
    char* env;
    char* var;
    char* token;
    PAPI_event_info_t info;
    vt_metricmap_t* mapv = NULL;

    /* read environment variable "VT_METRICS". Return if 
       uset and no PAPI timer used. */
    env = vt_env_metrics();
    if( env == NULL )
    {
#if TIMER != TIMER_PAPI_REAL_CYC && TIMER != TIMER_PAPI_REAL_USEC
      return nmetrics;
#endif
    }

    mapv = vt_metricmap_init(
       (vt_metmap_t)(VT_METMAP_MEASURE|VT_METMAP_AGGROUP));
    /*vt_metricmap_dump(mapv);*/

    /* initialize PAPI */
    retval = PAPI_library_init(PAPI_VER_CURRENT);
    if ( retval != PAPI_VER_CURRENT )
        vt_metric_error(retval, "PAPI_library_init");

    /* return if environment variable is unset */
    if ( env == NULL )
        return nmetrics;

    var = (char*)calloc(strlen(env) + 1, sizeof(char));
    strcpy(var, env);
    vt_cntl_msg("VT_METRICS=%s", var);
        
    /* read metrics from specification string */
    token = strtok(var, ":");
    while ( token && (nmetrics < VT_METRIC_MAXNUM) ) {
        /* search metricmap for a suitable definition */
        vt_metricmap_t* map = mapv;
        /*printf("Token%d: <%s>\n", nmetrics, token);*/
        while (map != NULL) {
            if ( strcmp(map->event_name, token) == 0 ) {
                /*printf("Definition %s = <%s>\n", token, map->alias_name);*/
                /* expand definition and set components */
                char* c_token = map->alias_name;
                int len = strcspn(c_token, " \t"); /* first token */
                int got_valid_match = 1; /* to be verified */
                int k = 0;
                do { /* verify each component of definition is available */
                    char component[64];
                    int code = -1;
                    strncpy(component, c_token, len);
                    component[len] = '\0';
                    /*printf("Comp[%d] <%s>\n", k, component);*/
                    c_token += len + strspn(c_token+len, " \t");
                    len = strcspn(c_token, " \t"); /* next token */

                    PAPI_event_name_to_code(component, &code);
                    memset(&info, 0, sizeof(PAPI_event_info_t));
                    retval = PAPI_get_event_info(code, &info);
                    /*printf("v[%d] %s [0x%X] %d\n", k, component, code, info.count);*/
       
                    if (info.count == 0) {
                        /*printf("Event %s *N/A*\n", component);*/
                        got_valid_match = 0;
                    } else if ((k==0) && (len==0)) { /* use provided event name */
                        metricv_add(token, code);
                    } else { /* use alias component name */
                        metricv_add(component, code);
                    }
                    k++;
                } while (got_valid_match && (len > 0));
                if (got_valid_match) {
                    /*printf("Definition %s = <%s> OK\n", map->event_name, map->alias_name);*/
                    break; /* accept this event definition */
                }
            }
            map = map->next;
        }

        if (map == NULL) { /* no map match, so try given name */
            int code = -1;
            char* component = token;
            /*printf("Comp[X] <%s>\n", component);*/
            retval = PAPI_event_name_to_code(component, &code);
            if (retval != PAPI_OK || code == -1)
                vt_error_msg("Metric <%s> not supported\n", component);

            memset(&info, 0, sizeof(PAPI_event_info_t));
            retval = PAPI_get_event_info(code, &info);
            /*printf("v[%d] %s [0x%X] %d\n", nmetrics, component, code, info.count);*/
            if (retval != PAPI_OK)
                vt_error_msg("Metric <%s> not available\n", component);

            metricv_add(component, code);
        }

        token = strtok(NULL, ":");
    }

    /*printf("nmetrics=%d\n", nmetrics);*/

    /* clean up */
    vt_metricmap_free(mapv);
    free(var);

    /* Check whether event combination is valid. This is done here to
       avoid errors when creating the event set for each thread, which
       would multiply the error messages. */
    vt_metric_test();

    vt_metric_descriptions();

    return nmetrics;
}

void vt_metric_close()
{
  int i;
  
  for ( i = 0; i < nmetrics; i++ )
    {
      free (metricv[i]->name);
      free(metricv[i]);
    }
}

void vt_metric_thread_init(int (*id_fn)(void))
{
  int retval;

  if ( nmetrics == 0 )
    return;

  retval = PAPI_thread_init((unsigned long (*)(void))(id_fn)); 
  if ( retval != PAPI_OK)
    vt_metric_error(retval, "PAPI_thread_init");
  vt_cntl_msg("PAPI thread support initialized");
}

void vt_metric_thread_fini()
{
  int retval;

  if ( nmetrics == 0 )
    return;

  retval = PAPI_unregister_thread();
  if ( retval != PAPI_OK)
    vt_metric_error(retval, "PAPI_unregister_thread");
  /* vt_cntl_msg("PAPI thread finalized"); */
}

int vt_metric_num()
{
  return nmetrics;
}

uint64_t vt_metric_clckrt()
{
  const PAPI_hw_info_t* hwinfo = NULL;
  double hertz;

  if (!PAPI_is_initialized()) {
      /* initialize PAPI, since it hasn't already been initialized */
      int retval = PAPI_library_init(PAPI_VER_CURRENT);  
      if ( retval != PAPI_VER_CURRENT )
        vt_metric_error(retval, "PAPI_library_init");
  }

  hwinfo = PAPI_get_hardware_info(); 
  if ( hwinfo == NULL)
    vt_error_msg("Failed to access PAPI hardware info");
  vt_cntl_msg("Clock rate: %f MHz", hwinfo->mhz);

  hertz = hwinfo->mhz * 1000000.0;

  return (uint64_t)hertz;
}

uint64_t vt_metric_real_cyc()
{
  return (uint64_t)PAPI_get_real_cyc();
}

uint64_t vt_metric_real_usec()
{
  return (uint64_t)PAPI_get_real_usec();
}

const char* vt_metric_name(int i)
{
  return metricv[i]->name;
}

const char* vt_metric_descr(int i)
{
  return metricv[i]->descr;
}

int vt_metric_dtype(int i)
{
  return VT_INTEGER;
}

int vt_metric_mode(int i)
{
  return VT_COUNTER;
}

int vt_metric_iv(int i)
{
  return VT_START;
}


struct vt_metv* vt_metric_create()
{
  struct vt_metv* metv;
  int retval, i;

  if ( nmetrics == 0 )
    return NULL;

  metv = (struct vt_metv*)malloc(sizeof(struct vt_metv));
  if ( metv == NULL )
    vt_error();
  
  /* create event set */
  metv->EventSet = PAPI_NULL;
  retval = PAPI_create_eventset(&metv->EventSet);
  if ( retval != PAPI_OK)
    vt_metric_error(retval, "PAPI_create_eventset");
  
  for ( i = 0; i < nmetrics; i++ )
    {
      /* add event to event set */
      retval = PAPI_add_event(metv->EventSet, metricv[i]->papi_code);
      if ( retval != PAPI_OK )
	vt_metric_error(retval, "PAPI_add_event");
    }

  retval = PAPI_start(metv->EventSet);
  if ( retval != PAPI_OK )
    vt_metric_error(retval, "PAPI_start");
  /*vt_cntl_msg("Counters started");*/

  return metv;
}

void vt_metric_free(struct vt_metv* metv)
{
  int retval;
  long_long papi_vals[VT_METRIC_MAXNUM];

  if ( metv == NULL )
    return;

  /* treat PAPI failures at this point as non-fatal */

  retval = PAPI_stop(metv->EventSet, papi_vals);
  if ( retval != PAPI_OK ) {
    vt_metric_warning(retval, "PAPI_stop");
  } else { /* cleanup/destroy require successful PAPI_stop */

    retval = PAPI_cleanup_eventset(metv->EventSet);
    if ( retval != PAPI_OK )
      vt_metric_warning(retval, "PAPI_cleanup_eventset");

    retval = PAPI_destroy_eventset(&metv->EventSet);
    if ( retval != PAPI_OK )
      vt_metric_warning(retval, "PAPI_destroy_eventset");
    
    free(metv);

    /*vt_cntl_msg("Counters stopped");*/
  }
}

void vt_metric_read(struct vt_metv* metv, uint64_t values[])
{
  int retval;

  if ( metv == NULL )
    return;

  if (sizeof(long_long) == 8)
    {
      retval = PAPI_read(metv->EventSet, (long_long*) values);
    }
  else
    {
      int i;
      long_long papi_vals[VT_METRIC_MAXNUM];
      retval = PAPI_read(metv->EventSet, papi_vals);
      for (i = 0; i < nmetrics; i++)
	values[i] = (uint64_t) papi_vals[i];
    }
     
  if ( retval != PAPI_OK )
    vt_metric_error(retval, "PAPI_read");
}

