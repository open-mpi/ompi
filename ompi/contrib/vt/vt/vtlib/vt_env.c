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

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "vt_env.h"
#include "vt_error.h"
#include "vt_defs.h"
#include "vt_pform.h"

#define VT_MAX_THREADS 65536

static char* replace_vars(char *v) {
  char* start;
  char* end;
  char* vname;
  char* vval;
  char* res;
  int extra = 0;
  int plen = 0;

  if ((start = strchr(v, '$')) == NULL ) {
    /* no $ in v -> no replacement necessary */
    return v;
  } else {
    if ( start[1] == '{' ) {
      /* ${....} form */
      extra = 1;
      end = start += 2;
      while ( *end && *end != '}' ) end++;
    } else {
      /* $### form where # is letter, digit, or underscore */
      end = ++start;
      while ( *end && (isalnum(*end) || *end == '_')) end++;
    }
    /* determine name of variable */
    vname = (char*)malloc((end-start)+1);
    strncpy(vname, start, end-start);
    vname[end-start] = '\0';

    /* get its content */
    if ((vval = getenv(vname)) == NULL) vval = "";

    /* put together string with variable replaced by value */
    /* -- allocate enough space and copy stuff before variable part */
    res = (char*)malloc(strlen(v)+strlen(vval)+1);
    plen = (start - v) - 1 - extra;
    if (plen) strncpy(res, v, plen);
    res[plen] = '\0';
    /* -- add variable content */
    strcat(res, vval);
    /* -- add stuff after variable */
    if ( *end ) strcat(res, end + extra);

    free(vname);
    return res;
  }
}

static int parse_bool(char *str) {
  static char strbuf[128];
  char* ptr = strbuf;

  strncpy(strbuf, str, sizeof(strbuf)-1);
  while ( *ptr )
    {
      *ptr = tolower(*ptr);
      ++ptr;
    }

  if ( strcmp(strbuf, "yes") == 0  ||
       strcmp(strbuf, "true") == 0 ||
       strcmp(strbuf, "1") == 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

static size_t parse_size(char *str) {
  size_t size = 0;

  if (strlen(str) > 1)
  {
     int multiply = 1;

     if (str[strlen(str)-1] == 'M' 
	 || str[strlen(str)-1] == 'm')
     {
	multiply = 1e6;
     }
     else if (str[strlen(str)-1] == 'G' 
	      || str[strlen(str)-1] == 'g')
     {
	multiply = 1e9;
     }

     size = atoll(str) * multiply;
  }

  return size;
}

char* vt_env_apppath()
{
  static int read = 1;
  static char* apppath = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_APPPATH");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  apppath = replace_vars(tmp);
	}
    }
  return apppath;
}

char* vt_env_dyn_blacklist()
{
  static int read = 1;
  static char* dyn_blacklist = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_DYN_BLACKLIST");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  dyn_blacklist = replace_vars(tmp);
	}
    }
  return dyn_blacklist;
}

char* vt_env_dyn_shlibs()
{
  static int read = 1;
  static char* dyn_shlibs = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_DYN_SHLIBS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  dyn_shlibs = replace_vars(tmp);
	}
    }
  return dyn_shlibs;
}

char* vt_env_gdir()
{
  static char* gdir = NULL;
  char* tmp;

  if (! gdir)
    {
      tmp = getenv("VT_PFORM_GDIR");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          gdir = replace_vars(tmp);
        }
      else
        {
          gdir = replace_vars(vt_pform_gdir());
        } 
    }
  return gdir;
}

char* vt_env_ldir()
{
  static char* ldir = NULL;
  char* tmp;

  if (! ldir)
    {
      tmp = getenv("VT_PFORM_LDIR");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          ldir = replace_vars(tmp);
        }
      else
        {
          ldir = replace_vars(vt_pform_ldir());
        } 
    }
  return ldir;
}

char* vt_env_fprefix()
{
  static char* fprefix = NULL;
  char* tmp;

  if (! fprefix)
    {
      tmp = getenv("VT_FILE_PREFIX");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          fprefix = replace_vars(tmp);
        }
      else
        {
          fprefix = "a";
        } 
    }
  return fprefix;
}

size_t vt_env_bsize()
{
   static size_t buffer_size = 0;
   char* tmp;

   if (buffer_size == 0)
     {
       tmp = getenv("VT_BUFFER_SIZE");
       if (tmp != NULL && strlen(tmp) > 0)
         {
	   buffer_size = parse_size(tmp);
	   if (buffer_size <= 0)
	     vt_error_msg("VT_BUFFER_SIZE not properly set");
	   else if (buffer_size < VT_MIN_BUFSIZE) {
	     vt_warning("VT_BUFFER_SIZE=%d resized to %d bytes", 
			buffer_size, VT_MIN_BUFSIZE);
	     buffer_size = VT_MIN_BUFSIZE;
	   }
	 }
       else
         {
	   buffer_size = VT_DEF_BUFSIZE;
	 }
     }
  return buffer_size;
}

int vt_env_mode()
{
  static int modeflags = 0;
  char* tmp;

  if (modeflags == 0)
    {
      tmp = getenv("VT_MODE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  char* tk = strtok(tmp, ":");
	  int dc = 0;
	  modeflags = 0;
	  do {
	    if (dc <= 1 &&
	       (strcmp( tk, "TRACE" ) == 0 || strcmp( tk, "trace" ) == 0))
	      modeflags |= 1;
	    else if(dc <= 1 &&
		    (strcmp( tk, "STAT" ) == 0 || strcmp( tk, "stat" ) == 0))
	      modeflags |= 2;
	    else
	      vt_error_msg("VT_MODE not properly set");
	    dc++;
	  } while((tk = strtok(0, ":")));
	}
      else
        {
	   modeflags = 1;
	}
    }
  return modeflags;
}

int vt_env_stat_intv()
{
  static int stat_intv = -1;
  char* tmp;

  if (stat_intv == -1)
    {
      tmp = getenv("VT_STAT_INTV");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  stat_intv = atoi(tmp);
	  if (stat_intv < 0)
	    vt_error_msg("VT_STAT_INTV not properly set");
	}
      else
        {
	   stat_intv = 0;
	}
    }
  return stat_intv;
}

int vt_env_stat_show()
{
  static int stat_show = -1;
  char* tmp;

  if (stat_show == -1)
    {
      tmp = getenv("VT_STAT_SHOW");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  stat_show = parse_bool(tmp);
	}
      else
        {
	  stat_show = 0;
	}
    }
  return stat_show;
}

int vt_env_is_verbose()
{
  static int verbose = -1;
  char* tmp;

  if (verbose == -1)
    {
      tmp = getenv("VT_VERBOSE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  int val = atoi(tmp);
	  if (val > 0)
	    verbose = val;
	  else
	    verbose = parse_bool(tmp);
	}
      else
        {
	  verbose = 0;
	}
    }
  return verbose;
}

int vt_env_debug()
{
  static int debug = -1;
  char* tmp;

  if (debug == -1)
    {
      tmp = getenv("VT_DEBUG");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          debug = atoi(tmp);
          if (debug < 0) debug = 0;
        }
      else
        {
          debug = 0;
        }
    }
  return debug;
}

int vt_env_do_demangle()
{
  static int do_demangle = -1;
  char* tmp;

  if (do_demangle == -1)
    {
      tmp = getenv("VT_DEMANGLE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  do_demangle = parse_bool(tmp);
	}
      else
        {
	  do_demangle = 0;
	}
    }
  return do_demangle;
}

int vt_env_do_unify()
{
  static int do_unify = -1;
  char* tmp;

  if (do_unify == -1)
    {
      tmp = getenv("VT_UNIFY");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  do_unify = parse_bool(tmp);
	}
      else
        {
	  do_unify = 1;
	}
    }
  return do_unify;
}

int vt_env_do_clean()
{
  static int do_clean = -1;
  char* tmp;

  if (do_clean == -1)
    {
      tmp = getenv("VT_CLEAN");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  do_clean = parse_bool(tmp);
	}
      else
        {
	  do_clean = 1;
	}
    }
  return do_clean;
}

int vt_env_memtrace()
{
  static int memtrace = -1;
  char* tmp;

  if (memtrace == -1)
    {
      tmp = getenv("VT_MEMTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  memtrace = parse_bool(tmp);
	}
      else
        {
	  memtrace = 0;
	}
    }
  return memtrace;
}

int vt_env_iotrace()
{
  static int iotrace = -1;
  char* tmp;

  if (iotrace == -1)
    {
      tmp = getenv("VT_IOTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  iotrace = parse_bool(tmp);
	}
      else
        {
	  iotrace = 0;
	}
    }
  return iotrace;
}

int vt_env_mpitrace()
{
  static int mpitrace = -1;
  char* tmp;

  if (mpitrace == -1)
    {
      tmp = getenv("VT_MPITRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  mpitrace = parse_bool(tmp);
	}
      else
        {
	  mpitrace = 1;
	}
    }
  return mpitrace;
}

char* vt_env_metrics()
{
  static int read = 1;
  static char* metrics = NULL;

  if (read)
    {
      read = 0;
      metrics = getenv("VT_METRICS");
      if ( metrics != NULL && strlen(metrics) == 0 )
	metrics = NULL;
    }
  return metrics;
}

/* The file with the metrics specifications can be defined with the
VT_METRICS_SPEC environment variable, otherwise it is looked for in
the current directory and the VampirTrace installation DATADIR. */

#define METRICS_SPEC "METRICS.SPEC"

char* vt_env_metrics_spec()
{
  char  msg[128];
  char* spec = getenv("VT_METRICS_SPEC");
  int len;

  if ( spec != NULL && strlen(spec) > 0 ) { /* use specified file */
    snprintf(msg, sizeof(msg)-1, "VT_METRICS_SPEC=%s", spec);
  } else if (access(METRICS_SPEC, R_OK) == 0) {
    /* use file in current directory */
    len = strlen(METRICS_SPEC)+3;
    spec = (char*)calloc(len, sizeof(char));
    snprintf(spec, len-1, "./%s", METRICS_SPEC);
    snprintf(msg, sizeof(msg)-1, "[CURDIR] VT_METRICS_SPEC=%s", spec);
  } else {
#ifdef DATADIR
    /* default to installation file */
    len = strlen(DATADIR)+strlen(METRICS_SPEC)+2;
    spec = (char*)calloc(len, sizeof(char));
    snprintf(spec, len-1, "%s/%s", DATADIR, METRICS_SPEC);
    snprintf(msg, sizeof(msg)-1, "[DATADIR] VT_METRICS_SPEC=%s", spec);
#else
    snprintf(msg, sizeof(msg)-1, "VT_METRICS_SPEC not set");
#endif
  }
  vt_cntl_msg(msg);
  return spec;
}

int vt_env_max_flushes()
{
  static int max_flushes = -1;
  char* tmp;

  if (max_flushes == -1)
    {
      tmp = getenv("VT_MAX_FLUSHES");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  max_flushes = atoi(tmp);
	  if (max_flushes < 0)
	    vt_error_msg("VT_MAX_FLUSHES not properly set");
	}
      else
        {
	  max_flushes = 1;
	}
    }
  return max_flushes;
}

int vt_env_max_threads()
{
  static int max_threads = -1;
  char* tmp;

  if (max_threads == -1)
    {
      tmp = getenv("VT_MAX_THREADS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  max_threads = atoi(tmp);
	}
      else
        {
	  max_threads = VT_MAX_THREADS;
	}
    }
  return max_threads;
}

char* vt_env_nm()
{
  static int read = 1;
  static char* nm = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_NM");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  nm = replace_vars(tmp);
        }
      else
        {
#ifdef DEFAULT_NM
          nm = DEFAULT_NM;
#endif /* DEFAULT_NM */
        }
    }

  return nm;

}

char* vt_env_nmfile()
{
  static int read = 1;
  static char* nmfile = NULL;
  char* tmp;

  if (read)
  {
    read = 0;
    tmp = getenv("VT_NMFILE");
    if (tmp != NULL && strlen(tmp) > 0)
      {
	nmfile = replace_vars(tmp);
      }
  }
  return nmfile;
}

int vt_env_compression()
{
  static int compression = -1;
  char* tmp;

  if (compression == -1)
    {
      tmp = getenv("VT_COMPRESSION");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  compression = parse_bool(tmp);
	}
      else
        {
	  compression = 1;
	}
    }
  return compression;
}

char*  vt_env_filter_spec()
{
  static int read = 1;
  static char* spec = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_FILTER_SPEC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  spec = replace_vars(tmp);
	}
    }
  return spec;
}

char*  vt_env_groups_spec()
{
  static int read = 1;
  static char* spec = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_GROUPS_SPEC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
	  spec = replace_vars(tmp);
	}
    }
  return spec;
}
