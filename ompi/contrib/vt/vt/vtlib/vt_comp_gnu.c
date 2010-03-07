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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include "vt_comp.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"
#if (defined (VT_OMPI) || defined (VT_OMP))
#  include <omp.h>
#endif

#define NM_LINE_BLK_LEN 1024
#define NM_LINE_MAX_LEN 16384

static int gnu_init = 1;       /* is initialization needed? */

/*
 *-----------------------------------------------------------------------------
 * Simple hash table to map function addresses to region names/identifier
 *-----------------------------------------------------------------------------
 */

typedef struct HN {
  long id;            /* hash code (address of function */
  const char* name;   /* associated function name       */
  const char* fname;  /*            file name           */
  int lno;            /*            line number         */
  uint32_t vtid;      /* associated region identifier   */
  struct HN* next;
} HashNode;

#define HASH_MAX 1021

static HashNode* htab[HASH_MAX];

/*
 * Stores function name `n' under hash code `h'
 */

static void hash_put(long h, const char* n, const char* fn, int lno) {
  long id = h % HASH_MAX;
  HashNode *add = (HashNode*)malloc(sizeof(HashNode));
  add->id = h;
  add->name  = n;
  add->fname = fn ? (const char*)strdup(fn) : fn;
  add->lno   = lno;
  add->vtid = VT_NO_ID;
  add->next = htab[id];
  htab[id] = add;
}

/*
 * Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL
 */

static HashNode* hash_get(long h) {
  long id = h % HASH_MAX;
  HashNode *curr = htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/*
 * Get symbol table by 'nm'
 */

static void get_symtab(void)
{
  char* nm_cmd = NULL;
  char* nm_filename;
  FILE* nm_stream;

  char* apppath = NULL;

  char* line;
  size_t line_size;
  uint32_t lineno = 0;

  uint8_t parse_error = 0;

  VT_SUSPEND_IO_TRACING();

  /* open nm-file, if given */
  nm_filename = vt_env_nmfile();
  if ( nm_filename != NULL && strlen(nm_filename) > 0 )
  {
    vt_cntl_msg("Collecting symbols from file %s", nm_filename);

    /* open nm-file */
    if ( (nm_stream = fopen(nm_filename, "r")) == NULL )
      vt_error_msg("Could not open symbol list file %s", nm_filename);
  }
  /* otherwise, try to get symbol table automatically */
  else
  {
    char* nm;
    size_t nm_cmd_len;

    char* apppath_env;

    vt_cntl_msg("Collecting symbols by 'nm'");

    /* get executable path from VT_APPPATH, if set */
    apppath_env = vt_env_apppath();
    if ( apppath_env != NULL && strlen(apppath_env) > 0 )
    {
      apppath = strdup(apppath_env);
      if ( apppath == NULL )
        vt_error();
    }
    /* otherwiese, try to get executable path from /proc file system */
    else
    {
      int pid = getpid();
      apppath = (char*)malloc(256 * sizeof(char*));
      if ( apppath == NULL )
        vt_error();

      snprintf(apppath, 255, "/proc/%d/exe", pid);
      if ( access(apppath, R_OK) != 0 )
      {
        snprintf(apppath, 255, "/proc/%d/object/exe", pid);
        if ( access(apppath, R_OK) != 0 )
        {
          vt_error_msg("Could not determine path of executable.\n"
                       "Please set the environment variable VT_APPPATH to the "
                       "path of your executable or set VT_NMFILE to a symbol "
                       "list file created with 'nm'.");
        }
      }
    }

    /* get nm command specified by VT_NM */
    nm = vt_env_nm();
    if ( nm == NULL )
    {
      vt_error_msg("VampirTrace was configured without an 'nm' command.\n"
                   "Please set the environment variable VT_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_NMFILE to a pre-created symbol list file." );
    }

    /* allocate memory for nm command */
    nm_cmd_len = strlen(nm) + 1 + strlen(apppath) + 1;
    nm_cmd = (char*)malloc(nm_cmd_len * sizeof(char));
    if ( nm_cmd == NULL )
      vt_error();

    /* compose nm command */
    snprintf(nm_cmd, nm_cmd_len, "%s %s", nm, apppath);

    /* execute nm command */
    vt_cntl_msg("Executing %s", nm_cmd);
    nm_stream = popen(nm_cmd, "r");
    /* error handling after pclose below */

    nm_filename = NULL;
  }

  /* allocate memory for lines */
  line = (char*)malloc(NM_LINE_BLK_LEN * sizeof(char));
  if ( line == NULL )
    vt_error();
  line_size = NM_LINE_BLK_LEN;

  /* read lines */

  while( nm_stream != NULL && fgets(line, line_size, nm_stream) )
  {
    char* col;
    char  delim[2] = " ";
    int   nc = 1;

    long  addr = 0;
    char* filename = NULL;
    char* funcname = NULL;
    unsigned int lno = VT_NO_LNO;

    lineno++;

    /* trigger a parse error, if line is empty */
    if ( strlen(line) == 0 )
    {
      parse_error = 1;
      break;
    }

    /* if line seems to be incomplete, enlarge line buffer and read the
       remaining line */
    while( !parse_error && line[strlen(line)-1] != '\n' )
    {
      char tmp[NM_LINE_BLK_LEN];

      /* read the remaining line; if it fails (EOF) the line seems to
         be complete after all */
      if ( !fgets(tmp, sizeof(tmp), nm_stream) )
        break;

      /* trigger a parse error, if line is to long (>NM_LINE_MAX_LEN) */
      if ( line_size + NM_LINE_BLK_LEN > NM_LINE_MAX_LEN )
      {
        parse_error = 1;
        break;
      }

      /* enlarge line buffer */
      line = (char*)realloc(line, (line_size + NM_LINE_BLK_LEN) * sizeof(char));
      if ( line == NULL )
        vt_error();
      line_size += NM_LINE_BLK_LEN;

      /* complete line */
      strcat(line, tmp);
    }
    if ( parse_error )
      break;

    /* chop new-line character from line */
    if ( line[strlen(line)-1] == '\n' )
      line[strlen(line)-1] = '\0';

    /* split line to columns */
    col = strtok(line, delim);
    do
    {
      if ( nc == 1 ) /* column 1 (address) */
      {
        /* ignore symbol, if it has no address */
        if ( strlen(col) == 1 )
          break;

        addr = strtol(col, NULL, 16);
      }
      else if ( nc == 2 ) /* column 2 (type) */
      {
        /* type must have a length of 1 */
        if ( strlen(col) != 1 )
        {
          parse_error = 1;
          break;
        }
        strcpy(delim, "\t");
      }
      else if ( nc == 3 ) /* column 3 (symbol) */
      {
        funcname = col;
        strcpy(delim, ":");
      }
      else if( nc == 4 ) /* column 4 (filename) */
      {
        filename = col;
      }
      else /* column 5 (line) */
      {
        lno = atoi(col);
        if( lno == 0 ) lno = VT_NO_LNO;
        break;
      }

      nc++;
      col = strtok(0, delim);
    } while( col );

    /* stop reading file, if an parse error occurred */
    if ( parse_error )
    {
      break;
    }
    /* add symbol to hash table, if we have its address */
    else if ( addr > 0 )
    {
      char* n = strdup(funcname);
      hash_put(addr, n, filename, lno);
    }
  }

  /* close file/pipe stream */

  if ( nm_filename != NULL )
  {
    fclose(nm_stream);

    if ( parse_error )
    {
      vt_error_msg("%s:%u: could not be parsed.\n"
                   "Please check the content of %s for BSD-style.",
                   nm_filename, lineno, nm_filename);
    }
  }
  else
  {
    uint8_t nmcmd_error = (nm_stream == NULL || pclose(nm_stream) != 0);

    if ( parse_error )
    {
      vt_error_msg("Could not parse 'nm' output created with %s.\n"
                   "Please set the environment variable VT_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_NMFILE to a pre-created symbol list file.",
                   nm_cmd);
    }
    else if ( nmcmd_error )
    {
      vt_error_msg("Failed to execute %s\n"
                   "Please set the environment variable VT_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_NMFILE to a pre-created symbol list file.",
                   nm_cmd);
    }

    free(nm_cmd);
    free(apppath);
  }

  free(line);

  VT_RESUME_IO_TRACING();
}

/*
 * Register new region
 */

static void register_region(HashNode *hn) {
  uint32_t fid = VT_NO_ID;
  uint32_t lno = VT_NO_LNO;

  /* -- register file if available -- */
  if (hn->fname != NULL)
  {
    fid = vt_def_file(hn->fname);
    lno = hn->lno;
  }

  /* -- register region and store region identifier -- */
  hn->vtid = vt_def_region(hn->name, fid, lno, VT_NO_LNO, VT_DEF_GROUP, VT_FUNCTION);
}


void __cyg_profile_func_enter(void* func, void* callsite);
void __cyg_profile_func_exit(void* func, void* callsite);

/*
 * This function is called at the entry of each function
 * The call is generated by the GNU/Intel (>=v10) compilers
 */

void __cyg_profile_func_enter(void* func, void* callsite) {
  HashNode *hn;

  void * funcptr = func;

  uint64_t time;

#ifdef __ia64__
  funcptr = *( void ** )func;
#endif

  /* -- if not yet initialized, initialize VampirTrace -- */
  if ( gnu_init ) {
    VT_MEMHOOKS_OFF();
    gnu_init = 0;
    vt_open();
    get_symtab();
    VT_MEMHOOKS_ON();
  }

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( (hn = hash_get((long)funcptr))) {
    if ( hn->vtid == VT_NO_ID ) {
      /* -- region entered the first time, register region -- */
#     if defined (VT_OMPI) || defined (VT_OMP)
      if (omp_in_parallel()) {
#       pragma omp critical (vt_comp_gnu_1)
	{
	  if ( hn->vtid == VT_NO_ID ) {
	    register_region(hn);
	  }
	}
      } else {
	register_region(hn);
      }
#     else
      register_region(hn);
#     endif
    }

    /* -- write enter record -- */
    vt_enter(&time, hn->vtid);
  }

  VT_MEMHOOKS_ON();
}

/*
 * This function is called at the exit of each function
 * The call is generated by the GNU/Intel (>=v10) compilers
 */

void __cyg_profile_func_exit(void* func, void* callsite) {
  void * funcptr = func;
  uint64_t time;

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

#ifdef __ia64__
  funcptr = *( void ** )func;
#endif

  if ( hash_get((long)funcptr) ) {
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();
}
