/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2009, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#ifdef VT_BFD
#  include "bfd.h"
#  if defined(HAVE_GNU_DEMANGLE) && HAVE_GNU_DEMANGLE
#    if defined(HAVE_DEMANGLE_H) && HAVE_DEMANGLE_H
#      include "demangle.h"
#    else /* HAVE_DEMANGLE_H */
      extern char* cplus_demangle (const char* mangled, int options);
#     define DMGL_NO_OPTS 0
#     define DMGL_PARAMS  (1 << 0)
#     define DMGL_ANSI    (1 << 1)
#     define DMGL_JAVA    (1 << 2)
#     define DMGL_VERBOSE (1 << 3)
#     define DMGL_TYPES   (1 << 4)
#    endif /* HAVE_DEMANGLE_H */
#  endif /* HAVE_GNU_DEMANGLE */
#endif /* VT_BFD */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include "vt_comp.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_iowrap.h"
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_trc.h"
#include "vt_thrd.h"

static int gnu_init = 1;       /* is initialization needed? */

/*
 *-----------------------------------------------------------------------------
 * Simple hash table to map function addresses to region names/identifier
 *-----------------------------------------------------------------------------
 */

typedef struct HN {
  long id;            /* hash code (address of function */
  char* name;         /* associated function name       */
  char* fname;        /*            file name           */
  int lno;            /*            line number         */
  uint32_t vtid;      /* associated region identifier   */
  struct HN* next;
} HashNode;

#define HASH_MAX 1021

static HashNode* htab[HASH_MAX];
static uint32_t n_htab_entries = 0;

/*
 * Stores function name `n' under hash code `h'
 */

static void hash_put(long h, const char* n, const char* fn, int lno) {
  long id = h % HASH_MAX;
  HashNode *add = (HashNode*)malloc(sizeof(HashNode));
  add->id = h;
  add->name  = (char*)n;
  add->fname = fn ? strdup(fn) : (char*)fn;
  add->lno   = lno;
  add->vtid = VT_NO_ID;
  add->next = htab[id];
  htab[id] = add;
  n_htab_entries++;
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

#ifdef VT_BFD

/*
 * Get symbol table by using BFD
 */

static void get_symtab_bfd(void) {
   bfd * BfdImage = 0;
   int nr_all_syms;
   int i; 
   size_t size;
   char* exe_env;
   asymbol **syms;
   int do_getsrc = vt_env_gnu_getsrc();
#if defined(HAVE_GNU_DEMANGLE) && HAVE_GNU_DEMANGLE
   int do_demangle = vt_env_gnu_demangle();
#endif /* HAVE_GNU_DEMANGLE */

   /* initialize BFD */
   bfd_init();

   /* get executable path from environment var. VT_APPPATH */
   exe_env = vt_env_apppath();
   if ( ! exe_env )
   {
     vt_error_msg("Could not determine path of executable.\n"
		  "There are two possible ways to solve this problem:\n"
		  "Set either the environment variable VT_APPPATH to the path of the executable or set VT_GNU_NMFILE to a symbol list file, created with 'nm'.");
   }
   else
   {
     /* get executable image */
     BfdImage = bfd_openr(exe_env, 0 );
     if ( ! BfdImage )
       vt_error_msg("BFD: bfd_openr(): failed\n"
		    "Could not get executable image from %s.\n"
		    "A possible solution to the problem is to set the environment variable VT_GNU_NMFILE to a symbol list file, created with 'nm'.", exe_env);
   }

   /* check image format */
   if ( ! bfd_check_format(BfdImage, bfd_object) ) { 
     vt_error_msg("BFD: bfd_check_format(): failed");
   }

   /* return if file has no symbols at all */
   if ( ! ( bfd_get_file_flags(BfdImage) & HAS_SYMS ) )
     vt_error_msg("BFD: bfd_get_file_flags(): failed");

   /* get the upper bound number of symbols */
   size = bfd_get_symtab_upper_bound(BfdImage);

   /* HAS_SYMS can be set even with no symbols in the file! */
   if ( size < 1 )
     vt_error_msg("BFD: bfd_get_symtab_upper_bound(): < 1");

   /* read canonicalized symbols */
   syms = (asymbol **)malloc(size);
   nr_all_syms = bfd_canonicalize_symtab(BfdImage, syms);
   if ( nr_all_syms < 1 )
     vt_error_msg("BFD: bfd_canonicalize_symtab(): < 1");

   for (i=0; i<nr_all_syms; ++i) {
      char* dem_name = NULL;
      long addr;
      const char* filename;
      const char* funcname;
      unsigned int lno;

      /* ignore symbols that are not a function */
      if ( !(syms[i]->flags & BSF_FUNCTION) ) continue;

      /* ignore system functions */
      if ( syms[i]->name[0] == '.' ||
           strncmp(syms[i]->name, "bfd_", 4) == 0 ||
           strstr(syms[i]->name, "@@") != NULL ) continue;

      /* get filename and linenumber from debug info */
      /* needs -g */
      filename = NULL;
      lno = VT_NO_LNO;
      if ( do_getsrc ) {
	bfd_find_nearest_line(BfdImage, bfd_get_section(syms[i]), syms,
			      syms[i]->value, &filename, &funcname, &lno);
      }

      /* calculate function address */
      addr = syms[i]->section->vma+syms[i]->value;

      /* use demangled name if possible */
#if defined(HAVE_GNU_DEMANGLE) && HAVE_GNU_DEMANGLE
      if ( do_demangle ) {
	dem_name = cplus_demangle(syms[i]->name,
				  DMGL_PARAMS | DMGL_ANSI 
				  | DMGL_VERBOSE | DMGL_TYPES);
      }
#endif /* HAVE_GNU_DEMANGLE */

      if( dem_name ) {
	hash_put(addr, dem_name, filename, lno);
      } else {
	char* n = strdup(syms[i]->name);
	hash_put(addr, n, filename, lno);
      }
   }

   free(syms);
   bfd_close(BfdImage);
   return;
}
#endif

/*
 * Get symbol table by parsing nm-file
 */

static void get_symtab_nm(const char* nmfilename)
{
  FILE* nmfile;
  char  line[1024];
  int   do_getsrc = vt_env_gnu_getsrc();

  /* open nm-file */
  if( !(nmfile = fopen(nmfilename, "r")) )
    vt_error_msg("Could not open symbol list file %s", nmfilename);

  /* read lines */
  while( fgets( line, sizeof(line)-1, nmfile ) )
  {
    char* col;
    char  delim[2] = " ";
    int   nc = 0;

    long  addr = -1;
    char* filename = NULL;
    char* funcname = NULL;
    unsigned int lno = VT_NO_LNO;

    if( strlen(line) == 0 || line[0] == ' ' )
      continue;

    if( line[strlen(line)-1] == '\n' )
      line[strlen(line)-1] = '\0';
    
    /* split line to columns */
    col = strtok(line, delim);
    do
    {
      if( nc == 0 ) /* column 1 (address) */
      {
	addr = strtol(col, NULL, 16);
	if( addr == 0 )
	  break;
      }
      else if( nc == 1 ) /* column 2 (type) */
      {
	strcpy(delim, "\t");
      }
      else if( nc == 2 ) /* column 3 (symbol) */
      {
	funcname = col;
	strcpy(delim, ":");
      }
      else if( nc == 3 ) /* column 4 (filename) */
      {
	if( do_getsrc )
	  filename = col;
	else
	  break;
      }
      else /* column 5 (line) */
      {
	lno = atoi(col);
	if( lno == 0 ) lno = VT_NO_LNO;
	break;
      }
      
      nc++;
    } while( ( col = strtok(0, delim) ) );

    /* add symbol to hash table */
    if( nc >= 3 )
    {
      char* n = strdup(funcname);
      hash_put(addr, n, filename, lno);
    }
  }

  /* close nm-file */
  fclose(nmfile);
}

/*
 * Get symbol table either by using BFD or by parsing nm-file
 */
static void get_symtab(void)
{
  char* nmfilename = vt_env_gnu_nmfile();

  VT_SUSPEND_IO_TRACING();

  /* read nm-output file, if given? */
  if( nmfilename )
  {
    get_symtab_nm( nmfilename );
  }
  /* read application's executable by using BFD */
  else
  {
#ifdef VT_BFD
    get_symtab_bfd();
#else
    vt_error_msg("No symbol list file given. Please set the environment variable VT_GNU_NMFILE to the path of your symbol list file, created with 'nm'.");
#endif
  }

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
    fid = vt_def_scl_file(hn->fname);
    lno = hn->lno;
  }

  /* -- register region and store region identifier -- */
  hn->vtid = vt_def_region(hn->name, fid, lno, VT_NO_LNO, NULL, VT_FUNCTION);
}

void gnu_finalize(void);
void __cyg_profile_func_enter(void* func, void* callsite);
void __cyg_profile_func_exit(void* func, void* callsite);

/*
 * Finalize instrumentation interface
 */

void gnu_finalize()
{
  int i, idx_min, idx_max;
  uint32_t min, max, n;
  double avg;
  min = 0xffffffff;
  max = 0;
  idx_min = idx_max = 0;
  avg = 0.0;

  n = n_htab_entries;
  for( i = 0; i < HASH_MAX; i++ )
  {
    uint32_t n_bucket_entries = 0;
    while( htab[i] )
    {
      struct HN* next = htab[i]->next;
      free(htab[i]->name);
      if( htab[i]->fname ) free(htab[i]->fname);
      free(htab[i]);
      htab[i] = next;
      n_htab_entries--;
      n_bucket_entries++;
    }
    if( n_bucket_entries < min ) {
      min = n_bucket_entries;
      idx_min = i;
    }
    if( n_bucket_entries > max ) {
      max = n_bucket_entries;
      idx_max = i;
    }
    avg += n_bucket_entries;
    vt_cntl_msg( 3, "Hash bucket %i had %u entries (%.1f/1000)", i, n_bucket_entries, ((double)n_bucket_entries*1000)/n );
  }
  avg /= HASH_MAX;
  vt_cntl_msg( 3, "Hash statistics:\n"
                  "\tNumber of entries: %u\n"
                  "\tMin bucket size:   %u (%.1f/1000) at index %i\n"
                  "\tMax bucket size:   %u (%.1f/1000) at index %i\n"
                  "\tAvg bucket size:   %.1f",
                  n,
                  min, ((double)min*1000)/n, idx_min,
                  max, ((double)max*1000)/n, idx_max,
                  avg );
  vt_assert( n_htab_entries==0 );
}

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
    vt_comp_finalize = gnu_finalize;
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
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
      if( hn->vtid == VT_NO_ID )
        register_region(hn);
      VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
      register_region(hn);
#endif /* VT_MT || VT_HYB */
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

  /* -- write exit record -- */
  if ( hash_get((long)funcptr) ) {
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();
}
