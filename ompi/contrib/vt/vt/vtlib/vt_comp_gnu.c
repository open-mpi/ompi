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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef VT_BFD
#  include "bfd.h"
#  if defined(HAVE_GNU_DEMANGLE) && HAVE_GNU_DEMANGLE
#    include "demangle.h"
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
#include "vt_memhook.h"
#include "vt_pform.h"
#include "vt_strdup.h"
#include "vt_trc.h"
#if (defined (VT_OMPI) || defined (VT_OMP))
#  include <omp.h>
#endif

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
  add->fname = fn ? (const char*)vt_strdup(fn) : fn;
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
#if defined(HAVE_GNU_DEMANGLE) && HAVE_GNU_DEMANGLE
   int do_demangle = vt_env_do_demangle();
#endif /* HAVE_GNU_DEMANGLE */

   /* initialize BFD */
   bfd_init();

   /* get executable path from environment var. VT_APPPATH */
   exe_env = vt_env_apppath();
   if ( exe_env )
   {
     /* get executable image */
     BfdImage = bfd_openr(exe_env, 0 );
     if ( ! BfdImage )
       vt_error_msg("BFD: Could not get executable image from %s.\n"
		    "A possible solution to the problem is to set the "
		    "environment variable VT_NMFILE\n"
		    "to a symbol list file, created with 'nm'.", exe_env);
   }
   else
   {
     int pid = getpid();
     char exe[256];

     sprintf(exe, "/proc/%d/exe", pid);
     BfdImage = bfd_openr(exe, 0 );
     if ( ! BfdImage ) {
       sprintf(exe, "/proc/%d/object/a.out", pid);
       BfdImage = bfd_openr(exe, 0 );
      
       if ( ! BfdImage ) {
	 vt_error_msg("BFD: Could not get executable image.\n"
		      "There are two possible ways to solve this problem:\n"
		      "Set either the environment variable VT_APPPATH to the "
		      "path of your application\n"
		      "or set VT_NMFILE to a symbol list file, created with "
		      "'nm'.", exe);
       }
     }
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
      char* dem_name = 0;
      long addr;
      const char* filename;
      const char* funcname;
      unsigned int lno;
      
      /* ignore system functions */
      if ( strncmp(syms[i]->name, "__", 2) == 0 ||
	   strncmp(syms[i]->name, "bfd_", 4) == 0 ||
	   strstr(syms[i]->name, "@@") != NULL ) continue;

      /* get filename and linenumber from debug info */
      /* needs -g */
      filename = NULL;
      lno = -1;
      bfd_find_nearest_line(BfdImage, bfd_get_section(syms[i]), syms,
			    syms[i]->value, &filename, &funcname, &lno);

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
	char *n = vt_strdup(syms[i]->name);
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

static void get_symtab_nm(void)
{
  char* nmfilename = vt_env_nmfile();
  FILE* nmfile;
  char  line[4096];

  /* open nm-file */
  if( !(nmfile = fopen(nmfilename, "r")) )
    vt_error_msg("Could not open symbol list file %s", nmfilename);

  /* read lines */
  while( fgets( line, 4096, nmfile ) )
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
	filename = col;
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
      char* n = vt_strdup(funcname);
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
  /* read nm-output file, if given? */
  if( vt_env_nmfile() )
  {
    get_symtab_nm();
  }
  /* read application's executable by using BFD */
  else
  {
#ifdef VT_BFD
    get_symtab_bfd();
#else
    vt_error_msg("No symbol list file given. Please set the environment variable VT_NMFILE to the path of your symbol list file, created with 'nm'.");
#endif
  }
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
  HashNode *hn;
  void * funcptr = func;
  uint64_t time;

  VT_MEMHOOKS_OFF();

  time = vt_pform_wtime();

#ifdef __ia64__
  funcptr = *( void ** )func;
#endif

  if ( (hn = hash_get((long)funcptr)) ) {
    vt_exit(&time);
  }

  VT_MEMHOOKS_ON();
}
