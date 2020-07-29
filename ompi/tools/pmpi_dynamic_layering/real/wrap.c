/*
 * Copyright (c) 2019-2020 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 */

/*
 * Note about dlopen() vs opal_dl_open():
 * This file contains wrappers for all the MPI_* / PMPI_* routines and initializes
 * itself on first-call. So it's performing all its dlopens before MPI_Init, so
 * the various MCAs that would normally be present aren't set up yet. It might
 * be possible to manually load MCAs I depend on, but for now I'm just using
 * vanilla dlopen().
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifndef _WIN32
#include <dlfcn.h>
#include <pthread.h>
#else
#include <windows.h>
#endif

#include "opal/util/parse_entry.h"
#include "opal/mca/dl/base/base.h"

#include <mpi.h>

/*
 *  This file wraps the MPI entrypoints in such a way that other
 *  wraped-usage of the MPI/PMPI interface can be layered multiple times.
 *
 *  A normal wrapper-library like we're trying to support (call it libwrap)
 *  might define MPI_Foo and call PMPI_Foo (in libpcmpi or whatever is the
 *  next library in the chain).  It might also define MPI_F1 (in libwrap)
 *  which calls MPI_F2 (also in libwrap) which calls PMPI_F2 (in libpcmpi).
 *  An example of this might be a fortran interface (mpi_send -> MPI_Send
 *  -> PMPI_Send).
 *
 *  In general when a wrapper library calls MPI_Anything it wants to call
 *  back into itself, and when a wrapper library calls PMPI_Anything it
 *  wants to go down to the next library.
 */

#define MAX_LEVELS 20
static int nwrapper_levels;

/*
 * -------------------------------------------------------------------
 * constructed_wrapper_variable_defs.h has lines of the form
 *
 * static int (*(fptr_MPI_Send[MAX_LEVELS]))(void* buf, int count, ....);
 * static void (*(fptr_mpi_send[MAX_LEVELS]))(void* buf, void *count, ....);
 * -------------------------------------------------------------------
 */
typedef void (*voidfuncptr)(void);
#include "constructed_wrapper_variable_defs.h"

#ifndef _WIN32
  static pthread_key_t depthkey;
  static pthread_once_t key_once = PTHREAD_ONCE_INIT;
#else
  static PVOID lpContext; /* required in InitOnceE call (?) but not used by us*/
  static DWORD depthkey;
  static INIT_ONCE key_once;
#endif

#ifndef _WIN32
  #define INIT_STUFF \
    pthread_once(&key_once, wrappers_init); \
    calldepth = (int*) pthread_getspecific(depthkey); \
    if (calldepth == NULL) { \
      int *p = malloc(4); \
      *p = 0; \
      pthread_setspecific(depthkey, p); \
    }
#else
/*
 *  Note, the current windows implementation leaks one malloc(4) for
 *  each thread that calls MPI routines.  It may be possible to avoid
 *  this by using a DllMain with DLL_THREAD_DETACH as shown here:
 *    http://msdn.microsoft.com/en-us/library/ms686997%28VS.85%29.aspx
 */
  #define INIT_STUFF \
    InitOnceExecuteOnce(&key_once, wrappers_init, NULL, &lpContext); \
    calldepth = (int*) TlsGetValue(depthkey); \
    if (calldepth == NULL) { \
      int *p = malloc(4); \
      *p = 0; \
      TlsSetValue(depthkey, p); \
    }
#endif

#ifndef _WIN32
#define GET_CALLDEPTH \
  calldepth = (int*) pthread_getspecific(depthkey);
#else
#define GET_CALLDEPTH \
  calldepth = (int*) TlsGetValue(depthkey);
#endif

/*
 *  fort_dlsym is used when dlsyming fortran symbols.  An MPI lib might
 *  define the full range of likely symbol names for fortran functions
 *  (mpi_send, mpi_send_, mpi_send__) but it's possible that some other
 *  library like libMPE.so might only define mpi_send_ for example.  So
 *  when we want to dlsym a fortran function pointer, we have to look
 *  for and accept any of the various possibilities.
 */
#ifndef _WIN32
void *
#else
HINSTANCE
#endif
fort_dlsym(
#ifndef _WIN32
  void *handle,
#else
  HINSTANCE handle,
#endif
  char *fname);

#ifndef _WIN32
void *
#else
HINSTANCE
#endif
fort_dlsym(
#ifndef _WIN32
  void *handle,
#else
  HINSTANCE handle,
#endif
  char *fname)
{
  void *p;
  char name[256];
  int i;

#ifndef _WIN32
#  define mydlsym dlsym
#  define mystrcpy(out,in) strcpy(out,in)
#  define mystrcat(out,in) strcat(out,in)
#else
#  define mydlsym(a,b) GetProcAddress(a,TEXT(b))
#  define mystrcpy(out,in) strncpy_s(out,256,in,256)
#  define mystrcat(out,in) strncat_s(out,256,in,256)
#endif
  mystrcpy(name, fname);
  p = mydlsym(handle, name);
  if (p) { return(p); }

  mystrcat(name, "_");
  p = mydlsym(handle, name);
  if (p) { return(p); }

  mystrcat(name, "_");
  p = mydlsym(handle, name);
  if (p) { return(p); }

  mystrcpy(name, fname);
  for (i=0; name[i]; ++i) { /* toupper */
    if (name[i] >= 'a' && name[i] <= 'z') {
      name[i] += ('A' - 'a');
    }
  }
  p = mydlsym(handle, name);
  if (p) { return(p); }

  mystrcat(name, "_");
  p = mydlsym(handle, name);
  if (p) { return(p); }

  mystrcat(name, "_");
  p = mydlsym(handle, name);
  if (p) { return(p); }

  return(0);
}

/*
 * struct describing where to lookup symbols for a given level
 * It can specify
 * - just fortran or all available symbols
 * - usebase  : use the bottom level interface, eg baselib[]
 * - handle : for every level other than a "usebase" level, there's a handle
 * It relies on baselib[]/nbaselib already existing.
 */
typedef struct level_s {
    int just_fortran;
    int usebase;
#ifndef _WIN32
    void *handle;
#else
    HINSTANCE handle;
#endif
} level_t;

static level_t levelinfo[MAX_LEVELS];
#ifndef _WIN32
static void *baselib[MAX_LEVELS];
#else
static HINSTANCE baselib[MAX_LEVELS];
#endif
static int nbaselib;

void *lookup_fp(int i, int isfortran, char *funcname);

#ifndef _WIN32
void
wrappers_init(void);
#else
BOOL CALLBACK
wrappers_init(PINIT_ONCE InitOnce, PVOID Parameter, PVOID *lpContext);
#endif

#ifndef _WIN32
void
wrappers_init(void)
#else
BOOL CALLBACK
wrappers_init(PINIT_ONCE InitOnce, PVOID Parameter, PVOID *lpContext)
#endif
{
    char *p;
#ifdef _WIN32
    char libname[1024];
#endif
    int fortran_specified;
    char **lib_names, **baselib_names;
    int nlib_names, nbaselib_names;

    int i, entry_is_active;
    /*
     * --mca tools_entry v   print libs list at the schizo level
     *       tools_entry vv  unused, could print more stuff here
     */
    ompi_entry_parse_mca(&entry_is_active, NULL, NULL,
        &lib_names, &nlib_names,
        &baselib_names, &nbaselib_names);

/* Use the entry_base list to get the bottom level where the real MPI
 * calls are.
 */
    nbaselib = nbaselib_names;
    for (i=0; i<nbaselib_names; ++i) {
        p = baselib_names[i];
#ifndef _WIN32
        baselib[i] = dlopen(p, RTLD_NOW|RTLD_GLOBAL);
#else
        baselib[i] = LoadLibrary(TEXT(p));
#endif
        if (!baselib[i]) {
#ifndef _WIN32
            fprintf(stderr, "Could not open baselib %s (%s)\n", p, dlerror());
#else
            fprintf(stderr, "Could not open baselib %s (%d)\n", p, GetLastError());
#endif
            exit(1);
        }
    }

/*
 *  Fill in levelinfo[] and nwrapper_levels. There will be a .handle for
 *  each -mca tools_entry lib the user specifies.  levelinfo[] can also be a
 *  "base" level, ie the bottom level MPI implementation, or a "fortran"
 *  level which is just the fortran symbols from the base level.
 *
 *  By default a fortran level from the base product is also put at the top.
 *
 *  The user has -mca tools_entry <options> and -mca tools_entry_base <options>
 *  controls for:
 *    entry: libmpe.so,libfoo.so  :  symbols for lib[] levels
 *    entry: fort                 :  specify where to put the base-product
 *                                   fortran symbols in the ordering. The
 *                                   default is equivalent to "fort,stuff",
 *                                   while "stuff,fort" is equivalent to
 *                                   not treating fortran as a special case
 *                                   at all.
 *    entry_base: libmpi.so,libmpif.so  :  where to get the MPI functions for
 *                                   the base product
 *    entry: v : for verbose
 */
    fortran_specified = 0;
    nwrapper_levels = nlib_names;

    for (i=0; i<nlib_names; ++i) {
        p = lib_names[i];

        if (0==strcmp(p, "fort") || 0==strcmp(p, "fortran")) {
            fortran_specified = 1;
            levelinfo[i].just_fortran = 1;
            levelinfo[i].usebase = 1;
        } else {
#ifndef _WIN32
            levelinfo[i].handle = dlopen(p, RTLD_NOW|RTLD_GLOBAL);
#else
            levelinfo[i].handle = LoadLibrary(TEXT(p));
#endif
            levelinfo[i].just_fortran = 0;
            levelinfo[i].usebase = 0;

            if (!levelinfo[i].handle) {
#ifndef _WIN32
                fprintf(stderr, "Could not open lib %s (%s)\n",
                    p, dlerror());
#else
                fprintf(stderr, "Could not open lib %s (%d)\n",
                    p, GetLastError());
#endif
                exit(1);
            }
        }
    }
/*
 * If fortran wasn't specified, add it to the top of the list of levels
 * And add the base product as a bottom level (although if the bottom level
 * is fortran, let that become the full base product instead of just fortran)
 */
    if (!fortran_specified) {
        for (i=nwrapper_levels; i>0; --i) {
            levelinfo[i] = levelinfo[i-1];
        }
        ++nwrapper_levels;
        levelinfo[0].just_fortran = 1;
        levelinfo[0].usebase = 1;
    }
    if (levelinfo[nwrapper_levels-1].just_fortran) {
        /* the last item was 'fort', so make it the full base product */
        levelinfo[nwrapper_levels-1].just_fortran = 0;
    } else {
        /* add a bottom level for the base product */
        levelinfo[nwrapper_levels].just_fortran = 0;
        levelinfo[nwrapper_levels].usebase = 1;
        ++nwrapper_levels;
    }

    free(lib_names);
    free(baselib_names);
/*
 * -------------------------------------------------------------------
 * constructed_wrapper_variable_assignments.h has lines of the form
 *
 *  fptr_MPI_Send[i] = lookup_fp(i, isfortran, "MPI_Send", &MPI_Send);
 *  fptr_mpi_send[i] = lookup_fp(i, isfortran, "mpi_send", &mpi_send);
 * -------------------------------------------------------------------
 */
    for (i=0; i<nwrapper_levels; ++i) {
#include "constructed_wrapper_variable_assignments.h"
    }

#ifndef _WIN32
    pthread_key_create(&depthkey, free);
#else
    depthkey = TlsAlloc();
    return(TRUE);
#endif
}

void *
lookup_fp(int i, int isfortran, char *funcname) {
    void *fp; /* dlsym at desired library level */
    fp = NULL;
/* deffp is for an RTLD_DEFAULT lookup which would be
 * libmpiprofilesupport's symbol. That is what would
 * would show up in a dlsym if a libuserwrapper.so didn't
 * define a symbol, in which case we want NULL.
 */
    void *deffp = NULL;
    if (!(levelinfo[i].just_fortran) || isfortran) {
        if (!levelinfo[i].usebase) {
            if (!isfortran) {
                fp = (void*) mydlsym(levelinfo[i].handle, funcname);
                deffp = (void*) mydlsym(RTLD_DEFAULT, funcname);
            } else {
                fp = (void*) fort_dlsym(levelinfo[i].handle, funcname);
                deffp = (void*) fort_dlsym(RTLD_DEFAULT, funcname);
            }
        } else {
            fp = NULL;
            int j;
            for (j=0; j<nbaselib && !fp; ++j) {
                if (!isfortran) {
                    fp = (void*) mydlsym(baselib[j], funcname);
                    deffp = (void*) mydlsym(RTLD_DEFAULT, funcname);
                } else {
                    fp = (void*) fort_dlsym(baselib[j], funcname);
                    deffp = (void*) fort_dlsym(RTLD_DEFAULT, funcname);
                }
            }
        }
    }
    if (fp == deffp) { fp = NULL; }
    return fp;
}

/*
 * -------------------------------------------------------------------
 * constructed_wrapper_cfunctions.h and
 * constructed_wrapper_ffunctions.h
 * have the generated functions for everything.
 * I'm not pasting a tempalte here since it would just bitrot.
 */
#include "constructed_wrapper_cfunctions.h"
#include "constructed_wrapper_ffunctions.h"

/*
 * -------------------------------------------------------------------
 * Except for MPI_Pcontrol/mpi_pcontrol which are handled differently:
 * This could be called from the app level, where calldepth==0,
 * or maybe from a library that wraps some MPI calls to enable/disable
 * tracing by calling MPI_Pcontrol() from the other.  So in general 
 * we want to walk through calling all the MPI_Pcontrol at or below
 * our depth.  PMPI_Pcontrol isn't a thing, but if somebody did call it
 * it would resolve into libmpi.so skipping all this.
 */
int MPI_Pcontrol(const int v0, ...);
int
MPI_Pcontrol(const int v0, ...) {
  int rv, i;
  int entrylev;
  int *calldepth;
  rv = MPI_SUCCESS;
  INIT_STUFF
#ifndef _WIN32
  calldepth = (int*) pthread_getspecific(depthkey);
#else
  calldepth = (int*) TlsGetValue(depthkey);
#endif
  entrylev = *calldepth;
  for (i=entrylev; i<nwrapper_levels; ++i) {
    if (fptr_MPI_Pcontrol[i]) {
      *calldepth = i;
      rv = fptr_MPI_Pcontrol[i](v0);
    }
  }
  *calldepth = entrylev;
  return(rv);
}
void mpi_pcontrol(void* v0);
void
mpi_pcontrol(void* v0) {
  int i;
  int entrylev;
  int *calldepth;
  INIT_STUFF
#ifndef _WIN32
  calldepth = (int*) pthread_getspecific(depthkey);
#else
  calldepth = (int*) TlsGetValue(depthkey);
#endif
  entrylev = *calldepth;
  for (i=entrylev; i<nwrapper_levels; ++i) {
    if (fptr_mpi_pcontrol[i]) {
      *calldepth = i;
      fptr_mpi_pcontrol[i](v0);
    }
  }
  *calldepth = entrylev;
  return;
}
void mpi_pcontrol_(void* v0);
void
mpi_pcontrol_(void* v0) {
  mpi_pcontrol(v0);
  return;
}
void mpi_pcontrol__(void* v0);
void
mpi_pcontrol__(void* v0) {
  mpi_pcontrol(v0);
  return;
}
