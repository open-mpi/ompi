/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_IOWRAP_H_
#define _VT_IOWRAP_H_

#include "config.h"

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if (defined(VT_IOWRAP))

#include "vt_error.h"
#include "vt_thrd.h"
#include "vt_inttypes.h"


#define VT_ENABLE_IO_TRACING() \
  VT_CHECK_THREAD; \
  VTTHRD_IO_TRACING_ENABLED(VTTHRD_MY_VTTHRD) = 1; \
  vt_debug_msg( DBG_INIT, "ENABLED I/O tracing (susp=%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(VTTHRD_MY_VTTHRD), __LINE__ );

#define VT_DISABLE_IO_TRACING() \
  VT_CHECK_THREAD; \
  VTTHRD_IO_TRACING_ENABLED(VTTHRD_MY_VTTHRD) = 0; \
  vt_debug_msg( DBG_INIT, "DISABLED I/O tracing (susp=%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(VTTHRD_MY_VTTHRD), __LINE__ );

#define VT_SUSPEND_IO_TRACING() \
  VT_CHECK_THREAD; \
  { \
    VTThrd *thrd = VTTHRD_MY_VTTHRD; \
    if( VTTHRD_IO_TRACING_ENABLED(thrd) ) { \
      VTTHRD_IO_TRACING_STATE(thrd) = VTTHRD_IO_TRACING_ENABLED(thrd); \
      VTTHRD_IO_TRACING_SUSPEND_CNT(thrd)++; \
      VTTHRD_IO_TRACING_ENABLED(thrd) = 0; \
      vt_debug_msg( DBG_INIT, "SUSPENDED I/O tracing (%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(thrd), __LINE__ ); \
    } \
    else { \
      VTTHRD_IO_TRACING_SUSPEND_CNT(thrd)++; \
      vt_debug_msg( DBG_INIT, "SUSPENDED I/O tracing (%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(thrd), __LINE__ ); \
    } \
  }

#define VT_RESUME_IO_TRACING() \
  { \
    VTThrd *thrd = VTTHRD_MY_VTTHRD; \
    vt_debug_msg( DBG_INIT, "TRY RESUME I/O tracing (%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(thrd), __LINE__ ); \
    if( VTTHRD_IO_TRACING_SUSPEND_CNT(thrd) > 0 ) { \
      if( (--VTTHRD_IO_TRACING_SUSPEND_CNT(thrd)) == 0 ) { \
        vt_debug_msg( DBG_INIT, "RESUMED I/O tracing (%hhu) at " __FILE__ ", %i", VTTHRD_IO_TRACING_SUSPEND_CNT(thrd), __LINE__ ); \
        VTTHRD_IO_TRACING_ENABLED(thrd) = VTTHRD_IO_TRACING_STATE(thrd); \
      } \
    } \
  }

/* TODO: fflush */
enum func_id {
	open_IDX,
	open64_IDX,
	creat_IDX,
	creat64_IDX,
	close_IDX,
	dup_IDX,
	dup2_IDX,
	lseek_IDX,
	lseek64_IDX,
	read_IDX,
	write_IDX,
	readv_IDX,
	writev_IDX,
	pread_IDX,
	pwrite_IDX,
	pread64_IDX,
	pwrite64_IDX,
	fdopen_IDX,
	fopen_IDX,
	fopen64_IDX,
	fclose_IDX,
	fseek_IDX,
	fseeko_IDX,
	fseeko64_IDX,
	rewind_IDX,
	fsetpos_IDX,
	fsetpos64_IDX,
	fread_IDX,
	fwrite_IDX,
	fgetc_IDX,
	getc_IDX,
	fgets_IDX,
	gets_IDX,
	fputc_IDX,
	putc_IDX,
	fputs_IDX,
	puts_IDX,
	fscanf_IDX,
	fprintf_IDX,
	unlink_IDX,
	flockfile_IDX,
	ftrylockfile_IDX,
	funlockfile_IDX,
	lockf_IDX,
	fcntl_IDX,
	NUMFUNCTIONS
};

struct iofunctions {
	int traceme;
	int vt_func_id;
/* The following is necessary to avoid "warning: ISO C forbids conversion of
 * object pointer to function pointer type". If the function calls break on some
 * platform, the cause would most possibly lie here.
 * Then sizeof(void *) != sizeof(<function pointer>)
 */
	union {
		void *p;
		void (*f)(void);
	} lib_func;
};


/* io wrapper initialization/finalization */
EXTERN void vt_iowrap_externals_init(void);
EXTERN void vt_iowrap_init(void);
EXTERN void vt_iowrap_reg(void);
EXTERN void vt_iowrap_finalize(void);

EXTERN int(*libc_fprintf)(FILE *, const char *, ...);

#define open_FUNCDEF		(int (*)(const char *, int, mode_t))
#define open_FUNCTYPE		VT_IOOP_OPEN
#define open64_FUNCDEF		(int (*)(const char *, int, mode_t))
#define open64_FUNCTYPE		VT_IOOP_OPEN
#define creat_FUNCDEF		(int (*)(const char *, mode_t))
#define creat_FUNCTYPE		VT_IOOP_OPEN
#define creat64_FUNCDEF		(int (*)(const char *, mode_t))
#define creat64_FUNCTYPE	VT_IOOP_OPEN
#define dup_FUNCDEF		(int (*)(int))
#define dup_FUNCTYPE		VT_IOOP_DUP
#define dup2_FUNCDEF		(int (*)(int, int))
#define dup2_FUNCTYPE		VT_IOOP_DUP
#define close_FUNCDEF		(int (*)(int))
#define close_FUNCTYPE		VT_IOOP_CLOSE
#define lseek_FUNCDEF		(off_t (*)(int, off_t, int))
#define lseek_FUNCTYPE		VT_IOOP_SEEK
#define lseek64_FUNCDEF		(off64_t (*)(int, off64_t, int))
#define lseek64_FUNCTYPE	VT_IOOP_SEEK
#define read_FUNCDEF		(ssize_t (*)(int, void *, size_t))
#define read_FUNCTYPE		VT_IOOP_READ
#define write_FUNCDEF		(ssize_t (*)(int, const void *, size_t))
#define write_FUNCTYPE		VT_IOOP_WRITE
#define readv_FUNCDEF		(int (*)(int, const struct iovec *, size_t))
#define readv_FUNCTYPE		VT_IOOP_READ
#define writev_FUNCDEF		(int (*)(int, const struct iovec *, size_t))
#define writev_FUNCTYPE		VT_IOOP_WRITE
#define pread_FUNCDEF		(ssize_t (*)(int, void *, size_t, off_t))
#define pread_FUNCTYPE		VT_IOOP_READ
#define pwrite_FUNCDEF		(ssize_t (*)(int, const void *, size_t, off_t))
#define pwrite_FUNCTYPE		VT_IOOP_WRITE
#define pread64_FUNCDEF		(ssize_t (*)(int, void *, size_t, off64_t))
#define pread64_FUNCTYPE	VT_IOOP_READ
#define pwrite64_FUNCDEF	(ssize_t (*)(int, const void *, size_t, off64_t))
#define pwrite64_FUNCTYPE	VT_IOOP_WRITE
#define fdopen_FUNCDEF		(FILE *(*)(int, const char *))
#define fdopen_FUNCTYPE		VT_IOOP_OPEN
#define fopen_FUNCDEF		(FILE *(*)(const char *, const char *))
#define fopen_FUNCTYPE		VT_IOOP_OPEN
#define fopen64_FUNCDEF		(FILE *(*)(const char *, const char *))
#define fopen64_FUNCTYPE	VT_IOOP_OPEN
#define fclose_FUNCDEF		(int (*)(FILE *))
#define fclose_FUNCTYPE		VT_IOOP_CLOSE
#define fseek_FUNCDEF		(int (*)(FILE *, long, int))
#define fseek_FUNCTYPE		VT_IOOP_SEEK
#define fseeko_FUNCDEF		(int (*)(FILE *, off_t, int))
#define fseeko_FUNCTYPE		VT_IOOP_SEEK
#define fseeko64_FUNCDEF	(int (*)(FILE *, off64_t, int))
#define fseeko64_FUNCTYPE	VT_IOOP_SEEK
#define rewind_FUNCDEF		(void (*)(FILE *))
#define rewind_FUNCTYPE		VT_IOOP_SEEK
#define fsetpos_FUNCDEF		(int (*)(FILE *, const fpos_t *))
#define fsetpos_FUNCTYPE	VT_IOOP_SEEK
#define fsetpos64_FUNCDEF	(int (*)(FILE *, const fpos64_t *))
#define fsetpos64_FUNCTYPE	VT_IOOP_SEEK
#define fread_FUNCDEF		(size_t (*)(void *, size_t, size_t, FILE *))
#define fread_FUNCTYPE		VT_IOOP_READ
#define fwrite_FUNCDEF		(size_t (*)(const void *, size_t, size_t, FILE *))
#define fwrite_FUNCTYPE		VT_IOOP_WRITE
#define fgetc_FUNCDEF		(int (*)(FILE *))
#define fgetc_FUNCTYPE		VT_IOOP_READ
#define getc_FUNCDEF		(int (*)(FILE *))
#define getc_FUNCTYPE		VT_IOOP_READ
#define fgets_FUNCDEF		(char *(*)(char *, int, FILE *))
#define fgets_FUNCTYPE		VT_IOOP_READ
#define gets_FUNCDEF		(char *(*)(char *))
#define gets_FUNCTYPE		VT_IOOP_READ
#define fputc_FUNCDEF		(int (*)(int, FILE *))
#define fputc_FUNCTYPE		VT_IOOP_WRITE
#define putc_FUNCDEF		(int (*)(int, FILE *))
#define putc_FUNCTYPE		VT_IOOP_WRITE
#define fputs_FUNCDEF		(int (*)(const char *, FILE *))
#define fputs_FUNCTYPE		VT_IOOP_WRITE
#define puts_FUNCDEF 		(int (*)(const char *))
#define puts_FUNCTYPE		VT_IOOP_WRITE
/* #define ungetc_FUNCDEF	(int (*)(int, FILE *)) */
#define fscanf_FUNCDEF		(int (*)(FILE *, const char *, ...))
#define fscanf_FUNCTYPE		VT_IOOP_READ
#define fprintf_FUNCDEF		(int (*)(FILE *, const char *, ...))
#define fprintf_FUNCTYPE	VT_IOOP_WRITE
/* #define vfscanf_FUNCDEF	(int (*)(FILE *, const char *, va_list)) */
/* #define vfprintf_FUNCDEF	(int (*)(FILE *, const char *, va_list)) */
#define unlink_FUNCDEF          (int (*)(const char *))
#define unlink_FUNCTYPE         VT_IOOP_UNLINK
#define flockfile_FUNCDEF       (void (*)(FILE *))
#define flockfile_FUNCTYPE      VT_IOOP_LOCK
#define ftrylockfile_FUNCDEF    (int (*)(FILE *))
#define ftrylockfile_FUNCTYPE   VT_IOOP_LOCK
#define funlockfile_FUNCDEF     (void (*)(FILE *))
#define funlockfile_FUNCTYPE    VT_IOOP_UNLOCK
#define lockf_FUNCDEF           (int (*)(int, int, off_t))
/* No need for lockf_FUNCTYPE, it is set inside the function */
#define fcntl_FUNCDEF           (int (*)(int, int, void *))
/* No need for fcntl_FUNCTYPE, it is set inside the function */

/* #define IOWRAP_REGION_DESCR_LEN	256 */
#define DBG_INIT	1
#define DBG_IO		2
#define DBG_VT_CALL	3
#define DBG_TRACECHK    4
#define DBG_FULL        255


#define FUNC_IDX(f) _FUNC_IDX(f)
#define _FUNC_IDX(f) f ## _IDX
#define VT_IOWRAP_FUNCDEF(f) f ## _FUNCDEF
#define VT_IOWRAP_FUNCTYPE(f) _VT_IOWRAP_FUNCTYPE(f)
#define _VT_IOWRAP_FUNCTYPE(f) f ## _FUNCTYPE
/* need double macro for stringify to evaluate macro arguments before stringifying */
#define stringify(x) _stringify(x)
#define _stringify(x) #x

/** Setup libc pointers
 * ... to be used in global initialization before everything else
 * iolib_handle MUST be initialized already!
 */
#define VT_IOWRAP_INIT_FUNC(FUNC_NAME) \
{ \
	if (!iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p) { \
		vt_debug_msg(DBG_INIT, "init_func: dlsym(" stringify(FUNC_NAME) ") --> "); \
		(void)dlerror(); \
		iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p = \
			dlsym( iolib_handle, stringify(FUNC_NAME) ); \
		vt_debug_msg(DBG_INIT, "%p", iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p); \
		if (!iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p) \
			symload_fail( stringify(FUNC_NAME), dlerror() ); \
	} \
	else { \
		vt_debug_msg(DBG_INIT, "init_func: " stringify(FUNC_NAME) " was already looked up: %p", \
			iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p); \
	} \
}

/** Setup VT region and tracing specific settings
 * ... to be used in global initialization after RFG initialization
 */
#define VT_IOWRAP_REG_FUNC(FUNC_NAME) \
{ \
	vt_debug_msg(DBG_INIT, "reg_func: vt_def_region(" stringify(FUNC_NAME) ")"); \
	iofunctions[FUNC_IDX(FUNC_NAME)].vt_func_id = \
		vt_def_region(	stringify(FUNC_NAME), \
				vt_fid, \
				VT_NO_LNO, \
				VT_NO_LNO, \
        NULL, \
				VT_LIBC_IO ); \
	iofunctions[FUNC_IDX(FUNC_NAME)].traceme = 1; \
}


/** Call the function FUNC_NAME from the I/O library (usually libc)
 */
#if 0
#define VT_IOWRAP_CALL_LIBFUNC(FUNC_NAME, ...) \
( VT_IOWRAP_FUNCDEF(FUNC_NAME) \
	(iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.f) ) \
	(__VA_ARGS__) 
#endif
#define VT_IOWRAP_CALL_LIBFUNC1(FUNC_NAME, ARG1) \
( VT_IOWRAP_FUNCDEF(FUNC_NAME) \
	(iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.f) ) \
	(ARG1)
#define VT_IOWRAP_CALL_LIBFUNC2(FUNC_NAME, ARG1, ARG2) \
( VT_IOWRAP_FUNCDEF(FUNC_NAME) \
	(iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.f) ) \
	(ARG1, ARG2)
#define VT_IOWRAP_CALL_LIBFUNC3(FUNC_NAME, ARG1, ARG2, ARG3) \
( VT_IOWRAP_FUNCDEF(FUNC_NAME) \
	(iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.f) ) \
	(ARG1, ARG2, ARG3)
#define VT_IOWRAP_CALL_LIBFUNC4(FUNC_NAME, ARG1, ARG2, ARG3, ARG4) \
( VT_IOWRAP_FUNCDEF(FUNC_NAME) \
	(iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.f) ) \
	(ARG1, ARG2, ARG3, ARG4)


/** Resolve function address from the I/O library (usually libc)
 */
#define VT_IOWRAP_INIT_IOFUNC() \
        uint64_t handleid = 0; \
        ssize_t num_bytes=0; \
        uint8_t was_recorded; \
	uint8_t enable_memhooks=0; \
{ \
	VT_CHECK_THREAD; \
	if( VT_MEMHOOKS_ENABLED() ) \
		{ VT_MEMHOOKS_OFF(); enable_memhooks = 1; } \
	if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) { \
		get_iolib_handle(); \
		(void)dlerror(); \
		iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p = \
			dlsym( iolib_handle, stringify(VT_IOWRAP_THISFUNCNAME) ); \
		if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) \
			symload_fail( stringify(VT_IOWRAP_THISFUNCNAME), dlerror() ); \
	        vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_INIT_IOFUNC(): " stringify(VT_IOWRAP_THISFUNCNAME) " --> %p", iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p); \
	} \
}

/** Resolve function address from the I/O library (usually libc)
 */
#define VT_IOWRAP_INIT_IOFUNC_OPEN() \
        uint64_t handleid = 0; \
        uint8_t was_recorded; \
	uint8_t enable_memhooks=0; \
{ \
	VT_CHECK_THREAD; \
	if( VT_MEMHOOKS_ENABLED() ) \
		{ VT_MEMHOOKS_OFF(); enable_memhooks = 1; } \
	if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) { \
                get_iolib_handle(); \
                (void)dlerror(); \
		iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p = \
			dlsym( iolib_handle, stringify(VT_IOWRAP_THISFUNCNAME) ); \
		if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) \
			symload_fail( stringify(VT_IOWRAP_THISFUNCNAME), dlerror() ); \
                vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_INIT_IOFUNC_OPEN(): " stringify(VT_IOWRAP_THISFUNCNAME) " --> %p", iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p); \
	} \
}

/**
 * Check if tracing is enabled and return immediately if not
 */
#if 0
#define VT_IOWRAP_CHECK_TRACING(...) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC(VT_IOWRAP_THISFUNCNAME, __VA_ARGS__); \
}
#endif
#define VT_IOWRAP_CHECK_TRACING1(ARG1) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC1(VT_IOWRAP_THISFUNCNAME, ARG1); \
}
#define VT_IOWRAP_CHECK_TRACING2(ARG1, ARG2) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC2(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2); \
}
#define VT_IOWRAP_CHECK_TRACING3(ARG1, ARG2, ARG3) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC3(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3); \
}
#define VT_IOWRAP_CHECK_TRACING4(ARG1, ARG2, ARG3, ARG4) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC4(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3, ARG4); \
}

#if 0
#define VT_IOWRAP_CHECK_TRACING_VOID(...) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC(VT_IOWRAP_THISFUNCNAME, __VA_ARGS__); \
                return; \
        } \
}
#endif
#define VT_IOWRAP_CHECK_TRACING_VOID1(ARG1) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC1(VT_IOWRAP_THISFUNCNAME, ARG1); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID2(ARG1, ARG2) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC2(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID3(ARG1, ARG2, ARG3) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC3(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID4(ARG1, ARG2, ARG3, ARG4) \
{ \
	vt_debug_msg( DBG_TRACECHK, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC4(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3, ARG4); \
                return; \
        } \
}


/** Write enter record and register/unregister file, if necessary
 */
#define VT_IOWRAP_ENTER_IOFUNC() \
{ \
	enter_time = vt_pform_wtime(); \
	vt_debug_msg(DBG_VT_CALL, "vt_enter(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp %llu", (unsigned long long)enter_time); \
	was_recorded = vt_enter( &enter_time, iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].vt_func_id ); \
	if( was_recorded ) { \
                handleid = VTTHRD_IO_NEXT_HANDLEID(VTTHRD_MY_VTTHRD); \
                vt_iobegin( &enter_time, handleid ); \
	} \
}

/** Write leave record and counter, if appropriate
 *  The argument is a failure condition and decides whether VT_IOFLAG_FAILED is
 *  added in vt_ioend
 */
#define VT_IOWRAP_LEAVE_IOFUNC(ERROR_CONDITION,FD) \
{ \
	uint64_t time = vt_pform_wtime(); \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
	if( was_recorded ) { \
                uint32_t ioop = VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                uint32_t fid; \
                if( FD == -1 ) { \
                        fid = invalid_fd_fid; \
                } \
                else { \
                        vampir_file_t* file; \
                	file = get_vampir_file( FD ); \
                	fid = file->vampir_file_id; \
                } \
                if( fid ) { \
                        if( ERROR_CONDITION ) { \
                                vt_debug_msg(DBG_VT_CALL, "vt_ioend(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp %llu", (unsigned long long)time); \
                                vt_ioend( &time, fid, handleid, ioop | VT_IOFLAG_IOFAILED, (uint64_t)num_bytes ); \
                        } \
                        else { \
                                vt_ioend( &time, fid, handleid, ioop, (uint64_t)num_bytes ); \
                        } \
                } \
        } \
        vt_exit( &time ); \
	if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}

#define VT_IOWRAP_LEAVE_IOFUNC_OPEN(ERROR_CONDITION,FD) \
{ \
        uint64_t time = vt_pform_wtime(); \
        vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC_OPEN(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( was_recorded ) { \
                uint32_t ioop = VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                uint32_t fid; \
                if( ERROR_CONDITION ) { \
                        if( path && strlen(path) > 0 ) { \
                                fid = vt_iofile_id(path); \
                        } \
                        else { \
                                fid = invalid_fd_fid; \
                        } \
                        vt_debug_msg(DBG_VT_CALL, "vt_ioend(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp %llu", (unsigned long long)time); \
                        vt_ioend( &time, fid, handleid, ioop | VT_IOFLAG_IOFAILED, 0 ); \
                } \
                else { \
                        vampir_file_t* file; \
                        vt_iofile_open( path, FD ); \
                        file = get_vampir_file( FD ); \
                        fid = file->vampir_file_id; \
                        vt_ioend( &time, fid, handleid, ioop, 0 ); \
                } \
        } \
        vt_exit( &time ); \
        if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}

#define VT_IOWRAP_LEAVE_IOFUNC_DUP(ERROR_CONDITION,OLDFD,NEWFD) \
{ \
        uint64_t time = vt_pform_wtime(); \
        vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC_DUP(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( was_recorded ) { \
                uint32_t ioop = VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                vampir_file_t* file; \
                uint32_t fid; \
                file = get_vampir_file( OLDFD ); \
                vt_iofile_dupfd( OLDFD, NEWFD ); \
                fid = file->vampir_file_id; \
                if( fid ) { \
                        if( ERROR_CONDITION ) { \
                                vt_debug_msg(DBG_VT_CALL, "vt_ioend(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp %llu", (unsigned long long)time); \
                                vt_ioend( &time, fid, handleid, ioop | VT_IOFLAG_IOFAILED, (uint64_t)num_bytes ); \
                        } \
                        else { \
                                vt_ioend( &time, fid, handleid, ioop, (uint64_t)num_bytes ); \
                        } \
                } \
        } \
        vt_exit( &time ); \
        if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}

/* Used for I/O functions that work on paths instead of using fd's, e.g. unlink.
 * No handle id used here.
 */
#define VT_IOWRAP_LEAVE_IOFUNC_PATH(ERROR_CONDITION,PATH) \
{ \
        uint64_t time = vt_pform_wtime(); \
        vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC_PATH(), Function " stringify(VT_IOWRAP_THISFUNCNAME) ); \
        if( was_recorded ) { \
                uint32_t ioop = VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                uint32_t fid; \
                if( ERROR_CONDITION ) { \
                        if( PATH && strlen(PATH) > 0 ) { \
                                fid = vt_iofile_id(PATH); \
                        } \
                        else { \
                                fid = invalid_fd_fid; \
                        } \
                        ioop |= VT_IOFLAG_IOFAILED; \
                } \
                else { \
                        fid = vt_iofile_id(PATH); \
                } \
                vt_debug_msg(DBG_VT_CALL, "vt_ioend(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp %llu", (unsigned long long)time); \
                vt_ioend( &time, fid, handleid, ioop, 0 ); \
        } \
        vt_exit( &time ); \
        if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}

#else /* VT_IOWRAP */

#define VT_ENABLE_IO_TRACING()
#define VT_DISABLE_IO_TRACING()
#define VT_SUSPEND_IO_TRACING()
#define VT_RESUME_IO_TRACING()

#define libc_fprintf fprintf
#define vt_iowrap_externals_init()

#endif

#endif /* _VT_IOWRAP_H_ */

