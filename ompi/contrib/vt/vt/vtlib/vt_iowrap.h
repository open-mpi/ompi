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

#ifndef _VT_IOWRAP_H_
#define _VT_IOWRAP_H_

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

#include "config.h"

#if (defined(VT_IOWRAP))
#include "otf.h"
#include "vt_error.h"
# define VT_ENABLE_IO_TRACING() vt_io_tracing_enabled = 1;
# define VT_DISABLE_IO_TRACING() vt_io_tracing_enabled = 0;
# define VT_SUSPEND_IO_TRACING() \
    vt_io_tracing_state = vt_io_tracing_enabled; \
    vt_io_tracing_enabled = 0;
# define VT_RESUME_IO_TRACING() \
    vt_io_tracing_enabled = vt_io_tracing_state;

#if SIZEOF_LONG == 8
# define U64_STRARG "%lu"
# define S64_STRARG "%li"
#else /* SIZEOF_LONG */
# define U64_STRARG "%llu"
# define S64_STRARG "%lli"
#endif /* SIZEOF_LONG */

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


struct iofunctions iofunctions[NUMFUNCTIONS];

/* io wrapper initialization/finalization */
EXTERN int vt_iowrap_init(void);
EXTERN int vt_iowrap_finalize(void);

EXTERN int vt_io_tracing_enabled;
EXTERN int vt_io_tracing_state;

#ifndef OTF_FILEOP_DUP
#define OTF_FILEOP_DUP 10
#endif

#define open_FUNCDEF		(int (*)(const char *, int, mode_t))
#define open_FUNCTYPE		OTF_FILEOP_OPEN
#define open64_FUNCDEF		(int (*)(const char *, int, mode_t))
#define open64_FUNCTYPE		OTF_FILEOP_OPEN
#define creat_FUNCDEF		(int (*)(const char *, mode_t))
#define creat_FUNCTYPE		OTF_FILEOP_OPEN
#define creat64_FUNCDEF		(int (*)(const char *, mode_t))
#define creat64_FUNCTYPE	OTF_FILEOP_OPEN
#define dup_FUNCDEF		(int (*)(int))
#define dup_FUNCTYPE		OTF_FILEOP_DUP
#define dup2_FUNCDEF		(int (*)(int, int))
#define dup2_FUNCTYPE		OTF_FILEOP_DUP
#define close_FUNCDEF		(int (*)(int))
#define close_FUNCTYPE		OTF_FILEOP_CLOSE
#define lseek_FUNCDEF		(off_t (*)(int, off_t, int))
#define lseek_FUNCTYPE		OTF_FILEOP_SEEK
#define lseek64_FUNCDEF		(off64_t (*)(int, off64_t, int))
#define lseek64_FUNCTYPE	OTF_FILEOP_SEEK
#define read_FUNCDEF		(ssize_t (*)(int, void *, size_t))
#define read_FUNCTYPE		OTF_FILEOP_READ
#define write_FUNCDEF		(ssize_t (*)(int, const void *, size_t))
#define write_FUNCTYPE		OTF_FILEOP_WRITE
#define readv_FUNCDEF		(int (*)(int, const struct iovec *, size_t))
#define readv_FUNCTYPE		OTF_FILEOP_READ
#define writev_FUNCDEF		(int (*)(int, const struct iovec *, size_t))
#define writev_FUNCTYPE		OTF_FILEOP_WRITE
#define pread_FUNCDEF		(ssize_t (*)(int, void *, size_t, off_t))
#define pread_FUNCTYPE		OTF_FILEOP_READ
#define pwrite_FUNCDEF		(ssize_t (*)(int, const void *, size_t, off_t))
#define pwrite_FUNCTYPE		OTF_FILEOP_WRITE
#define pread64_FUNCDEF		(ssize_t (*)(int, void *, size_t, off64_t))
#define pread64_FUNCTYPE	OTF_FILEOP_READ
#define pwrite64_FUNCDEF	(ssize_t (*)(int, const void *, size_t, off64_t))
#define pwrite64_FUNCTYPE	OTF_FILEOP_WRITE
#define fdopen_FUNCDEF		(FILE *(*)(int, const char *))
#define fdopen_FUNCTYPE		OTF_FILEOP_OPEN
#define fopen_FUNCDEF		(FILE *(*)(const char *, const char *))
#define fopen_FUNCTYPE		OTF_FILEOP_OPEN
#define fopen64_FUNCDEF		(FILE *(*)(const char *, const char *))
#define fopen64_FUNCTYPE	OTF_FILEOP_OPEN
#define fclose_FUNCDEF		(int (*)(FILE *))
#define fclose_FUNCTYPE		OTF_FILEOP_CLOSE
#define fseek_FUNCDEF		(int (*)(FILE *, long, int))
#define fseek_FUNCTYPE		OTF_FILEOP_SEEK
#define fseeko_FUNCDEF		(int (*)(FILE *, off_t, int))
#define fseeko_FUNCTYPE		OTF_FILEOP_SEEK
#define fseeko64_FUNCDEF	(int (*)(FILE *, off64_t, int))
#define fseeko64_FUNCTYPE	OTF_FILEOP_SEEK
#define rewind_FUNCDEF		(void (*)(FILE *))
#define rewind_FUNCTYPE		OTF_FILEOP_SEEK
#define fsetpos_FUNCDEF		(int (*)(FILE *, const fpos_t *))
#define fsetpos_FUNCTYPE	OTF_FILEOP_SEEK
#define fsetpos64_FUNCDEF	(int (*)(FILE *, const fpos64_t *))
#define fsetpos64_FUNCTYPE	OTF_FILEOP_SEEK
#define fread_FUNCDEF		(size_t (*)(void *, size_t, size_t, FILE *))
#define fread_FUNCTYPE		OTF_FILEOP_READ
#define fwrite_FUNCDEF		(size_t (*)(const void *, size_t, size_t, FILE *))
#define fwrite_FUNCTYPE		OTF_FILEOP_WRITE
#define fgetc_FUNCDEF		(int (*)(FILE *))
#define fgetc_FUNCTYPE		OTF_FILEOP_READ
#define getc_FUNCDEF		(int (*)(FILE *))
#define getc_FUNCTYPE		OTF_FILEOP_READ
#define fgets_FUNCDEF		(char *(*)(char *, int, FILE *))
#define fgets_FUNCTYPE		OTF_FILEOP_READ
#define gets_FUNCDEF		(char *(*)(char *))
#define gets_FUNCTYPE		OTF_FILEOP_READ
#define fputc_FUNCDEF		(int (*)(int, FILE *))
#define fputc_FUNCTYPE		OTF_FILEOP_WRITE
#define putc_FUNCDEF		(int (*)(int, FILE *))
#define putc_FUNCTYPE		OTF_FILEOP_WRITE
#define fputs_FUNCDEF		(int (*)(const char *, FILE *))
#define fputs_FUNCTYPE		OTF_FILEOP_WRITE
#define puts_FUNCDEF 		(int (*)(const char *))
#define puts_FUNCTYPE		OTF_FILEOP_WRITE
/* #define ungetc_FUNCDEF	(int (*)(int, FILE *)) */
#define fscanf_FUNCDEF		(int (*)(FILE *, const char *, ...))
#define fscanf_FUNCTYPE		OTF_FILEOP_READ
#define fprintf_FUNCDEF		(int (*)(FILE *, const char *, ...))
#define fprintf_FUNCTYPE	OTF_FILEOP_WRITE
/* #define vfscanf_FUNCDEF	(int (*)(FILE *, const char *, va_list)) */
/* #define vfprintf_FUNCDEF	(int (*)(FILE *, const char *, va_list)) */


/* #define IOWRAP_REGION_DESCR_LEN	256 */
#define DBG_INIT	1
#define DBG_IO		2
#define DBG_VT_CALL	3


#define FUNC_IDX(f) _FUNC_IDX(f)
#define _FUNC_IDX(f) f ## _IDX
#define VT_IOWRAP_FUNCDEF(f) f ## _FUNCDEF
#define VT_IOWRAP_FUNCTYPE(f) _VT_IOWRAP_FUNCTYPE(f)
#define _VT_IOWRAP_FUNCTYPE(f) f ## _FUNCTYPE
/* need double macro for stringify to evaluate macro arguments before stringifying */
#define stringify(x) _stringify(x)
#define _stringify(x) #x

/** 
 * Boolean macro for checking if we shall trace right now 
 * Tracing can be disabled globally via vt_io_tracing_enabled = 0 or
 * for each function separately via iofunctions[IDX].traceme = 0
 */
#define DO_TRACE() \
	( vt_is_alive && \
	  vt_io_tracing_enabled && \
	  iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].traceme )

/** Set up VT region and tracing specific settings
 * ... to be used in global initialization
 */
#define VT_IOWRAP_INIT_FUNC(FUNC_NAME) \
{ \
	vt_debug_msg(DBG_INIT, "init_func: vt_def_region(" stringify(FUNC_NAME) ")\n"); \
	iofunctions[FUNC_IDX(FUNC_NAME)].vt_func_id = \
		vt_def_region(	stringify(FUNC_NAME), \
				vt_fid, \
				VT_NO_LNO, \
				VT_NO_LNO, \
				"I/O", \
				VT_FUNCTION ); \
	iofunctions[FUNC_IDX(FUNC_NAME)].traceme = 1; \
	if (!iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p) { \
		vt_debug_msg(DBG_INIT, "init_func: dlsym(" stringify(FUNC_NAME) ") --> "); \
		iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p = \
			dlsym( RTLD_NEXT, stringify(FUNC_NAME) ); \
		vt_debug_msg(DBG_INIT, "%p\n", iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p); \
		if (!iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p) \
			symload_fail( stringify(FUNC_NAME) ); \
	} \
	else { \
		vt_debug_msg(DBG_INIT, "init_func: " stringify(FUNC_NAME) " was already looked up: %p\n", \
			iofunctions[FUNC_IDX(FUNC_NAME)].lib_func.p); \
	} \
}


/** Call the function FUNC_NAME from the next library
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


/** Resolve function address from next library
 */
#define VT_IOWRAP_INIT_IOFUNC() \
        ssize_t num_bytes=0; \
	uint8_t enable_memhooks=0; \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_INIT_IOFUNC(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( VT_MEMHOOKS_ENABLED() ) \
		{ VT_MEMHOOKS_OFF(); enable_memhooks = 1; } \
	if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) { \
		vt_debug_msg(DBG_INIT, stringify(VT_IOWRAP_THISFUNCNAME) ": dlsym(" stringify(VT_IOWRAP_THISFUNCNAME) ") --> "); \
		iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p = \
			dlsym( RTLD_NEXT, stringify(VT_IOWRAP_THISFUNCNAME) ); \
		vt_debug_msg(DBG_INIT, "%p\n", iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p); \
		if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) \
			symload_fail( stringify(VT_IOWRAP_THISFUNCNAME) ); \
	} \
}

/** Resolve function address from next library
 */
#define VT_IOWRAP_INIT_IOFUNC_OPEN() \
	uint8_t enable_memhooks=0; \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_INIT_IOFUNC_OPEN(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( VT_MEMHOOKS_ENABLED() ) \
		{ VT_MEMHOOKS_OFF(); enable_memhooks = 1; } \
	if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) { \
		vt_debug_msg(DBG_INIT, stringify(VT_IOWRAP_THISFUNCNAME) ": dlsym(" stringify(VT_IOWRAP_THISFUNCNAME) ") --> "); \
		iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p = \
			dlsym( RTLD_NEXT, stringify(VT_IOWRAP_THISFUNCNAME) ); \
		vt_debug_msg(DBG_INIT, "%p\n", iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p); \
		if (!iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].lib_func.p) \
			symload_fail( stringify(VT_IOWRAP_THISFUNCNAME) ); \
	} \
}

/**
 * Check if tracing is enabled and return immediately if not
 */
#if 0
#define VT_IOWRAP_CHECK_TRACING(...) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC(VT_IOWRAP_THISFUNCNAME, __VA_ARGS__); \
}
#endif
#define VT_IOWRAP_CHECK_TRACING1(ARG1) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC1(VT_IOWRAP_THISFUNCNAME, ARG1); \
}
#define VT_IOWRAP_CHECK_TRACING2(ARG1, ARG2) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC2(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2); \
}
#define VT_IOWRAP_CHECK_TRACING3(ARG1, ARG2, ARG3) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC3(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3); \
}
#define VT_IOWRAP_CHECK_TRACING4(ARG1, ARG2, ARG3, ARG4) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
	if( !DO_TRACE() ) \
		return VT_IOWRAP_CALL_LIBFUNC4(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3, ARG4); \
}

#if 0
#define VT_IOWRAP_CHECK_TRACING_VOID(...) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC(VT_IOWRAP_THISFUNCNAME, __VA_ARGS__); \
                return; \
        } \
}
#endif
#define VT_IOWRAP_CHECK_TRACING_VOID1(ARG1) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC1(VT_IOWRAP_THISFUNCNAME, ARG1); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID2(ARG1, ARG2) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC2(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID3(ARG1, ARG2, ARG3) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( !DO_TRACE() ) { \
                VT_IOWRAP_CALL_LIBFUNC3(VT_IOWRAP_THISFUNCNAME, ARG1, ARG2, ARG3); \
                return; \
        } \
}
#define VT_IOWRAP_CHECK_TRACING_VOID4(ARG1, ARG2, ARG3, ARG4) \
{ \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_CHECK_TRACING_VOID(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
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
	vt_debug_msg(DBG_VT_CALL, "vt_enter(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp " U64_STRARG "\n", enter_time); \
	vt_enter( &enter_time, iofunctions[FUNC_IDX(VT_IOWRAP_THISFUNCNAME)].vt_func_id ); \
}

/** Write leave record and counter, if appropriate
 *  The argument is a failure condition and decides, wheter we 
 *  use vt_exit or vt_ioexit
 */
#define VT_IOWRAP_LEAVE_IOFUNC(ERROR_CONDITION,FD) \
{ \
	uint64_t time = vt_pform_wtime(); \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( ERROR_CONDITION ) \
        { \
        	vt_debug_msg(DBG_VT_CALL, "vt_exit(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp " U64_STRARG "\n", time); \
	        vt_exit( &time ); \
        } \
        else \
        { \
                uint32_t func_id=VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                vampir_file_t* file; \
                file = get_vampir_file( FD ); \
                if( func_id==OTF_FILEOP_DUP ) \
                        func_id=OTF_FILEOP_OPEN; \
                if( file->vampir_file_id==0 ) \
                        vt_exit( &time );\
                else \
                        vt_ioexit( &enter_time, &time, file->vampir_file_id,  \
                                    file->handle_id, func_id, (uint64_t)num_bytes ); \
                vt_debug_msg(DBG_VT_CALL, "vt_exit(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp " U64_STRARG "\n", time); \
        } \
	if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}

#define VT_IOWRAP_LEAVE_IOFUNC_OPEN(ERROR_CONDITION,FD) \
{ \
	uint64_t time = vt_pform_wtime(); \
	vt_debug_msg( DBG_INIT, "Macro VT_IOWRAP_LEAVE_IOFUNC_OPEN(), Function " stringify(VT_IOWRAP_THISFUNCNAME) "\n"); \
        if( ERROR_CONDITION ) \
        { \
        	vt_debug_msg(DBG_VT_CALL, "vt_exit(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp " U64_STRARG "\n", time); \
	        vt_exit( &time ); \
        } \
        else \
        { \
                uint32_t func_id=VT_IOWRAP_FUNCTYPE(VT_IOWRAP_THISFUNCNAME); \
                vampir_file_t* file; \
                vt_iofile_open( path, FD ); \
                file = get_vampir_file( FD ); \
                if( func_id==OTF_FILEOP_DUP ) \
                        func_id=OTF_FILEOP_OPEN; \
                if( file->vampir_file_id==0 ) \
                        vt_exit( &time );\
                else \
                        vt_ioexit( &enter_time, &time, file->vampir_file_id,  \
                                    file->handle_id, func_id, 0 ); \
                vt_debug_msg(DBG_VT_CALL, "vt_exit(" stringify(VT_IOWRAP_THISFUNCNAME) "), stamp " U64_STRARG "\n", time); \
        } \
	if( enable_memhooks ) VT_MEMHOOKS_ON(); \
}
#else /* VT_IOWRAP */
# define VT_ENABLE_IO_TRACING()
# define VT_DISABLE_IO_TRACING()
# define VT_SUSPEND_IO_TRACING()
# define VT_RESUME_IO_TRACING()
#endif

#endif /* _VT_IOWRAP_H_ */

