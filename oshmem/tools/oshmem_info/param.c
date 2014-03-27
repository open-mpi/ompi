/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "mpi.h"
#include "shmem.h"

#include <string.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include MCA_timer_IMPLEMENTATION_HEADER
#include "opal/class/opal_value_array.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/printf.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal_info_support.h"

#if OMPI_RTE_ORTE
#include "orte/util/show_help.h"
#endif

#include "ompi/tools/ompi_info/ompi_info.h"
#include "ompi/include/mpi_portable_platform.h"

#include "oshmem/tools/oshmem_info/oshmem_info.h"


const char *ompi_info_deprecated_value = "deprecated-ompi-info-value";

static void append(char *dest, size_t max, int *first, char *src)
{
    size_t len;

    if (NULL == src) {
        return;
    }

    len = max - strlen(dest);
    if (!(*first)) {
        strncat(dest, ", ", len);
        len = max - strlen(dest);
    }
    strncat(dest, src, len);
    *first = 0;
}


/*
 * do_config
 * Accepts:
 *  - want_all: boolean flag; TRUE -> display all options
 *                FALSE -> display selected options
 *
 * This function displays all the options with which the current
 * installation of ompi was configured. There are many options here
 * that are carried forward from OMPI-7 and are not mca parameters
 * in OMPI-10. I have to dig through the invalid options and replace
 * them with OMPI-10 options.
 */
void oshmem_info_do_config(bool want_all)
{
    char *cxx;
    char *fortran_mpifh;
    char *fortran_usempi;
    char *fortran_usempif08;
    char *fortran_usempif08_compliance;
    char *fortran_have_ignore_tkr;
    char *fortran_have_f08_assumed_rank;
    char *fortran_build_f08_subarrays;
    char *fortran_have_optional_args;
    char *fortran_have_bind_c;
    char *fortran_have_private;
    char *fortran_have_abstract;
    char *fortran_have_asynchronous;
    char *fortran_have_procedure;
    char *fortran_08_using_wrappers_for_choice_buffer_functions;
    char *java;
    char *heterogeneous;
    char *memprofile;
    char *memdebug;
    char *debug;
    char *mpi_interface_warning;
    char *cprofiling;
    char *cxxprofiling;
    char *fortran_mpifh_profiling;
    char *fortran_usempi_profiling;
    char *fortran_usempif08_profiling;
    char *cxxexceptions;
    char *threads;
    char *want_libltdl;
#if OMPI_RTE_ORTE
    char *mpirun_prefix_by_default;
#endif
    char *sparse_groups;
    char *have_mpi_io;
    char *wtime_support;
    char *symbol_visibility;
    char *ft_support;
    char *crdebug_support;
    char *topology_support;
    char *vt_support;
    
    /* Do a little preprocessor trickery here to figure opal_info_out the
     * tri-state of MPI_PARAM_CHECK (which will be either 0, 1, or
     * ompi_mpi_param_check).  The preprocessor will only allow
     * comparisons against constants, so you'll get a warning if you
     * check MPI_PARAM_CHECK against 0 or 1, but its real value is the
     * char *ompi_mpi_param_check.  So define ompi_mpi_param_check to
     * be a constant, and then all the preprocessor comparisons work
     * opal_info_out ok.  Note that we chose the preprocessor
     * comparison ropal_info_oute because it is not sufficient to
     * simply set the variable ompi_mpi_param_check to a non-0/non-1
     * value.  This is because the compiler will generate a warning
     * that that C variable is unused when MPI_PARAM_CHECK is
     * hard-coded to 0 or 1.
     */
    char *paramcheck;
#define ompi_mpi_param_check 999
#if 0 == MPI_PARAM_CHECK
    paramcheck = "never";
#elif 1 == MPI_PARAM_CHECK
    paramcheck = "always";
#else
    paramcheck = "runtime";
#endif

    /* setup the strings that don't require allocations*/
    cxx = OMPI_BUILD_CXX_BINDINGS ? "yes" : "no";
    if (OMPI_BUILD_FORTRAN_USEMPI_BINDINGS) {
        if (OMPI_FORTRAN_HAVE_IGNORE_TKR) {
            fortran_usempi = "yes (full: ignore TKR)";
        } else {
            fortran_usempi = "yes (limited: overloading)";
        }
    } else {
        fortran_usempi = "no";
    }
    fortran_usempif08 = OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS ? "yes" : "no";
    fortran_have_f08_assumed_rank = OMPI_FORTRAN_HAVE_F08_ASSUMED_RANK ?
        "yes" : "no";
    fortran_build_f08_subarrays = OMPI_BUILD_FORTRAN_F08_SUBARRAYS ?
        "yes" : "no";
    fortran_have_optional_args = OMPI_FORTRAN_HAVE_OPTIONAL_ARGS ?
        "yes" : "no";
    fortran_have_bind_c = OMPI_FORTRAN_HAVE_BIND_C ? "yes" : "no";
    fortran_have_private = OMPI_FORTRAN_HAVE_PRIVATE ? "yes" : "no";
    fortran_have_abstract = OMPI_FORTRAN_HAVE_ABSTRACT ? "yes" : "no";
    fortran_have_asynchronous = OMPI_FORTRAN_HAVE_ASYNCHRONOUS ? "yes" : "no";
    fortran_have_procedure = OMPI_FORTRAN_HAVE_PROCEDURE ? "yes" : "no";
    fortran_08_using_wrappers_for_choice_buffer_functions =
        OMPI_FORTRAN_NEED_WRAPPER_ROUTINES ? "yes" : "no";

    /* Build a string describing what level of compliance the mpi_f08
       module has */
    if (OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS) {

        /* Do we have everything? */
        if (OMPI_BUILD_FORTRAN_F08_SUBARRAYS &&
            OMPI_FORTRAN_HAVE_PRIVATE &&
            OMPI_FORTRAN_HAVE_ABSTRACT &&
            OMPI_FORTRAN_HAVE_ASYNCHRONOUS &&
            OMPI_FORTRAN_HAVE_PROCEDURE &&
            OMPI_FORTRAN_NEED_WRAPPER_ROUTINES) {
            fortran_usempif08_compliance = strdup("The mpi_f08 module is available, and is fully compliant.  w00t!");
        } else {
            char f08[1024];
            int first = 1;
            snprintf(f08, sizeof(f08),
                     "The mpi_f08 module is available, but due to limitations in the %s compiler, does not support the following: ",
                     OMPI_FC);
            if (!OMPI_BUILD_FORTRAN_F08_SUBARRAYS) {
                append(f08, sizeof(f08), &first, "array subsections");
            }
            if (!OMPI_FORTRAN_HAVE_PRIVATE) {
                append(f08, sizeof(f08), &first, "private MPI_Status members");
            }
            if (!OMPI_FORTRAN_HAVE_ABSTRACT) {
                append(f08, sizeof(f08), &first, "ABSTRACT INTERFACE function pointers");
            }
            if (!OMPI_FORTRAN_HAVE_ASYNCHRONOUS) {
                append(f08, sizeof(f08), &first, "Fortran '08-specified ASYNCHRONOUS behavior");
            }
            if (!OMPI_FORTRAN_HAVE_PROCEDURE) {
                append(f08, sizeof(f08), &first, "PROCEDUREs");
            }
            if (OMPI_FORTRAN_NEED_WRAPPER_ROUTINES) {
                append(f08, sizeof(f08), &first, "direct passthru (where possible) to underlying Open MPI's C functionality");
            }
            fortran_usempif08_compliance = strdup(f08);
        }
    } else {
        fortran_usempif08_compliance = strdup("The mpi_f08 module was not built");
    }

    java = OMPI_WANT_JAVA_BINDINGS ? "yes" : "no";
    heterogeneous = OPAL_ENABLE_HETEROGENEOUS_SUPPORT ? "yes" : "no";
    memprofile = OPAL_ENABLE_MEM_PROFILE ? "yes" : "no";
    memdebug = OPAL_ENABLE_MEM_DEBUG ? "yes" : "no";
    debug = OPAL_ENABLE_DEBUG ? "yes" : "no";
    mpi_interface_warning = OMPI_WANT_MPI_INTERFACE_WARNING ? "yes" : "no";
    cprofiling = OMPI_ENABLE_MPI_PROFILING ? "yes" : "no";
    cxxprofiling = (OMPI_BUILD_CXX_BINDINGS && OMPI_ENABLE_MPI_PROFILING) ? "yes" : "no";
    cxxexceptions = (OMPI_BUILD_CXX_BINDINGS && OMPI_HAVE_CXX_EXCEPTION_SUPPORT) ? "yes" : "no";
    fortran_mpifh_profiling = (OMPI_ENABLE_MPI_PROFILING && OMPI_BUILD_FORTRAN_MPIFH_BINDINGS) ? "yes" : "no";
    fortran_usempi_profiling = (OMPI_ENABLE_MPI_PROFILING && OMPI_BUILD_FORTRAN_USEMPI_BINDINGS) ? "yes" : "no";
    fortran_usempif08_profiling = (OMPI_ENABLE_MPI_PROFILING && OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS) ? "yes" : "no";
    want_libltdl = OPAL_WANT_LIBLTDL ? "yes" : "no";
#if OMPI_RTE_ORTE
    mpirun_prefix_by_default = ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT ? "yes" : "no";
#endif
    sparse_groups = OMPI_GROUP_SPARSE ? "yes" : "no";
    have_mpi_io = OMPI_PROVIDE_MPI_FILE_INTERFACE ? "yes" : "no";
    wtime_support = OPAL_TIMER_USEC_NATIVE ? "native" : "gettimeofday";
    symbol_visibility = OPAL_C_HAVE_VISIBILITY ? "yes" : "no";
    topology_support = OPAL_HAVE_HWLOC ? "yes" : "no";
    vt_support = OMPI_ENABLE_CONTRIB_vt ? "yes" : "no";

    /* setup strings that require allocation */
    if (OMPI_BUILD_FORTRAN_MPIFH_BINDINGS) {
        (void)asprintf(&fortran_mpifh, "yes (%s)",
                       (OPAL_HAVE_WEAK_SYMBOLS ? "all" :
                        (OMPI_FORTRAN_CAPS ? "caps" :
                         (OMPI_FORTRAN_PLAIN ? "lower case" :
                          (OMPI_FORTRAN_SINGLE_UNDERSCORE ? "single underscore" : "double underscore")))));
    } else {
        fortran_mpifh = strdup("no");
    }

    if (OMPI_FORTRAN_HAVE_IGNORE_TKR) {
        /* OMPI_FORTRAN_IGNORE_TKR_PREDECL is already in quotes; it
           didn't work consistently to put it in _STRINGIFY because
           sometimes the compiler would actually interpret the pragma
           in there before stringify-ing it. */
        (void)asprintf(&fortran_have_ignore_tkr, "yes (%s)",
                       OMPI_FORTRAN_IGNORE_TKR_PREDECL);
    } else {
        fortran_have_ignore_tkr = strdup("no");
    }

    if (OPAL_HAVE_POSIX_THREADS) {        /* should just test OPAL_HAVE_THREADS */
#if OMPI_RTE_ORTE
        (void)asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: %s, OPAL support: %s, OMPI progress: %s, ORTE progress: yes, Event lib: yes)",
                       (OPAL_HAVE_POSIX_THREADS ? "posix" : "type unknown"), /* "type unknown" can presumably never happen */
                       OMPI_ENABLE_THREAD_MULTIPLE ? "yes" : "no",
                       OPAL_ENABLE_MULTI_THREADS ? "yes" : "no",
                       OMPI_ENABLE_PROGRESS_THREADS ? "yes" : "no");
#else
        (void)asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: %s, OPAL support: %s, OMPI progress: %s, Event lib: yes)",
                       (OPAL_HAVE_POSIX_THREADS ? "posix" : "type unknown"), /* "type unknown" can presumably never happen */
                       OMPI_ENABLE_THREAD_MULTIPLE ? "yes" : "no",
                       OPAL_ENABLE_MULTI_THREADS ? "yes" : "no",
                       OMPI_ENABLE_PROGRESS_THREADS ? "yes" : "no");
#endif
    } else {
        threads = strdup("no");
    }

    (void)asprintf(&ft_support, "%s (checkpoint thread: %s)",
                   OPAL_ENABLE_FT ? "yes" : "no", OPAL_ENABLE_FT_THREAD ? "yes" : "no");

    (void)asprintf(&crdebug_support, "%s",
                   OPAL_ENABLE_CRDEBUG ? "yes" : "no");

    /* output values */
    opal_info_out("Configured by", "config:user", OPAL_CONFIGURE_USER);
    opal_info_out("Configured on", "config:timestamp", OPAL_CONFIGURE_DATE);
    opal_info_out("Configure host", "config:host", OPAL_CONFIGURE_HOST);

    opal_info_out("Built by", "build:user", OMPI_BUILD_USER);
    opal_info_out("Built on", "build:timestamp", OMPI_BUILD_DATE);
    opal_info_out("Built host", "build:host", OMPI_BUILD_HOST);

    opal_info_out("C bindings", "bindings:c", "yes");
    opal_info_out("C++ bindings", "bindings:cxx", cxx);
    opal_info_out("Fort mpif.h", "bindings:mpif.h", fortran_mpifh);
    free(fortran_mpifh);
    opal_info_out("Fort use mpi", "bindings:use_mpi",
                  fortran_usempi);
    opal_info_out("Fort use mpi size", "bindings:use_mpi:size",
                  ompi_info_deprecated_value);
    opal_info_out("Fort use mpi_f08", "bindings:use_mpi_f08",
                  fortran_usempif08);
    opal_info_out("Fort mpi_f08 compliance", "bindings:use_mpi_f08:compliance",
                  fortran_usempif08_compliance);
    if (NULL != fortran_usempif08_compliance) {
        free(fortran_usempif08_compliance);
    }
    opal_info_out("Fort mpi_f08 subarrays", "bindings:use_mpi_f08:subarrays-supported",
                  fortran_build_f08_subarrays);
    opal_info_out("Java bindings", "bindings:java", java);

    opal_info_out("Wrapper compiler rpath", "compiler:all:rpath",
                  WRAPPER_RPATH_SUPPORT);
    opal_info_out("C compiler", "compiler:c:command", OPAL_CC);
    opal_info_out("C compiler absolute", "compiler:c:absolute",
                  OPAL_CC_ABSOLUTE);
    opal_info_out("C compiler family name", "compiler:c:familyname",
                  _STRINGIFY(OPAL_BUILD_PLATFORM_COMPILER_FAMILYNAME));
    opal_info_out("C compiler version", "compiler:c:version",
                  _STRINGIFY(OPAL_BUILD_PLATFORM_COMPILER_VERSION_STR));

    if (want_all) {
        opal_info_out_int("C char size", "compiler:c:sizeof:char", sizeof(char));
        /* JMS: should be fixed in MPI-2.2 to differentiate between C
           _Bool and C++ bool.  For the moment, the code base assumes
           that they are the same.  Because of opal_config_bottom.h,
           we can sizeof(bool) here, so we might as well -- even
           though this technically isn't right.  This should be fixed
           when we update to MPI-2.2.  See below for note about C++
           bool alignment. */
        opal_info_out_int("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
        opal_info_out_int("C short size", "compiler:c:sizeof:short", sizeof(short));
        opal_info_out_int("C int size", "compiler:c:sizeof:int", sizeof(int));
        opal_info_out_int("C long size", "compiler:c:sizeof:long", sizeof(long));
        opal_info_out_int("C float size", "compiler:c:sizeof:float", sizeof(float));
        opal_info_out_int("C double size", "compiler:c:sizeof:double", sizeof(double));
        opal_info_out_int("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
        opal_info_out_int("C char align", "compiler:c:align:char", OPAL_ALIGNMENT_CHAR);
#if OMPI_BUILD_CXX_BINDINGS
        /* JMS: See above for note about C++ bool size.  We don't have
           the bool alignment the way configure currently runs -- need
           to clean this up when we update for MPI-2.2. */
        opal_info_out_int("C bool align", "compiler:c:align:bool", OPAL_ALIGNMENT_CXX_BOOL);
#else
        opal_info_out("C bool align", "compiler:c:align:bool", "skipped");
#endif
        opal_info_out_int("C int align", "compiler:c:align:int", OPAL_ALIGNMENT_INT);
        opal_info_out_int("C float align", "compiler:c:align:float", OPAL_ALIGNMENT_FLOAT);
        opal_info_out_int("C double align", "compiler:c:align:double", OPAL_ALIGNMENT_DOUBLE);
    }

    opal_info_out("C++ compiler", "compiler:cxx:command", OMPI_CXX);
    opal_info_out("C++ compiler absolute", "compiler:cxx:absolute", OMPI_CXX_ABSOLUTE);
    opal_info_out("Fort compiler", "compiler:fortran:command", OMPI_FC);
    opal_info_out("Fort compiler abs", "compiler:fortran:absolute",
                  OMPI_FC_ABSOLUTE);
    opal_info_out("Fort ignore TKR", "compiler:fortran:ignore_tkr",
                  fortran_have_ignore_tkr);
    free(fortran_have_ignore_tkr);
    opal_info_out("Fort 08 assumed shape",
                  "compiler:fortran:f08_assumed_rank",
                  fortran_have_f08_assumed_rank);
    opal_info_out("Fort optional args",
                  "compiler:fortran:optional_arguments",
                  fortran_have_optional_args);
    opal_info_out("Fort BIND(C)",
                  "compiler:fortran:bind_c",
                  fortran_have_bind_c);
    opal_info_out("Fort PRIVATE",
                  "compiler:fortran:private",
                  fortran_have_private);
    opal_info_out("Fort ABSTRACT",
                  "compiler:fortran:abstract",
                  fortran_have_abstract);
    opal_info_out("Fort ASYNCHRONOUS",
                  "compiler:fortran:asynchronous",
                  fortran_have_asynchronous);
    opal_info_out("Fort PROCEDURE",
                  "compiler:fortran:procedure",
                  fortran_have_procedure);
    opal_info_out("Fort f08 using wrappers",
                  "compiler:fortran:08_wrappers",
                  fortran_08_using_wrappers_for_choice_buffer_functions);

    if (want_all) {

        /* Will always have the size of Fortran integer */

        opal_info_out_int("Fort integer size", "compiler:fortran:sizeof:integer",
                      OMPI_SIZEOF_FORTRAN_INTEGER);

        opal_info_out_int("Fort logical size", "compiler:fortran:sizeof:logical",
                      OMPI_SIZEOF_FORTRAN_LOGICAL);
        opal_info_out_int("Fort logical value true", "compiler:fortran:value:true",
                      OMPI_FORTRAN_VALUE_TRUE);


        /* May or may not have the other Fortran sizes */

        if (OMPI_BUILD_FORTRAN_MPIFH_BINDINGS ||
            OMPI_BUILD_FORTRAN_USEMPI_BINDINGS ||
            OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS) {
            opal_info_out("Fort have integer1", "compiler:fortran:have:integer1",
                          OMPI_HAVE_FORTRAN_INTEGER1 ? "yes" : "no");
            opal_info_out("Fort have integer2", "compiler:fortran:have:integer2",
                          OMPI_HAVE_FORTRAN_INTEGER2 ? "yes" : "no");
            opal_info_out("Fort have integer4", "compiler:fortran:have:integer4",
                          OMPI_HAVE_FORTRAN_INTEGER4 ? "yes" : "no");
            opal_info_out("Fort have integer8", "compiler:fortran:have:integer8",
                          OMPI_HAVE_FORTRAN_INTEGER8 ? "yes" : "no");
            opal_info_out("Fort have integer16", "compiler:fortran:have:integer16",
                          OMPI_HAVE_FORTRAN_INTEGER16 ? "yes" : "no");

            opal_info_out("Fort have real4", "compiler:fortran:have:real4",
                          OMPI_HAVE_FORTRAN_REAL4 ? "yes" : "no");
            opal_info_out("Fort have real8", "compiler:fortran:have:real8",
                          OMPI_HAVE_FORTRAN_REAL8 ? "yes" : "no");
            opal_info_out("Fort have real16", "compiler:fortran:have:real16",
                          OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C ? "yes" : "no");

            opal_info_out("Fort have complex8", "compiler:fortran:have:complex8",
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? "yes" : "no");
            opal_info_out("Fort have complex16", "compiler:fortran:have:complex16",
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? "yes" : "no");
            opal_info_out("Fort have complex32", "compiler:fortran:have:complex32",
                          OMPI_HAVE_FORTRAN_COMPLEX32 && OMPI_REAL16_MATCHES_C ? "yes" : "no");

            opal_info_out_int("Fort integer1 size", "compiler:fortran:sizeof:integer1",
                          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_SIZEOF_FORTRAN_INTEGER1 : -1);
            opal_info_out_int("Fort integer2 size", "compiler:fortran:sizeof:integer2",
                          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_SIZEOF_FORTRAN_INTEGER2 : -1);
            opal_info_out_int("Fort integer4 size", "compiler:fortran:sizeof:integer4",
                          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_SIZEOF_FORTRAN_INTEGER4 : -1);
            opal_info_out_int("Fort integer8 size", "compiler:fortran:sizeof:integer8",
                          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_SIZEOF_FORTRAN_INTEGER8 : -1);
            opal_info_out_int("Fort integer16 size", "compiler:fortran:sizeof:integer16",
                          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_SIZEOF_FORTRAN_INTEGER16 : -1);

            opal_info_out_int("Fort real size", "compiler:fortran:sizeof:real",
                          OMPI_SIZEOF_FORTRAN_REAL);
            opal_info_out_int("Fort real4 size", "compiler:fortran:sizeof:real4",
                          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_SIZEOF_FORTRAN_REAL4 : -1);
            opal_info_out_int("Fort real8 size", "compiler:fortran:sizeof:real8",
                          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_SIZEOF_FORTRAN_REAL8 : -1);
            opal_info_out_int("Fort real16 size", "compiler:fortran:sizeof:real17",
                          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_SIZEOF_FORTRAN_REAL16 : -1);

            opal_info_out_int("Fort dbl prec size",
                          "compiler:fortran:sizeof:double_precision",
                          OMPI_SIZEOF_FORTRAN_DOUBLE_PRECISION);

            opal_info_out_int("Fort cplx size", "compiler:fortran:sizeof:complex",
                          OMPI_SIZEOF_FORTRAN_COMPLEX);
            opal_info_out_int("Fort dbl cplx size",
                          "compiler:fortran:sizeof:double_complex",
                          OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX ? OMPI_SIZEOF_FORTRAN_DOUBLE_COMPLEX : -1);
            opal_info_out_int("Fort cplx8 size", "compiler:fortran:sizeof:complex8",
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_SIZEOF_FORTRAN_COMPLEX8 : -1);
            opal_info_out_int("Fort cplx16 size", "compiler:fortran:sizeof:complex16",
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_SIZEOF_FORTRAN_COMPLEX16 : -1);
            opal_info_out_int("Fort cplx32 size", "compiler:fortran:sizeof:complex32",
                          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_SIZEOF_FORTRAN_COMPLEX32 : -1);

            opal_info_out_int("Fort integer align", "compiler:fortran:align:integer",
                          OMPI_ALIGNMENT_FORTRAN_INTEGER);
            opal_info_out_int("Fort integer1 align", "compiler:fortran:align:integer1",
                          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_ALIGNMENT_FORTRAN_INTEGER1 : -1);
            opal_info_out_int("Fort integer2 align", "compiler:fortran:align:integer2",
                          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_ALIGNMENT_FORTRAN_INTEGER2 : -1);
            opal_info_out_int("Fort integer4 align", "compiler:fortran:align:integer4",
                          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_ALIGNMENT_FORTRAN_INTEGER4 : -1);
            opal_info_out_int("Fort integer8 align", "compiler:fortran:align:integer8",
                          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_ALIGNMENT_FORTRAN_INTEGER8 : -1);
            opal_info_out_int("Fort integer16 align", "compiler:fortran:align:integer16",
                          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_ALIGNMENT_FORTRAN_INTEGER16 : -1);

            opal_info_out_int("Fort real align", "compiler:fortran:align:real",
                          OMPI_ALIGNMENT_FORTRAN_REAL);
            opal_info_out_int("Fort real4 align", "compiler:fortran:align:real4",
                          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_ALIGNMENT_FORTRAN_REAL4 : -1);
            opal_info_out_int("Fort real8 align", "compiler:fortran:align:real8",
                          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_ALIGNMENT_FORTRAN_REAL8 : -1);
            opal_info_out_int("Fort real16 align", "compiler:fortran:align:real16",
                          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_ALIGNMENT_FORTRAN_REAL16 : -1);

            opal_info_out_int("Fort dbl prec align",
                          "compiler:fortran:align:double_precision",
                          OMPI_ALIGNMENT_FORTRAN_DOUBLE_PRECISION);

            opal_info_out_int("Fort cplx align", "compiler:fortran:align:complex",
                          OMPI_ALIGNMENT_FORTRAN_COMPLEX);
            opal_info_out_int("Fort dbl cplx align",
                          "compiler:fortran:align:double_complex",
                          OMPI_HAVE_FORTRAN_DOUBLE_COMPLEX ? OMPI_ALIGNMENT_FORTRAN_DOUBLE_COMPLEX : -1);
            opal_info_out_int("Fort cplx8 align", "compiler:fortran:align:complex8",
                          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX8 : -1);
            opal_info_out_int("Fort cplx16 align", "compiler:fortran:align:complex16",
                          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX16 : -1);
            opal_info_out_int("Fort cplx32 align", "compiler:fortran:align:complex32",
                          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX32 : -1);

        } else {
            opal_info_out("Fort real size", "compiler:fortran:sizeof:real", "skipped");
            opal_info_out("Fort dbl prec size",
                          "compiler:fortran:sizeof:double_precision", "skipped");
            opal_info_out("Fort cplx size", "compiler:fortran:sizeof:complex", "skipped");
            opal_info_out("Fort dbl cplx size",
                          "compiler:fortran:sizeof:double_complex", "skipped");

            opal_info_out("Fort integer align", "compiler:fortran:align:integer", "skipped");
            opal_info_out("Fort real align", "compiler:fortran:align:real", "skipped");
            opal_info_out("Fort dbl prec align",
                          "compiler:fortran:align:double_precision","skipped");
            opal_info_out("Fort cplx align", "compiler:fortran:align:complex", "skipped");
            opal_info_out("Fort dbl cplx align",
                          "compiler:fortran:align:double_complex", "skipped");
        }
    }

    opal_info_out("C profiling", "option:profiling:c", cprofiling);
    opal_info_out("C++ profiling", "option:profiling:cxx", cxxprofiling);
    opal_info_out("Fort mpif.h profiling", "option:profiling:mpif.h",
                  fortran_mpifh_profiling);
    opal_info_out("Fort use mpi profiling", "option:profiling:use_mpi",
                  fortran_usempi_profiling);
    opal_info_out("Fort use mpi_f08 prof",
                  "option:profiling:use_mpi_f08",
                  fortran_usempif08_profiling);

    opal_info_out("C++ exceptions", "option:cxx_exceptions", cxxexceptions);
    opal_info_out("Thread support", "option:threads", threads);
    free(threads);
    opal_info_out("Sparse Groups", "option:sparse:groups", sparse_groups);

    if (want_all) {

        /* Don't display the build CPPFLAGS or CXXCPPFLAGS because they're
         * just -I$(top_srcdir)/include, etc.  Hence, they're a) boring,
         * and c) specific for ompi_info.
         */

        opal_info_out("Build CFLAGS", "option:build:cflags", OMPI_BUILD_CFLAGS);
        opal_info_out("Build CXXFLAGS", "option:build:cxxflags", OMPI_BUILD_CXXFLAGS);
        opal_info_out("Build FCFLAGS", "option:build:fcflags", OMPI_BUILD_FCFLAGS);
        opal_info_out("Build LDFLAGS", "option:build:ldflags", OMPI_BUILD_LDFLAGS);
        opal_info_out("Build LIBS", "option:build:libs", OMPI_BUILD_LIBS);

        opal_info_out("Wrapper extra CFLAGS", "option:wrapper:extra_cflags",
                      WRAPPER_EXTRA_CFLAGS);
        opal_info_out("Wrapper extra CXXFLAGS", "option:wrapper:extra_cxxflags",
                      WRAPPER_EXTRA_CXXFLAGS);
        opal_info_out("Wrapper extra FCFLAGS", "option:wrapper:extra_fcflags",
                      WRAPPER_EXTRA_FCFLAGS);
        opal_info_out("Wrapper extra LDFLAGS", "option:wrapper:extra_ldflags",
                      WRAPPER_EXTRA_LDFLAGS);
        opal_info_out("Wrapper extra LIBS", "option:wrapper:extra_libs",
                      WRAPPER_EXTRA_LIBS);
    }

    opal_info_out("Internal debug support", "option:debug", debug);
    opal_info_out("MPI interface warnings", "option:mpi-interface-warning", mpi_interface_warning);
    opal_info_out("MPI parameter check", "option:mpi-param-check", paramcheck);
    opal_info_out("Memory profiling support", "option:mem-profile", memprofile);
    opal_info_out("Memory debugging support", "option:mem-debug", memdebug);
    opal_info_out("libltdl support", "option:dlopen", want_libltdl);
    opal_info_out("Heterogeneous support", "options:heterogeneous", heterogeneous);
#if OMPI_RTE_ORTE
    opal_info_out("mpirun default --prefix", "mpirun:prefix_by_default",
                  mpirun_prefix_by_default);
#endif
    opal_info_out("MPI I/O support", "options:mpi-io", have_mpi_io);
    opal_info_out("MPI_WTIME support", "options:mpi-wtime", wtime_support);
    opal_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
    opal_info_out("Host topology support", "options:host-topology",
                  topology_support);

    opal_info_out("MPI extensions", "options:mpi_ext", OMPI_MPIEXT_COMPONENTS);

    opal_info_out("FT Checkpoint support", "options:ft_support", ft_support);
    free(ft_support);

    opal_info_out("C/R Enabled Debugging", "options:crdebug_support", crdebug_support);
    free(crdebug_support);

    opal_info_out("VampirTrace support", "options:vt", vt_support);

    opal_info_out_int("MPI_MAX_PROCESSOR_NAME", "options:mpi-max-processor-name",
                  MPI_MAX_PROCESSOR_NAME);
    opal_info_out_int("MPI_MAX_ERROR_STRING",   "options:mpi-max-error-string",
                  MPI_MAX_ERROR_STRING);
    opal_info_out_int("MPI_MAX_OBJECT_NAME",    "options:mpi-max-object-name",
                  MPI_MAX_OBJECT_NAME);
    opal_info_out_int("MPI_MAX_INFO_KEY",       "options:mpi-max-info-key",
                  MPI_MAX_INFO_KEY);
    opal_info_out_int("MPI_MAX_INFO_VAL",       "options:mpi-max-info-val",
                  MPI_MAX_INFO_VAL);
    opal_info_out_int("MPI_MAX_PORT_NAME",      "options:mpi-max-port-name",
                  MPI_MAX_PORT_NAME);
#if OMPI_PROVIDE_MPI_FILE_INTERFACE
    opal_info_out_int("MPI_MAX_DATAREP_STRING", "options:mpi-max-datarep-string",
                  MPI_MAX_DATAREP_STRING);
#else
    opal_info_out("MPI_MAX_DATAREP_STRING", "options:mpi-max-datarep-string",
                  "IO interface not provided");
#endif
    
    /* This block displays all the options with which the current
     * installation of oshmem was configured. */
    {
        char *oshmem_fortran = OSHMEM_BUILD_FORTRAN_BINDINGS ? "yes" : "no";
        char *oshmem_compat = OSHMEM_SPEC_COMPAT ? "yes" : "no";
        char *oshmem_param_check = OSHMEM_PARAM_CHECK ? "yes" : "no";
        char *oshmem_profiling = OSHMEM_PROFILING ? "yes" : "no";
        
        opal_info_out("OSHMEM C bindings", "oshmem:bindings:c", "yes");
        opal_info_out("OSHMEM Fortran bindings", "oshmem:bindings:fort", oshmem_fortran);
        opal_info_out("OSHMEM SGI/Quadrics mode", "oshmem:options:spec_compat", oshmem_compat);
        opal_info_out("OSHMEM API param check", "oshmem:options:param_check", oshmem_param_check);
        opal_info_out("OSHMEM profiling support", "oshmem:options:profiling", oshmem_profiling);
    }
}
