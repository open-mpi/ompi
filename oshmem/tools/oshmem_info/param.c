/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * Copyright (c) 2014-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
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
#include "opal/include/opal/version.h"
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


const char *opal_info_deprecated_value = "deprecated-ompi-info-value";

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
    char *fortran;
    char *heterogeneous;
    char *memprofile;
    char *memdebug;
    char *debug;
    char *mpi_interface_warning;
    char *cprofiling;
    char *cxxprofiling;
    char *fortran_profiling;
    char *cxxexceptions;
    char *threads;
    char *have_dl;
#if OMPI_RTE_ORTE
    char *mpirun_prefix_by_default;
#endif
    char *sparse_groups;
    char *wtime_support;
    char *symbol_visibility;
    char *ft_support;
    char *crdebug_support;
    char *topology_support;

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

    heterogeneous = OPAL_ENABLE_HETEROGENEOUS_SUPPORT ? "yes" : "no";
    memprofile = OPAL_ENABLE_MEM_PROFILE ? "yes" : "no";
    memdebug = OPAL_ENABLE_MEM_DEBUG ? "yes" : "no";
    debug = OPAL_ENABLE_DEBUG ? "yes" : "no";
    mpi_interface_warning = OMPI_WANT_MPI_INTERFACE_WARNING ? "yes" : "no";
    cprofiling = "yes";
    cxxprofiling = OMPI_BUILD_CXX_BINDINGS ? "yes" : "no";
    cxxexceptions = (OMPI_BUILD_CXX_BINDINGS && OMPI_HAVE_CXX_EXCEPTION_SUPPORT) ? "yes" : "no";
    fortran_profiling = (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_MPIFH_BINDINGS) ? "yes" : "no";
    have_dl = OPAL_HAVE_DL_SUPPORT ? "yes" : "no";
#if OMPI_RTE_ORTE
    mpirun_prefix_by_default = ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT ? "yes" : "no";
#endif
    sparse_groups = OMPI_GROUP_SPARSE ? "yes" : "no";
    wtime_support = OPAL_TIMER_USEC_NATIVE ? "native" : "gettimeofday";
    symbol_visibility = OPAL_C_HAVE_VISIBILITY ? "yes" : "no";
    topology_support = "yes";

    /* setup strings that require allocation */
    if (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_MPIFH_BINDINGS) {
        (void)asprintf(&fortran, "yes (%s)",
                       (OPAL_HAVE_WEAK_SYMBOLS ? "all" :
                        (OMPI_FORTRAN_CAPS ? "caps" :
                         (OMPI_FORTRAN_PLAIN ? "lower case" :
                          (OMPI_FORTRAN_SINGLE_UNDERSCORE ? "single underscore" : "double underscore")))));
    } else {
        fortran = strdup("no");
    }

#if OMPI_RTE_ORTE
    (void)asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: yes, OPAL support: yes, OMPI progress: %s, ORTE progress: yes, Event lib: yes)",
                   "posix", OPAL_ENABLE_PROGRESS_THREADS ? "yes" : "no");
#else
    (void)asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: yes, OPAL support: yes, OMPI progress: %s, Event lib: yes)",
                   "posix", OPAL_ENABLE_PROGRESS_THREADS ? "yes" : "no");
#endif

    (void)asprintf(&ft_support, "%s (checkpoint thread: %s)",
                   OPAL_ENABLE_FT ? "yes" : "no", OPAL_ENABLE_FT_THREAD ? "yes" : "no");

    (void)asprintf(&crdebug_support, "%s",
                   OPAL_ENABLE_CRDEBUG ? "yes" : "no");

    /* output values */
    opal_info_out("Configured by", "config:user", OPAL_CONFIGURE_USER);
    opal_info_out("Configured on", "config:timestamp", OPAL_CONFIGURE_DATE);
    opal_info_out("Configure host", "config:host", OPAL_CONFIGURE_HOST);
    opal_info_out("Configure command line", "config:cli", OPAL_CONFIGURE_CLI);

    opal_info_out("Built by", "build:user", OMPI_BUILD_USER);
    opal_info_out("Built on", "build:timestamp", OMPI_BUILD_DATE);
    opal_info_out("Built host", "build:host", OMPI_BUILD_HOST);

    opal_info_out("C bindings", "bindings:c", "yes");
    opal_info_out("Fort shmem.fh", "bindings:fortran", fortran);
    free(fortran);

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

    if (want_all) {

        /* Will always have the size of Fortran integer */

        opal_info_out_int("Fort integer size", "compiler:fortran:sizeof:integer",
                      OMPI_SIZEOF_FORTRAN_INTEGER);

        opal_info_out_int("Fort logical size", "compiler:fortran:sizeof:logical",
                      OMPI_SIZEOF_FORTRAN_LOGICAL);
        opal_info_out_int("Fort logical value true", "compiler:fortran:value:true",
                      OMPI_FORTRAN_VALUE_TRUE);


        /* May or may not have the other Fortran sizes */

        if (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_MPIFH_BINDINGS) {
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
    opal_info_out("Fort shmem.fh profiling", "option:profiling:shmem.fh",
                  fortran_profiling);

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
    opal_info_out("dl support", "option:dlopen", have_dl);
    opal_info_out("Heterogeneous support", "options:heterogeneous", heterogeneous);
#if OMPI_RTE_ORTE
    opal_info_out("mpirun default --prefix", "mpirun:prefix_by_default",
                  mpirun_prefix_by_default);
#endif
    opal_info_out("MPI_WTIME support", "options:mpi-wtime", wtime_support);
    opal_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
    opal_info_out("Host topology support", "options:host-topology",
                  topology_support);

    opal_info_out("MPI extensions", "options:mpi_ext", OMPI_MPIEXT_COMPONENTS);

    opal_info_out("FT Checkpoint support", "options:ft_support", ft_support);
    free(ft_support);

    opal_info_out("C/R Enabled Debugging", "options:crdebug_support", crdebug_support);
    free(crdebug_support);

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
    opal_info_out_int("MPI_MAX_DATAREP_STRING", "options:mpi-max-datarep-string",
                  MPI_MAX_DATAREP_STRING);

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
