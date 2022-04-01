/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

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
#include "opal/opal_portable_platform.h"
#include "opal/class/opal_value_array.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/printf.h"
#include "opal/memoryhooks/memory.h"
#include "opal/runtime/opal_info_support.h"

#include "ompi/tools/ompi_info/ompi_info.h"


const char *ompi_info_deprecated_value = "deprecated-ompi-info-value";

static void append(char *dest, size_t max, int *first, char *src)
{
    size_t len;

    if (NULL == src) {
        return;
    }

    len = max - strlen(dest);
    if (!(*first)) {
        strncat(dest, ", ", len - 1);
        len = max - strlen(dest);
    }
    strncat(dest, src, len - 1);
    *first = 0;
}


/*
 * do_config
 * Accepts:
 *	- want_all: boolean flag; TRUE -> display all options
 *				  FALSE -> display selected options
 *
 * This function displays all the options with which the current
 * installation of ompi was configured. There are many options here
 * that are carried forward from OMPI-7 and are not mca parameters
 * in OMPI-10. I have to dig through the invalid options and replace
 * them with OMPI-10 options.
 */
void ompi_info_do_config(bool want_all)
{
    char *fortran_mpifh;
    char *fortran_usempi;
    char *fortran_usempif08;
    char *fortran_usempif08_compliance;
    char *fortran_have_ignore_tkr;
    char *fortran_have_f08_assumed_rank;
    char *fortran_build_f08_subarrays;
    char *fortran_have_optional_args;
    char *fortran_have_interface;
    char *fortran_have_iso_fortran_env;
    char *fortran_have_storage_size;
    char *fortran_have_bind_c;
    char *fortran_have_iso_c_binding;
    char *fortran_have_bind_c_sub;
    char *fortran_have_bind_c_type;
    char *fortran_have_bind_c_type_name;
    char *fortran_have_private;
    char *fortran_have_abstract;
    char *fortran_have_asynchronous;
    char *fortran_have_procedure;
    char *fortran_have_use_only;
    char *fortran_have_c_funloc;
    char *fortran_08_using_wrappers_for_choice_buffer_functions;
    char *fortran_build_sizeof;
    char *java;
    char *heterogeneous;
    char *memprofile;
    char *memdebug;
    char *debug;
    char *mpi_interface_warning;
    char *cprofiling;
    char *fortran_mpifh_profiling;
    char *fortran_usempi_profiling;
    char *fortran_usempif08_profiling;
    char *threads;
    char *have_dl;
    char *sparse_groups;
    char *wtime_support;
    char *symbol_visibility;
    char *ft_support;
    char *ft_mpi_support;
    char *topology_support;
    char *ipv6_support;

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

    /* The current mpi_f08 implementation does not support Fortran
       subarrays.  However, someday it may/will.  Hence, I'm leaving
       in all the logic that checks to see whether subarrays are
       supported, but I'm just hard-coding
       OMPI_BUILD_FORTRAN_F08_SUBARRAYS to 0 (we used to have a
       prototype mpi_f08 module that implemented a handful of
       descriptor-based interfaces and supported subarrays, but that
       has been removed). */
    const int OMPI_BUILD_FORTRAN_F08_SUBARRAYS = 0;

    /* setup the strings that don't require allocations*/
    if (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_USEMPI_BINDINGS) {
        if (OMPI_FORTRAN_HAVE_IGNORE_TKR) {
            fortran_usempi = "yes (full: ignore TKR)";
        } else {
            fortran_usempi = "yes (limited: overloading)";
        }
    } else {
        fortran_usempi = "no";
    }
    fortran_usempif08 = OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_USEMPIF08_BINDINGS ? "yes" : "no";
    fortran_have_f08_assumed_rank = OMPI_FORTRAN_HAVE_F08_ASSUMED_RANK ?
        "yes" : "no";
    fortran_build_f08_subarrays = OMPI_BUILD_FORTRAN_F08_SUBARRAYS ?
        "yes" : "no";
    fortran_have_optional_args = OMPI_FORTRAN_HAVE_OPTIONAL_ARGS ?
        "yes" : "no";
    fortran_have_interface = OMPI_FORTRAN_HAVE_INTERFACE ? "yes" : "no";
    fortran_have_iso_fortran_env = OMPI_FORTRAN_HAVE_ISO_FORTRAN_ENV ?
        "yes" : "no";
    fortran_have_storage_size = OMPI_FORTRAN_HAVE_STORAGE_SIZE ? "yes" : "no";
    fortran_have_bind_c = OMPI_FORTRAN_HAVE_BIND_C ? "yes" : "no";
    fortran_have_iso_c_binding = OMPI_FORTRAN_HAVE_ISO_C_BINDING ?
        "yes" : "no";
    fortran_have_bind_c_sub = OMPI_FORTRAN_HAVE_BIND_C_SUB ? "yes" : "no";
    fortran_have_bind_c_type = OMPI_FORTRAN_HAVE_BIND_C_TYPE ? "yes" : "no";
    fortran_have_bind_c_type_name = OMPI_FORTRAN_HAVE_BIND_C_TYPE_NAME ?
        "yes" : "no";
    fortran_have_private = OMPI_FORTRAN_HAVE_PRIVATE ? "yes" : "no";
    fortran_have_abstract = OMPI_FORTRAN_HAVE_ABSTRACT ? "yes" : "no";
    fortran_have_asynchronous = OMPI_FORTRAN_HAVE_ASYNCHRONOUS ? "yes" : "no";
    fortran_have_procedure = OMPI_FORTRAN_HAVE_PROCEDURE ? "yes" : "no";
    fortran_have_use_only = OMPI_FORTRAN_HAVE_USE_ONLY ? "yes" : "no";
    fortran_have_c_funloc = OMPI_FORTRAN_HAVE_C_FUNLOC ? "yes" : "no";
    fortran_08_using_wrappers_for_choice_buffer_functions =
        OMPI_FORTRAN_NEED_WRAPPER_ROUTINES ? "yes" : "no";
    fortran_build_sizeof = OMPI_FORTRAN_BUILD_SIZEOF ?
        "yes" : "no";

    /* Build a string describing what level of compliance the mpi_f08
       module has */
    char f08_msg[1024];
    if (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_USEMPIF08_BINDINGS) {

        /* Do we have everything? (not including PROTECTED, which
           isn't *needed* for the mpi_f08 module compliance -- it's
           just *nice to have*) */
        if (OMPI_BUILD_FORTRAN_F08_SUBARRAYS &&
            OMPI_FORTRAN_HAVE_PRIVATE &&
            OMPI_FORTRAN_HAVE_ABSTRACT &&
            OMPI_FORTRAN_HAVE_ASYNCHRONOUS &&
            OMPI_FORTRAN_HAVE_PROCEDURE &&
            OMPI_FORTRAN_HAVE_USE_ONLY &&
            OMPI_FORTRAN_HAVE_C_FUNLOC &&
            OMPI_FORTRAN_NEED_WRAPPER_ROUTINES) {
            fortran_usempif08_compliance = "The mpi_f08 module is available, and is fully compliant.  w00t!";
        } else {
            int first = 1;
            snprintf(f08_msg, sizeof(f08_msg),
                     "The mpi_f08 module is available, but due to limitations in the %s compiler and/or Open MPI, does not support the following: ",
                     OMPI_FC);
            if (!OMPI_BUILD_FORTRAN_F08_SUBARRAYS) {
                append(f08_msg, sizeof(f08_msg), &first, "array subsections");
            }
            if (!OMPI_FORTRAN_HAVE_PRIVATE) {
                append(f08_msg, sizeof(f08_msg), &first,
                       "private MPI_Status members");
            }
            if (!OMPI_FORTRAN_HAVE_ABSTRACT) {
                append(f08_msg, sizeof(f08_msg), &first,
                       "ABSTRACT INTERFACE function pointers");
            }
            if (!OMPI_FORTRAN_HAVE_ASYNCHRONOUS) {
                append(f08_msg, sizeof(f08_msg), &first,
                       "Fortran '08-specified ASYNCHRONOUS behavior");
            }
            if (!OMPI_FORTRAN_HAVE_PROCEDURE) {
                append(f08_msg, sizeof(f08_msg), &first, "PROCEDUREs");
            }
            if (!OMPI_FORTRAN_HAVE_USE_ONLY) {
                append(f08_msg, sizeof(f08_msg), &first, "USE_ONLY");
            }
            if (!OMPI_FORTRAN_HAVE_C_FUNLOC) {
                append(f08_msg, sizeof(f08_msg), &first, "C_FUNLOCs");
            }
            if (OMPI_FORTRAN_NEED_WRAPPER_ROUTINES) {
                append(f08_msg, sizeof(f08_msg), &first,
                       "direct passthru (where possible) to underlying Open MPI's C functionality");
            }
            fortran_usempif08_compliance = f08_msg;
        }
    } else {
        fortran_usempif08_compliance = "The mpi_f08 module was not built";
    }

    java = OMPI_WANT_JAVA_BINDINGS ? "yes" : "no";
    heterogeneous = OPAL_ENABLE_HETEROGENEOUS_SUPPORT ? "yes" : "no";
    memprofile = OPAL_ENABLE_MEM_PROFILE ? "yes" : "no";
    memdebug = OPAL_ENABLE_MEM_DEBUG ? "yes" : "no";
    debug = OPAL_ENABLE_DEBUG ? "yes" : "no";
    mpi_interface_warning = OMPI_WANT_MPI_INTERFACE_WARNING ? "yes" : "no";
    cprofiling = "yes";
    fortran_mpifh_profiling = (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_MPIFH_BINDINGS) ? "yes" : "no";
    fortran_usempi_profiling = (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_USEMPI_BINDINGS) ? "yes" : "no";
    fortran_usempif08_profiling = (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_USEMPIF08_BINDINGS) ? "yes" : "no";
    have_dl = OPAL_HAVE_DL_SUPPORT ? "yes" : "no";
    sparse_groups = OMPI_GROUP_SPARSE ? "yes" : "no";
    wtime_support = OPAL_TIMER_USEC_NATIVE ? "native" : "gettimeofday";
    symbol_visibility = OPAL_C_HAVE_VISIBILITY ? "yes" : "no";
    topology_support = "yes";
    ipv6_support = OPAL_ENABLE_IPV6 ? "yes" : "no";

    /* setup strings that require allocation */
    if (OMPI_BUILD_FORTRAN_BINDINGS >= OMPI_FORTRAN_MPIFH_BINDINGS) {
        (void)opal_asprintf(&fortran_mpifh, "yes (%s)",
                       (OPAL_HAVE_WEAK_SYMBOLS ? "all" :
                        (OMPI_FORTRAN_CAPS ? "caps" :
                         (OMPI_FORTRAN_PLAIN ? "lower case" :
                          (OMPI_FORTRAN_SINGLE_UNDERSCORE ? "single underscore" : "double underscore")))));
    } else {
        fortran_mpifh = strdup("no");
    }

    if (OMPI_FORTRAN_HAVE_IGNORE_TKR) {
        /* OMPI_FORTRAN_IGNORE_TKR_PREDECL is already in quotes; it
           didn't work consistently to put it in PLATFORM_STRINGIFY because
           sometimes the compiler would actually interpret the pragma
           in there before stringify-ing it. */
        (void)opal_asprintf(&fortran_have_ignore_tkr, "yes (%s)",
                       OMPI_FORTRAN_IGNORE_TKR_PREDECL);
    } else {
        fortran_have_ignore_tkr = strdup("no");
    }

    (void)opal_asprintf(&threads, "%s (MPI_THREAD_MULTIPLE: yes, OPAL support: yes, OMPI progress: %s, Event lib: yes)",
                   "posix", OPAL_ENABLE_PROGRESS_THREADS ? "yes" : "no");

    (void)opal_asprintf(&ft_support, "%s",
                   OPAL_ENABLE_FT ? "yes" : "no" );

    (void)opal_asprintf(&ft_mpi_support, "%s",
                   OPAL_ENABLE_FT_MPI ? "yes" : "no");

    /* output values */
    opal_info_out("Configured by", "config:user", OPAL_CONFIGURE_USER);
    opal_info_out("Configured on", "config:timestamp", OPAL_CONFIGURE_DATE);
    opal_info_do_hostname();
    opal_info_out("Configure command line", "config:cli", OPAL_CONFIGURE_CLI);

    opal_info_out("Built by", "build:user", OMPI_BUILD_USER);
    opal_info_out("Built on", "build:timestamp", OMPI_BUILD_DATE);
    opal_info_out("Built host", "build:host", OMPI_BUILD_HOST);

    opal_info_out("C bindings", "bindings:c", "yes");
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
    opal_info_out("Fort mpi_f08 subarrays", "bindings:use_mpi_f08:subarrays-supported",
                  fortran_build_f08_subarrays);
    opal_info_out("Java bindings", "bindings:java", java);

    opal_info_out("Wrapper compiler rpath", "compiler:all:rpath",
                  WRAPPER_RPATH_SUPPORT);
    opal_info_out("C compiler", "compiler:c:command", OPAL_CC);
    opal_info_out("C compiler absolute", "compiler:c:absolute",
                  OPAL_CC_ABSOLUTE);
    opal_info_out("C compiler family name", "compiler:c:familyname",
                  PLATFORM_STRINGIFY(PLATFORM_COMPILER_FAMILYNAME));
    opal_info_out("C compiler version", "compiler:c:version",
                  PLATFORM_COMPILER_VERSION_STR);

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
#if defined(HAVE_SHORT_FLOAT)
        opal_info_out_int("C short float size", "compiler:c:sizeof:short_float", sizeof(short float));
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
        opal_info_out_int("C short float size", "compiler:c:sizeof:short_float", sizeof(opal_short_float_t));
#endif
        opal_info_out_int("C float size", "compiler:c:sizeof:float", sizeof(float));
        opal_info_out_int("C double size", "compiler:c:sizeof:double", sizeof(double));
        opal_info_out_int("C long double size", "compiler:c:sizeof:long_double", sizeof(long double));
        opal_info_out_int("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
        opal_info_out_int("C char align", "compiler:c:align:char", OPAL_ALIGNMENT_CHAR);
        opal_info_out("C bool align", "compiler:c:align:bool", "skipped");
        opal_info_out_int("C int align", "compiler:c:align:int", OPAL_ALIGNMENT_INT);
#if defined(HAVE_SHORT_FLOAT)
        opal_info_out_int("C short float align", "compiler:c:align:short_float", OPAL_ALIGNMENT_SHORT_FLOAT);
#elif defined(HAVE_OPAL_SHORT_FLOAT_T)
        opal_info_out_int("C short float align", "compiler:c:align:short_float", OPAL_ALIGNMENT_OPAL_SHORT_FLOAT_T);
#endif
        opal_info_out_int("C float align", "compiler:c:align:float", OPAL_ALIGNMENT_FLOAT);
        opal_info_out_int("C double align", "compiler:c:align:double", OPAL_ALIGNMENT_DOUBLE);
        opal_info_out_int("C long double align", "compiler:c:align:long_double", OPAL_ALIGNMENT_LONG_DOUBLE);
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
    opal_info_out("Fort INTERFACE",
                  "compiler:fortran:interface",
                  fortran_have_interface);
    opal_info_out("Fort ISO_FORTRAN_ENV",
                  "compiler:fortran:iso_fortran_env",
                  fortran_have_iso_fortran_env);
    opal_info_out("Fort STORAGE_SIZE",
                  "compiler:fortran:storage_size",
                  fortran_have_storage_size);
    opal_info_out("Fort BIND(C) (all)",
                  "compiler:fortran:bind_c",
                  fortran_have_bind_c);
    opal_info_out("Fort ISO_C_BINDING",
                  "compiler:fortran:iso_c_binding",
                  fortran_have_iso_c_binding);
    opal_info_out("Fort SUBROUTINE BIND(C)",
                  "compiler:fortran:subroutine_bind_c",
                  fortran_have_bind_c_sub);
    opal_info_out("Fort TYPE,BIND(C)",
                  "compiler:fortran:type_bind_c",
                  fortran_have_bind_c_type);
    opal_info_out("Fort T,BIND(C,name=\"a\")",
                  "compiler:fortran:type_name_bind_c",
                  fortran_have_bind_c_type_name);
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
    opal_info_out("Fort USE...ONLY",
                  "compiler:fortran:use_only",
                  fortran_have_use_only);
    opal_info_out("Fort C_FUNLOC",
                  "compiler:fortran:c_funloc",
                  fortran_have_c_funloc);
    opal_info_out("Fort f08 using wrappers",
                  "compiler:fortran:08_wrappers",
                  fortran_08_using_wrappers_for_choice_buffer_functions);
    opal_info_out("Fort MPI_SIZEOF",
                  "compiler:fortran:mpi_sizeof",
                  fortran_build_sizeof);

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

            opal_info_out("Fort have real2", "compiler:fortran:have:real2",
                          OMPI_HAVE_FORTRAN_REAL2 ? "yes" : "no");
            opal_info_out("Fort have real4", "compiler:fortran:have:real4",
                          OMPI_HAVE_FORTRAN_REAL4 ? "yes" : "no");
            opal_info_out("Fort have real8", "compiler:fortran:have:real8",
                          OMPI_HAVE_FORTRAN_REAL8 ? "yes" : "no");
            opal_info_out("Fort have real16", "compiler:fortran:have:real16",
                          OMPI_HAVE_FORTRAN_REAL16 && OMPI_REAL16_MATCHES_C ? "yes" : "no");

            opal_info_out("Fort have complex4", "compiler:fortran:have:complex4",
                          OMPI_HAVE_FORTRAN_COMPLEX4 ? "yes" : "no");
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
            opal_info_out_int("Fort real2 size", "compiler:fortran:sizeof:real2",
                          OMPI_HAVE_FORTRAN_REAL2 ? OMPI_SIZEOF_FORTRAN_REAL2 : -1);
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
            opal_info_out_int("Fort cplx4 size", "compiler:fortran:sizeof:complex4",
                          OMPI_HAVE_FORTRAN_COMPLEX4 ? OMPI_SIZEOF_FORTRAN_COMPLEX4 : -1);
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
            opal_info_out_int("Fort real2 align", "compiler:fortran:align:real2",
                          OMPI_HAVE_FORTRAN_REAL2 ? OMPI_ALIGNMENT_FORTRAN_REAL2 : -1);
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
            opal_info_out_int("Fort cplx4 align", "compiler:fortran:align:complex4",
                          OMPI_HAVE_FORTRAN_COMPLEX4 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX4 : -1);
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
    opal_info_out("Fort mpif.h profiling", "option:profiling:mpif.h",
                  fortran_mpifh_profiling);
    opal_info_out("Fort use mpi profiling", "option:profiling:use_mpi",
                  fortran_usempi_profiling);
    opal_info_out("Fort use mpi_f08 prof",
                  "option:profiling:use_mpi_f08",
                  fortran_usempif08_profiling);

    opal_info_out("Thread support", "option:threads", threads);
    free(threads);
    opal_info_out("Sparse Groups", "option:sparse:groups", sparse_groups);

    if (want_all) {

        /* Don't display the build CPPFLAGS or CXXCPPFLAGS because they're
         * just -I$(top_srcdir)/include, etc.  Hence, they're a) boring,
         * and c) specific for ompi_info.
         */

        opal_info_out("Build CFLAGS", "option:build:cflags", OMPI_BUILD_CFLAGS);
        opal_info_out("Build FCFLAGS", "option:build:fcflags", OMPI_BUILD_FCFLAGS);
        opal_info_out("Build LDFLAGS", "option:build:ldflags", OMPI_BUILD_LDFLAGS);
        opal_info_out("Build LIBS", "option:build:libs", OMPI_BUILD_LIBS);

        opal_info_out("Wrapper CFLAGS", "option:wrapper:cflags",
                      OMPI_WRAPPER_CFLAGS);
        opal_info_out("Wrapper CXXFLAGS", "option:wrapper:cxxflags",
                      OMPI_WRAPPER_CXXFLAGS);
        opal_info_out("Wrapper FCFLAGS", "option:wrapper:fcflags",
                      OMPI_WRAPPER_FCFLAGS);
        opal_info_out("Wrapper LDFLAGS", "option:wrapper:ldflags",
                      OMPI_WRAPPER_LDFLAGS);
        opal_info_out("Wrapper LIBS", "option:wrapper:libs",
                      OMPI_WRAPPER_LIBS);
    }

    opal_info_out("Internal debug support", "option:debug", debug);
    opal_info_out("MPI interface warnings", "option:mpi-interface-warning", mpi_interface_warning);
    opal_info_out("MPI parameter check", "option:mpi-param-check", paramcheck);
    opal_info_out("Memory profiling support", "option:mem-profile", memprofile);
    opal_info_out("Memory debugging support", "option:mem-debug", memdebug);
    opal_info_out("dl support", "option:dlopen", have_dl);
    opal_info_out("Heterogeneous support", "options:heterogeneous", heterogeneous);
    opal_info_out("MPI_WTIME support", "options:mpi-wtime", wtime_support);
    opal_info_out("Symbol vis. support", "options:visibility", symbol_visibility);
    opal_info_out("Host topology support", "options:host-topology",
                  topology_support);
    opal_info_out("IPv6 support", "options:ipv6", ipv6_support);

    opal_info_out("MPI extensions", "options:mpi_ext", OMPI_MPIEXT_COMPONENTS);

    opal_info_out("Fault Tolerance support", "options:ft_support", ft_support);
    free(ft_support);

    opal_info_out("FT MPI support", "options:ft_mpi_support", ft_mpi_support);
    free(ft_mpi_support);

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

}
