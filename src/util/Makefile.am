#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

AM_CPPFLAGS = -DOMPI_PKGDATADIR=\"$(pkgdatadir)\"
AM_LFLAGS = -Pompi_show_help_yy
LEX_OUTPUT_ROOT = lex.ompi_show_help_yy

libutil_la_CCASFLAGS = $(CPPFLAGS)

noinst_LTLIBRARIES = libutil.la

# Source code files

headers = \
    argv.h \
    bit_ops.h \
    cmd_line.h \
    common_cmd_line.h \
    few.h \
    if.h \
    malloc.h \
    numtostr.h \
    output.h \
    path.h \
    pack.h \
    sys_info.h \
    printf.h \
    proc_info.h \
    os_path.h \
    os_create_dirpath.h \
    pow2.h \
    session_dir.h \
    show_help.h \
    show_help_lex.h \
    daemon_init.h \
    universe_setup_file_io.h \
    strncpy.h

libutil_la_SOURCES = \
    $(headers) \
    asm.S \
    argv.c \
    cmd_line.c \
    common_cmd_line.c \
    few.c \
    if.c \
    malloc.c \
    numtostr.c \
    output.c \
    path.c \
    pack.c \
    printf.c \
    proc_info.c \
    sys_info.c \
    os_path.c \
    os_create_dirpath.c \
    pow2.c \
    session_dir.c \
    show_help.c \
    show_help_lex.l \
    daemon_init.c \
    universe_setup_file_io.c \
    strncpy.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/util
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
