# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# If the user selected --disable-profiling, then we don't traverse
# into the profile directory.

if COMPILE_PROFILING_SEPARATELY
profile_dir		= profile
profile_lib		= profile/libmpi_c_profile.la
else
profile_dir		=
profile_lib		=
endif

SUBDIRS			= $(profile_dir)
DIST_SUBDIRS		= profile

noinst_LTLIBRARIES	= libmpi_c.la

libmpi_c_la_SOURCES	= \
			bindings.h \
			bindings_profiling.h \
			comm_get_name.c \
			comm_set_name.c \
			init.c \
			finalize.c \
libmpi_c_la_LIBADD	= $(profile_lib)
