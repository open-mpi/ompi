#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = \
        mpirun2

mpirun2_SOURCES = \
        mpirun2.c 

mpirun2_LDADD = $(libs) 
mpirun2_DEPENDENCIES = $(mpirun2_LDADD)
