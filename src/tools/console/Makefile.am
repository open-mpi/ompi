#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = \
        ompiconsole

ompiconsole_SOURCES = \
        ompiconsole.c 

ompiconsole_LDADD = $(libs) 
ompiconsole_DEPENDENCIES = $(ompiconsole_LDADD)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)
