#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = \
        ompi-console

ompi-console_SOURCES = \
        ompi-console.c 

ompi-console_LDADD = $(libs) 
ompi-console_DEPENDENCIES = $(ompi-console_LDADD)

clean-local:
	test -z "$(OMPI_CXX_TEMPLATE_REPOSITORY)" || $(RM) -rf $(OMPI_CXX_TEMPLATE_REPOSITORY)
