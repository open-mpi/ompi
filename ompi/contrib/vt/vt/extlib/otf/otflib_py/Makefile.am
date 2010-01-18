EXTRA_DIST = otf.i

INCLUDES = \
	$(ZLIB_INCLUDE_LINE) \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib


noinst_HEADERS = \
	pyOTF_AuxiliaryFunctions.h \
	pyOTF_Wrapper.h
	

pkgpython_PYTHON = __init__.py
nodist_pkgpython_PYTHON = otf.py
pkgpyexec_LTLIBRARIES = _otf.la
nodist__otf_la_SOURCES = otf_wrap.c
_otf_la_CPPFLAGS = $(SWIG_PYTHON_CPPFLAGS)
_otf_la_LDFLAGS = -module
_otf_la_LIBADD = $(top_builddir)/otflib/libotf.la
_otf_la_DEPENDENCIES = $(_otf_la_LDADD)

otf_wrap.c : otf.i
	$(SWIG) $(SWIG_PYTHON_OPT) $(INCLUDES) -o $@ $<

CLEANFILES= \
            otf_wrap.c \
            otf.py
