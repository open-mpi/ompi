# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_PROGRAMS		= oob_cofs_test pcm_cofs_test

oob_cofs_test_SOURCES 	= oob_cofs_test.c
oob_cofs_test_LDADD	= \
			../../../../src/mca/lam/oob/cofs/libmca_lam_oob_cofs.la \
			../../../../src/liblam.la

pcm_cofs_test_SOURCES 	= pcm_cofs_test.c
pcm_cofs_test_LDADD	= \
			../../../../src/mca/lam/pcm/cofs/libmca_lam_pcm_cofs.la \
			../../../../src/mca/lam/oob/cofs/libmca_lam_oob_cofs.la \
			../../../../src/liblam.la
