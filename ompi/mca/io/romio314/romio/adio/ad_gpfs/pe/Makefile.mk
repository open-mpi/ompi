## -*- Mode: Makefile; -*-
## vim: set ft=automake :
##
## (C) 2012 by Argonne National Laboratory.
##     See COPYRIGHT in top-level directory.
##

if BUILD_AD_PE

noinst_HEADERS +=                                                    \
    adio/ad_gpfs/pe/ad_pe_aggrs.h

romio_other_sources +=                                               \
    adio/ad_gpfs/pe/ad_pe_aggrs.c

endif BUILD_AD_PE
