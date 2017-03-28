#
# Copyright (c) 2017      IBM Corporation. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Note: the Jenkins tests on LANL-distcheck and travis-ci keep
# failing when I write this Makefile.am in the "obvious" way.
# The test ends up running in $PWD
#     /path/to/openmpi-gitclone/_build/test/symbol_name/
# while files like nmcheck_prefix and nmcheck_prefix.pl are in
#     /path/to/openmpi-gitclone/test/symbol_name/
# and can be located with the env var $srcdir
#
# I tried putting nmchec_prefix.pl in check_SCRIPTS and that does
# cause a "make nmcheck_prefix.pl" step, but it then says there's
# no rule to make that target.
#
# Since I don't know what is the "correct" way to access the extra file
# nmcheck_prefix.pl, what I'm doing for now is using $srcdir to
# find it.

TESTS = nmcheck_prefix

EXTRA_DIST = nmcheck_prefix nmcheck_prefix.pl

AM_TESTS_ENVIRONMENT = MYBASE='$(top_builddir)'; OMPI_LIBMPI_NAME=@OMPI_LIBMPI_NAME@; export MYBASE OMPI_LIBMPI_NAME;

export VERBOSE=yes
