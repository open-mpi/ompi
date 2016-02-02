# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# This component is a workaround to a bug in libibverbs that prints a
# dire warning that usNIC devices are not supported (of course not --
# usNIC devices provide functionality through libfabric, not
# libibverbs).  This component was written before a better workaround
# was created: a "no op" libibverbs plugin for usNIC devices
# (https://github.com/cisco/libusnic_verbs, and is also available in
# binary form on cisco.com).
#
# Hence, this component no longer builds by default.  It's still
# available if a user specifically asks for it (e.g., if they do not
# want to install the "no op" libibverbs plugin), but it's not the
# default.  This component also has the side-effect of making
# libopen-pal.so depend on libibverbs.so, which can be annoying for
# packagers (which is another reason it isn't built by default any
# more).
#
# This component must be linked statically into libopen-pal because it
# registers a provider for libibverbs at run time, and there's no
# libibverbs API to *un*register a plugin.  Hence, we can't allow this
# code to be dlclosed/removed from the process.  Hence: it must be
# compiled statically into libopen-pal.
#
AC_DEFUN([MCA_opal_common_verbs_usnic_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# MCA_opal_common_verbs_usnic_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_common_verbs_usnic_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/verbs_usnic/Makefile])
    common_verbs_usnic_happy=0

    AC_ARG_WITH(verbs-usnic,
                AC_HELP_STRING([--with-verbs-usnic],
                               [Add support in Open MPI to defeat a seemingly dire warning message from libibverbs that Cisco usNIC devices are not supported.  This support is not compiled by default because you can also avoid this libibverbs bug by installing the libibverbs_usnic "no no" plugin, available from https://github.com/cisco/libusnic_verbs or in binary form from cisco.com]))

    AS_IF([test "$with_verbs_usnic" = "yes"],
          [common_verbs_usnic_happy=1])

    AS_IF([test $common_verbs_usnic_happy -eq 1],
          [OPAL_CHECK_OPENFABRICS([common_verbs_usnic],
                                  [common_verbs_usnic_happy=1],
                                  [common_verbs_usnic_happy=0])
          ])

    AC_DEFINE_UNQUOTED([OPAL_COMMON_VERBS_USNIC_HAPPY],
                       [$common_verbs_usnic_happy],
                       [Whether the common/usnic_verbs component is being built or not])

    AS_IF([test $common_verbs_usnic_happy -eq 1],
          [$1],
          [$2])

    # substitute in the things needed to build openib
    AC_SUBST([common_verbs_usnic_CPPFLAGS])
    AC_SUBST([common_verbs_usnic_LDFLAGS])
    AC_SUBST([common_verbs_usnic_LIBS])
])dnl
