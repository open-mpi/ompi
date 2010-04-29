dnl -*- Autoconf -*-
dnl
dnl Copyright 2009 INRIA, Université Bordeaux 1
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright ©  2006-2010 Cisco Systems, Inc.  All rights reserved.

#-----------------------------------------------------------------------

# Probably only ever invoked by hwloc's configure.ac
AC_DEFUN([HWLOC_BUILD_STANDALONE],[
    hwloc_mode=standalone
])dnl

#-----------------------------------------------------------------------

# Probably only ever invoked by hwloc's configure.ac
AC_DEFUN([HWLOC_DEFINE_ARGS],[
    # Embedded mode, or standalone?
    AC_ARG_ENABLE([embedded-mode],
                    AC_HELP_STRING([--enable-embedded-mode],
                                   [Using --enable-embedded-mode puts the HWLOC into "embedded" mode.  The default is --disable-embedded-mode, meaning that the HWLOC is in "standalone" mode.]))

    # Change the symbol prefix?
    AC_ARG_WITH([hwloc-symbol-prefix],
                AC_HELP_STRING([--with-hwloc-symbol-prefix=STRING],
                               [STRING can be any valid C symbol name.  It will be prefixed to all public HWLOC symbols.  Default: "hwloc_"]))

    # Debug mode?
    AC_ARG_ENABLE([debug],
                  AC_HELP_STRING([--enable-debug],
                                 [Using --enable-debug enables various hwloc maintainer-level debugging controls.  This option is not recomended for end users.]))

    # Doxygen?
    AC_ARG_ENABLE([doxygen],
        [AC_HELP_STRING([--enable-doxygen],
                        [enable support for building Doxygen documentation (note that this option is ONLY relevant in developer builds; Doxygen documentation is pre-built for tarball builds and this option is therefore ignored)])])

    # Picky?
    AC_ARG_ENABLE(picky,
                  AC_HELP_STRING([--disable-picky],
                                 [When in developer checkouts of hwloc and compiling with gcc, the default is to enable maximum compiler pickyness.  Using --disable-picky or --enable-picky overrides any default setting]))

    # Cairo?
    AC_ARG_ENABLE([cairo],
                  AS_HELP_STRING([--disable-cairo], 
                                 [Disable the Cairo back-end of hwloc's lstopo command]))

    # XML?
    AC_ARG_ENABLE([xml],
                  AS_HELP_STRING([--disable-xml], 
		                 [Disable the XML back-end of hwloc's lstopo command]))
])dnl

#-----------------------------------------------------------------------

dnl We only build documentation if this is a developer checkout.
dnl Distribution tarballs just install pre-built docuemntation that was
dnl included in the tarball.

# Probably only ever invoked by hwloc's configure.ac
AC_DEFUN([HWLOC_SETUP_DOCS],[
    cat <<EOF

###
### Configuring hwloc documentation
###
EOF

    AC_MSG_CHECKING([if this is a developer build])
    AS_IF([test ! -d "$srcdir/.svn" -a ! -d "$srcdir/.hg" -a ! -d "$srcdir/.git"],
          [AC_MSG_RESULT([no (doxygen generation is optional)])],
          [AC_MSG_RESULT([yes])])
    
    # Generating the doxygen output requires a few tools.  If we
    # don't have all of them, refuse the build the docs.
    AC_ARG_VAR([DOXYGEN], [Location of the doxygen program (required for building the hwloc doxygen documentation)])
    AC_PATH_TOOL([DOXYGEN], [doxygen])
    HWLOC_DOXYGEN_VERSION=`doxygen --version 2> /dev/null`
    
    AC_ARG_VAR([PDFLATEX], [Location of the pdflatex program (required for building the hwloc doxygen documentation)])
    AC_PATH_TOOL([PDFLATEX], [pdflatex])
    
    AC_ARG_VAR([MAKEINDEX], [Location of the makeindex program (required for building the hwloc doxygen documentation)])
    AC_PATH_TOOL([MAKEINDEX], [makeindex])
    
    AC_ARG_VAR([FIG2DEV], [Location of the fig2dev program (required for building the hwloc doxygen documentation)])
    AC_PATH_TOOL([FIG2DEV], [fig2dev])
    
    AC_MSG_CHECKING([if can build doxygen docs])
    AS_IF([test "x$DOXYGEN" != "x" -a "x$PDFLATEX" != "x" -a "x$MAKEINDEX" != "x" -a "x$FIG2DEV" != "x"],
                 [hwloc_generate_doxs=yes], [hwloc_generate_doxs=no])
    AC_MSG_RESULT([$hwloc_generate_doxs])
    
    # Making the top-level README requires w3m or lynx.
    AC_ARG_VAR([W3M], [Location of the w3m program (required to building the top-level hwloc README file)])
    AC_PATH_TOOL([W3M], [w3m])
    AC_ARG_VAR([LYNX], [Location of the lynx program (required to building the top-level hwloc README file)])
    AC_PATH_TOOL([LYNX], [lynx])
    
    AC_MSG_CHECKING([if can build top-level README])
    AS_IF([test "x$W3M" != "x"],
          [hwloc_generate_readme=yes
           HWLOC_W3_GENERATOR=$W3M],
          [AS_IF([test "x$LYNX" != "x"],
                 [hwloc_generate_readme=yes
                  HWLOC_W3_GENERATOR="$LYNX -dump -nolist"],
                 [hwloc_generate_readme=no])])
    AC_SUBST(HWLOC_W3_GENERATOR)
    AC_MSG_RESULT([$hwloc_generate_readme])
    
    # If any one of the above tools is missing, we will refuse to make dist.
    AC_MSG_CHECKING([if will build doxygen docs])
    AS_IF([test "x$hwloc_generate_doxs" = "xyes" -a "x$enable_doxygen" != "xno"],
          [], [hwloc_generate_doxs=no])
    AC_MSG_RESULT([$hwloc_generate_doxs])
    
    # See if we want to install the doxygen docs
    AC_MSG_CHECKING([if will install doxygen docs])
    AS_IF([test "x$hwloc_generate_doxs" = "xyes" -o \
    	    -f "$srcdir/doc/doxygen-doc/man/man3/hwloc_distribute.3" -a \
    	    -f "$srcdir/doc/doxygen-doc/hwloc-a4.pdf" -a \
    	    -f "$srcdir/doc/doxygen-doc/hwloc-letter.pdf"],
          [hwloc_install_doxs=yes],
          [hwloc_install_doxs=no])
    AC_MSG_RESULT([$hwloc_install_doxs])
    
    # For the common developer case, if we're in a developer checkout and
    # using the GNU compilers, turn on maximum warnings unless
    # specifically disabled by the user.
    AC_MSG_CHECKING([whether to enable "picky" compiler mode])
    hwloc_want_picky=0
    AS_IF([test "$GCC" = "yes"],
          [AS_IF([test -d "$srcdir/.svn" -o -d "$srcdir/.hg"],
                 [hwloc_want_picky=1])])
    if test "$enable_picky" = "yes"; then
        if test "$GCC" = "yes"; then
            AC_MSG_RESULT([yes])
            hwloc_want_picky=1
        else
            AC_MSG_RESULT([no])
            AC_MSG_WARN([Warning: --enable-picky used, but is currently only defined for the GCC compiler set -- automatically disabled])
            hwloc_want_picky=0
        fi
    elif test "$enable_picky" = "no"; then
        AC_MSG_RESULT([no])
        hwloc_want_picky=0
    else
        if test "$hwloc_want_picky" = 1; then
            AC_MSG_RESULT([yes (default)])
        else
            AC_MSG_RESULT([no (default)])
        fi
    fi
    if test "$hwloc_want_picky" = 1; then
        add="-Wall -Wunused-parameter -Wundef -Wno-long-long -Wsign-compare"
        add="$add -Wmissing-prototypes -Wstrict-prototypes"
        add="$add -Wcomment -pedantic"

        CFLAGS="$CFLAGS $add"
    fi

    # Generate some files for the docs
    AC_CONFIG_FILES(
        hwloc_config_prefix[doc/Makefile]
        hwloc_config_prefix[doc/doxygen-config.cfg])
])

#-----------------------------------------------------------------------

# Probably only ever invoked by hwloc's configure.ac
AC_DEFUN([HWLOC_SETUP_UTILS],[
    cat <<EOF

###
### Configuring hwloc command line utilities
###
EOF

    hwloc_build_utils=yes

    # Cairo support
    if test "x$enable_cairo" != "xno"; then
      HWLOC_PKG_CHECK_MODULES([CAIRO], [cairo], [:], [enable_cairo="no"])
      if test "x$enable_cairo" != "xno"; then
        AC_PATH_XTRA
	CFLAGS_save=$CFLAGS
	LIBS_save=$LIBS

	CFLAGS="$CFLAGS $X_CFLAGS"
	LIBS="$LIBS $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"
        AC_CHECK_HEADERS([X11/Xlib.h], [
          AC_CHECK_HEADERS([X11/Xutil.h X11/keysym.h], [
            AC_CHECK_LIB([X11], [XOpenDisplay], [
              enable_X11=yes
              AC_SUBST([HWLOC_X11_LIBS], ["-lX11"])
              AC_DEFINE([HWLOC_HAVE_X11], [1], [Define to 1 if X11 libraries are available.])
            ])]
          )],,
          [[#include <X11/Xlib.h>]]
        )
        if test "x$enable_X11" != "xyes"; then
          AC_MSG_WARN([X11 headers not found, Cairo/X11 back-end disabled])
        fi

	CFLAGS=$CFLAGS_save
	LIBS=$LIBS_save
      fi
    fi
    
    if test "x$enable_cairo" != "xno"; then
        AC_DEFINE([HWLOC_HAVE_CAIRO], [1], [Define to 1 if you have the `cairo' library.])
    fi

    # XML support        
    
    if test "x$enable_xml" != "xno"; then
        HWLOC_PKG_CHECK_MODULES([XML], [libxml-2.0], [:], [enable_xml="no"])
    fi
    
    if test "x$enable_xml" != "xno"; then
        HWLOC_REQUIRES="libxml-2.0 $HWLOC_REQUIRES"
        AC_DEFINE([HWLOC_HAVE_XML], [1], [Define to 1 if you have the `xml' library.])
        AC_SUBST([HWLOC_HAVE_XML], [1])
        AC_CHECK_PROGS(XMLLINT, [xmllint])
    else
        AC_SUBST([HWLOC_HAVE_XML], [0])
    fi
    AC_SUBST(HWLOC_REQUIRES)
    HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_XML_CFLAGS"

    # Only generate this if we're building the utilities
    AC_CONFIG_FILES(
        hwloc_config_prefix[utils/Makefile]
        hwloc_config_prefix[hwloc.pc])
])dnl

#-----------------------------------------------------------------------

# Probably only ever invoked by hwloc's configure.ac
AC_DEFUN([HWLOC_SETUP_TESTS],[
    cat <<EOF

###
### Configuring hwloc tests
###
EOF

    hwloc_build_tests=yes

    # Only generate these files if we're making the tests
    AC_CONFIG_FILES(
        hwloc_config_prefix[tests/Makefile ]
        hwloc_config_prefix[tests/linux/Makefile]
        hwloc_config_prefix[tests/xml/Makefile]
        hwloc_config_prefix[tests/ports/Makefile]
        hwloc_config_prefix[tests/linux/gather-topology.sh]
        hwloc_config_prefix[tests/linux/test-topology.sh]
        hwloc_config_prefix[tests/xml/test-topology.sh]
        hwloc_config_prefix[utils/test-hwloc-distrib.sh])

    AC_CONFIG_COMMANDS([chmoding-scripts], [chmod +x ]hwloc_config_prefix[tests/linux/test-topology.sh ]hwloc_config_prefix[tests/xml/test-topology.sh ]hwloc_config_prefix[tests/linux/gather-topology.sh ]hwloc_config_prefix[utils/test-hwloc-distrib.sh])

    # These links are only needed in standalone mode.  It would
    # be nice to m4 foreach this somehow, but whenever I tried
    # it, I got obscure "invalid tag" errors from
    # AC_CONFIG_LINKS.  :-\ Since these tests are only run when
    # built in standalone mode, only generate them in
    # standalone mode.
    AC_CONFIG_LINKS(
        hwloc_config_prefix[tests/ports/topology.c]:hwloc_config_prefix[src/topology.c]
	hwloc_config_prefix[tests/ports/traversal.c]:hwloc_config_prefix[src/traversal.c]
	hwloc_config_prefix[tests/ports/topology-synthetic.c]:hwloc_config_prefix[src/topology-synthetic.c]
	hwloc_config_prefix[tests/ports/topology-solaris.c]:hwloc_config_prefix[src/topology-solaris.c]
	hwloc_config_prefix[tests/ports/topology-aix.c]:hwloc_config_prefix[src/topology-aix.c]
	hwloc_config_prefix[tests/ports/topology-osf.c]:hwloc_config_prefix[src/topology-osf.c]
	hwloc_config_prefix[tests/ports/topology-windows.c]:hwloc_config_prefix[src/topology-windows.c]
	hwloc_config_prefix[tests/ports/topology-darwin.c]:hwloc_config_prefix[src/topology-darwin.c]
	hwloc_config_prefix[tests/ports/topology-freebsd.c]:hwloc_config_prefix[src/topology-freebsd.c]
	hwloc_config_prefix[tests/ports/topology-hpux.c]:hwloc_config_prefix[src/topology-hpux.c])
    ])

	echo done setting up tests
])dnl
