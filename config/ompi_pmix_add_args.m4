AC_DEFUN([OMPI_PMIX_ADD_ARGS],[
    opal_show_subtitle "PMIx Configuration options"

    AC_ARG_WITH([pmix-platform-patches-dir],
                [AC_HELP_STRING([--with-pmix-platform-patches-dir=DIR],
                                [Location of the platform patches directory. If you use this option, you must also use --with-pmix-platform.])])

    AC_ARG_WITH([pmix-platform],
                [AC_HELP_STRING([--with-pmix-platform=FILE],
                                [Load options for build from FILE.  Options on the
                                 command line not in FILE are used.  Options on the
                                 command line and in FILE are replaced by what is on the command line])])

	AC_ARG_WITH([jansson],
                [AC_HELP_STRING([--with-jansson(=DIR)],
                                [Build jansson support (default=no), optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([jansson-libdir],
                [AC_HELP_STRING([--with-jansson-libdir=DIR],
                                [Search for Jansson libraries in DIR])])

	AC_ARG_WITH([curl],
                [AC_HELP_STRING([--with-curl(=DIR)],
                                [Build curl support (default=no), optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([curl-libdir],
                [AC_HELP_STRING([--with-curl-libdir=DIR],
                                [Search for Curl libraries in DIR])])

   AC_ARG_WITH([zlib],
                [AC_HELP_STRING([--with-zlib=DIR],
                                [Search for zlib headers and libraries in DIR ])])
    AC_ARG_WITH([zlib-libdir],
                [AC_HELP_STRING([--with-zlib-libdir=DIR],
                                [Search for zlib libraries in DIR ])])

    AC_ARG_WITH([munge],
                [AC_HELP_STRING([--with-munge=DIR],
                                [Search for munge headers and libraries in DIR ])])
    AC_ARG_WITH([munge-libdir],
                [AC_HELP_STRING([--with-munge-libdir=DIR],
                                [Search for munge libraries in DIR ])])

    AC_ARG_ENABLE([dstore-pthlck],
                  [AC_HELP_STRING([--disable-dstore-pthlck],
                                  [Disable pthread-based locking in dstor (default: enabled)])])
])
