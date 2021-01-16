AC_DEFUN([OMPI_PRRTE_ADD_ARGS],[
    opal_show_subtitle "PRRTE Configuration options"

    AC_ARG_WITH([alps],
                [AC_HELP_STRING([--with-alps(=DIR|yes|no)],
                                [Build with ALPS scheduler component, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries (default: auto)])],[],with_alps=auto)
    AC_ARG_WITH([alps-libdir],
                [AC_HELP_STRING([--with-alps-libdir=DIR],
                                [Location of alps libraries (alpslli, alpsutil) (default: /usr/lib/alps (/opt/cray/xe-sysroot/default/user on eslogin nodes))])])

	AC_ARG_WITH([sge],
                [AC_HELP_STRING([--with-sge],
                               [Build SGE or Grid Engine support (default: no)])])

    AC_ARG_WITH([tm],
                [AC_HELP_STRING([--with-tm(=DIR)],
                                [Build TM (Torque, PBSPro, and compatible) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
                                
    AC_ARG_WITH([moab],
                [AC_HELP_STRING([--with-moab],
                                [Build MOAB scheduler component (default: yes)])])
    AC_ARG_WITH([moab-libdir],
                [AC_HELP_STRING([--with-moab-libdir=DIR],
                                [Search for Moab libraries in DIR])])

    AC_ARG_WITH([lsf],
                [AC_HELP_STRING([--with-lsf(=DIR)],
                                [Build LSF support])])
    AC_ARG_WITH([lsf-libdir],
                [AC_HELP_STRING([--with-lsf-libdir=DIR],
                                [Search for LSF libraries in DIR])])

    AC_ARG_WITH([slurm],
                [AC_HELP_STRING([--with-slurm],
                                [Build SLURM scheduler component (default: yes)])])

    AC_ARG_WITH([singularity],
                [AC_HELP_STRING([--with-singularity(=DIR)],
                                [Build support for the Singularity container, optionally adding DIR to the search path])])

    AC_ARG_WITH([prte-platform-patches-dir],
                [AC_HELP_STRING([--with-prte-platform-patches-dir=DIR],
                                [Location of the platform patches directory. If you use this option, you must also use --with-prte-platform.])])
    AC_ARG_WITH([prte-platform],
                [AC_HELP_STRING([--with-prte-platform=FILE],
                                [Load options for build from FILE.  Options on the
                                 command line not in FILE are used.  Options on the
                                 command line and in FILE are replaced by what is on the command line])])

    AC_ARG_ENABLE([prte-ft],
                  [AC_HELP_STRING([--enable-prte-ft],
                  [Enable PRRTE fault tolerance support (default: disabled)])])
])
