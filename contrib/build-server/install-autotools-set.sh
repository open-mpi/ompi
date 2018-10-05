#!/bin/sh -f

#
# This script installs specific versions of a quad of (GNU autotools + flex)
# to be used on the OMPI build server to make official Open MPI tarballs.
# We put this into a script because if a human performs this manually,
# they will inevitably forget one of the manual steps (e.g., making the
# modulefile, or installing flex, or ...).
#

m4=1.4.16
ac=2.69
am=1.13.4
lt=2.4.2
flex=2.5.35

getgnu() {
    tarball=$1
    url_prefix=$2


}

doit() {
    tarball=$1
    url_prefix=$2
    url_suffix=$3
    directory=$4
    prefix=$5

    if test ! -r $HOME/local/$tarball; then
        echo "+++ Downloading $tarball"
        echo wget $url_prefix/$tarball$url_suffix -O $HOME/local/$tarball
        wget $url_prefix/$tarball$url_suffix -O $HOME/local/$tarball
    else
        echo "--- No need to download $tarball"
    fi
    status=$?

    if test ! -r $HOME/local/$tarball -o "$status" != "0"; then
        echo "--- Download of $tarball failed!"
        exit 1
    fi

    if test "`echo $tarball | egrep 'bz2$'`" != ""; then
        options=jxf
    elif test "`echo $tarball | egrep 'gz$'`" != ""; then
        options=zxf
    else
        echo "--- Don't know how to extract $tarball"
        exit 1
    fi

    echo "+++ Extracting $tarball"
    cd $prefix/src
    rm -rf $directory
    tar $options $HOME/local/$tarball
    if test ! -d $directory; then
        echo "--- Extraction failed!"
        exit 1
    fi

    echo "+++ Configuring"
    cd $directory
    ./configure --prefix=$prefix 2>&1 | tee config.out
    if test "$?" != "0" -o ! -f Makefile; then
        echo "--- configure failed!"
        exit 1
    fi

    echo "+++ Building / installing"
    make -j 4 install 2>&1 | tee make.out
    if test "$?" != "0"; then
        echo "--- make failed!"
        exit 1
    fi
}

#-----------------------------------------------------------------------------

# Make the base directory where this set of autotools will go

basedir=$HOME/local
thisdir=$basedir/autotools-$ac-$am-$lt-$m4
if test -d "$thisdir"; then
    echo "=== Removing pre-existing $thisdir..."
    rm -rf $thisdir
fi
mkdir -p $thisdir/src
cd $thisdir/src

#-----------------------------------------------------------------------------

# Add this directory to the PATH so that each tool will see its
# already-installed friends.

export PATH=$thisdir/bin:$PATH

#-----------------------------------------------------------------------------

# Install everything

doit m4-$m4.tar.bz2       ftp://ftp.gnu.org/gnu/m4       "" m4-$m4       $thisdir
doit autoconf-$ac.tar.gz  ftp://ftp.gnu.org/gnu/autoconf "" autoconf-$ac $thisdir
doit automake-$am.tar.gz  ftp://ftp.gnu.org/gnu/automake "" automake-$am $thisdir
doit libtool-$lt.tar.gz   ftp://ftp.gnu.org/gnu/libtool  "" libtool-$lt  $thisdir

doit flex-$flex.tar.bz2   http://prdownloads.sourceforge.net/flex '?download' flex-$flex $thisdir

#-----------------------------------------------------------------------------

# Make the modulefile

echo "+++ Creating modulefile"
moddir=$HOME/modules/autotools
mkdir -p $moddir

mod=$moddir/autotools-$ac-$am-$lt-$m4
rm -f $mod
cat > $mod <<EOF
#%Module 0.0 -*- tcl -*-
#
# Setup home-brew autotools

proc ModulesHelp { } {
    puts stderr "Load Autotools with versions:"
    puts stderr "\tm4 $m4"
    puts stderr "\tAutoconf $ac"
    puts stderr "\tAutomake $am"
    puts stderr "\tLibtool $lt"
    puts stderr "\tFlex $flex"
}

module-whatis  "Load Autotools (ac $ac, am $am, lt $lt, m4 $m4, and flex $flex)"

eval set  [ array get env HOME ]
set AUTOTOOLS "$thisdir"
setenv AUTOTOOLS \$AUTOTOOLS

prepend-path PATH \$AUTOTOOLS/bin
prepend-path MANPATH \$AUTOTOOLS/man
EOF

#-----------------------------------------------------------------------------

# All done!

echo "Autotools installed (AC $ac, AM $am, LT $lt, m4 $m4, Flex $flex)"
exit 0
