#! /bin/csh -f
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

set srcdir="`pwd`"
set distdir="$srcdir/$1"
set OMPI_VERSION="$2"
set OMPI_SVN_VERSION="$3"

if ("$distdir" == "") then
    echo "Must supply relative distdir as argv[1] -- aborting"
    exit 1
elif ("$OMPI_VERSION" == "") then
    echo "Must supply version as argv[2] -- aborting"
    exit 1
endif

set svn_r=
if (-d .svn) then
    set svn_r="`svnversion .`"
endif

set start=`date`
cat <<EOF
 
Creating Open MPI distribution
In directory: `pwd`
Version: $OMPI_VERSION
Started: $start
 
EOF

umask 022

#########################################################
# VERY IMPORTANT: Now go into the new distribution tree #
#########################################################

if (! -d "$distdir") then
    echo "*** ERROR: dist dir does not exist"
    echo "*** ERROR:   $distdir"
    exit 1
endif
cd "$distdir"
echo "*** Now in distdir: $distdir"

#
# See if we need VERSION.svn
#

if ("$svn_r" != "") then
    sed -e 's/^svn_r=.*/svn_r='$svn_r'/' VERSION > version.new
    cp version.new VERSION
    rm -f version.new
    echo "*** Updated VERSION file with SVN r number"
else
    echo "*** Did NOT updated VERSION file with SVN r number"
endif

#
# Get the latest config.guess and config.sub from ftp.gnu.org
#

echo "*** Downloading latest config.sub/config.guess from ftp.gnu.org..."
cd config
set configdir="`pwd`"
mkdir tmp.$$
cd tmp.$$
wget -t 1 -T 10 ftp://ftp.gnu.org/gnu/config/config.guess
wget -t 1 -T 10 ftp://ftp.gnu.org/gnu/config/config.sub

# Recently, ftp.gnu.org has had zero-legnth config.guess / config.sub
# files, which causes the automated nightly SVN snapshot tarball to
# fail to be made correctly.  This is a primitive attempt to fix that.
# If we got zero-length files from wget, use a config.guess /
# config.sub from a known location that is more recent than what ships
# in the current generation of auto* tools.

if (! -f config.guess || ! -s config.guess) then
    echo " - WARNING: Got BAD config.guess from ftp.gnu.org"
    echo " - WARNING: using included version"
else
    cp config.guess ..
endif
if (! -f config.sub || ! -s config.sub) then
    echo " - WARNING: Got BAD config.sub from ftp.gnu.org"
    echo " - WARNING: using known version"
else
    cp config.sub ..
endif
cd ..
rm -rf tmp.$$
cd ..


#
# Find all the config.guess/config.sub files, and replace them with
# the ones that we've downloaded
#

echo "*** Now in: `pwd`"
echo "*** Replacing config.sub/config.guess with latest from ftp.gnu.org..."
foreach file (config.guess config.sub)
    find src -name $file \
	-exec chmod +w {} \; \
	-exec cp -f $configdir/$file {} \; \
	-print
end


#
# Put in date/version number in man pages
# JMS don't have man pages yet -- this is a straight copy from LAM7
#

set ver="$OMPI_VERSION"
#echo "*** Updating version date/number in man pages"
#rm -f manfiles
#find man -type f | grep -v Makefile > manfiles

#set date="`date '+%B, %Y'`"
#cp $srcdir/config/doctext.nroff.def .
#foreach file (`cat manfiles` doctext.nroff.def)
#    sed -e "s/-RELEASEDATE-/$date/g" $file > foo
#    sed -e "s/-RELEASEVERSION-/$ver/g" foo > bar
#    rm -f $file # Needed 'cause automake makes hard links, not copies
#    mv bar $file
#    rm -f foo
#end
#rm -f manfiles

#
# Make all the man pages -- doctext needs to be in your path
# JMS: Don't have man pages yet; need to do this at some point
#

#
# Now we need to list all these generated man pages in the Makefile.am
# and Makefile.in in man/man3.  Ick!
# JMS: Will probably need to do this as well.  Sigh.
#

#echo "*** Frobbing Makefile.am and Makefile.in..."
#cd ../../man/man3
#set files="`ls MPI_*3 MPIO_*3 XMPI_*3 MPIL_*3`"

#
# This is unfortunately necessary because $files is too long to do a
# single sed search/replace.  Ugh.
# JMS: Will probably need to do this as well.  Sigh.
#

#echo "*** Adding man files to Makefile.in..."
#foreach file ($files)
#    set name_prefix="`echo $file | cut -c1-4`"
#    if ("$name_prefix" == "MPI_") then
#	set letter="`echo $file | cut -c5`"
#	set div="`expr $letter \> F`"
#	set line="generated_man_$div"
#    else
#	set line="generated_man_other"
#    endif
#    echo " - $file / $line"
#    foreach fix (Makefile.am Makefile.in)
#	sed -e "s/$line =/$line =$file /" $fix > $fix.new
#	chmod +w $fix
#	mv -f $fix.new $fix
#	chmod -w $fix
#    end
#end
#cd ../..


#
# Put the release version number in the README and INSTALL files
#

set files="README INSTALL"
echo "*** Updating version number in $files..."
foreach file ($files)
    echo " - Setting $file"
    if (-f $file) then
	sed -e "s/OMPI_VERSION/$ver/g" $file > bar
	mv -f bar $file
    endif
end

#
# All done
#

cat <<EOF
*** Open MPI version $ver distribution created
 
Started: $start
Ended:   `date`
 
EOF

