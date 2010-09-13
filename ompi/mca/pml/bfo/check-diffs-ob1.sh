#!/bin/sh
#
# Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This scripts runs a diff between the ob1 and bfo files.  This
# allows us to quickly see the differences between the two and
# how well the bfo files are tracking ob1 as it changes.  You
# can also modify this, and run it from the csum directory.

CP=/bin/cp
MKDIR=/bin/mkdir
RM=/bin/rm
TOUCH=/bin/touch
pml=bfo
PML=BFO
ob1=ob1
OB1=OB1
DIFF=$ob1-$pml.diff
DIFFDIR=diff-dir

$MKDIR $DIFFDIR

# List of files that will be diffed.  Basically, everything
# in the directory.
FILES="pml_NAME.c \
       pml_NAME.h \
       pml_NAME_comm.c \
       pml_NAME_comm.h \
       pml_NAME_component.c \
       pml_NAME_component.h \
       pml_NAME_hdr.h \
       pml_NAME_iprobe.c \
       pml_NAME_irecv.c \
       pml_NAME_isend.c \
       pml_NAME_progress.c \
       pml_NAME_rdma.c \
       pml_NAME_rdma.h \
       pml_NAME_rdmafrag.c \
       pml_NAME_rdmafrag.h \
       pml_NAME_recvfrag.c \
       pml_NAME_recvfrag.h \
       pml_NAME_recvreq.c \
       pml_NAME_recvreq.h \
       pml_NAME_sendreq.c \
       pml_NAME_sendreq.h \
       pml_NAME_start.c"

# Copy over the files from the bfo directory.
for name in $FILES
do 
  $CP `echo $name | sed s/NAME/$pml/` $DIFFDIR
done

cd $DIFFDIR
# Convert the pml/PML strings back into ob1/OB1 strings
# to avoid spurious differences between the files.
../../../../../contrib/search_replace.pl $pml $ob1
../../../../../contrib/search_replace.pl $PML $OB1

# Copy over the files from the ob1 directory.
for name in $FILES
do 
  $CP ../../ob1/`echo $name | sed s/NAME/$ob1/` .
done

$RM -f $DIFF
$TOUCH $DIFF

# First, strip off the copyright header from all the files
# as we do not care about any differences in that area.
for name in $FILES
do
  ob1_file=`echo $name | sed s/NAME/$ob1/`
  awk '/\$HEADER/, /UPTOENDOFFILE/' $ob1_file > $ob1_file.tmp
  mv $ob1_file.tmp $ob1_file
  pml_file=`echo $name | sed s/NAME/$pml/`
  awk '/\$HEADER/, /UPTOENDOFFILE/' $pml_file > $pml_file.tmp
  mv $pml_file.tmp $pml_file
done

# Now run the diff.
for name in $FILES
do
  diff -c `echo $name | sed s/NAME/$ob1/` `echo $name | sed s/NAME/$pml/` >> $DIFF
  if [ "$?" -eq 0 ] ; then
    echo "No differences in `echo $name | sed s/NAME/$ob1/` file"
  fi
done

# Cleanup
mv $DIFF ..
cd ..
$RM -rf $DIFFDIR
