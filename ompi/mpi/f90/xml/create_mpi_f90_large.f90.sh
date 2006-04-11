#! /bin/sh

XSL_PATH=.
OUT_PATH=../scripts

. functions_f90_large.list

if test "$1" != ""; then
    procedures=$1
fi

for proc in $procedures
do
  lc_proc=`echo ${proc}|tr '[A-Z]' '[a-z]'`
  echo "creating ${lc_proc}_f90.f90.sh"
  Xalan -p test_function "'${proc}'" -p interface_size "'large'" -o ${OUT_PATH}/${lc_proc}_f90.f90.sh ../xml/mpi.h.xml ${XSL_PATH}/chasm-mpi.f90.sh.xsl
  chmod 755 ../scripts/${lc_proc}_f90.f90.sh
done
