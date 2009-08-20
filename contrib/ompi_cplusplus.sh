#!/bin/sh
#
# Change all occurences of #if defined(c_plusplus) and variations
# to BEGIN_C_DECLS and END_C_DECLS using sed.
#
# List of files grepped are in XXX, sed script is in XXX
#
TMPDIR=${TMPDIR:-.}

while read LINE  ; do
  if test '#' = ${LINE:0:1} ; then
    continue
  fi

  FILE=${LINE%%:*}
  HAS_OPAL_CONFIG_H=0
  HAS_ORTE_CONFIG_H=0
  HAS_OMPI_CONFIG_H=0
  grep -q "#include \"opal_config.h\"" $FILE && HAS_OPAL_CONFIG_H=1
  grep -q "#include \"orte_config.h\"" $FILE && HAS_ORTE_CONFIG_H=1
  grep -q "#include \"ompi_config.h\"" $FILE && HAS_OMPI_CONFIG_H=1

  if test $HAS_OPAL_CONFIG_H -eq 0 -a \
          $HAS_ORTE_CONFIG_H -eq 0 -a \
          $HAS_OMPI_CONFIG_H -eq 0 ; then
    echo $FILE contains neither header
  fi
  sed -f contrib/ompi_cplusplus.sed $FILE > $TMPDIR/tmp_file.tmp
  mv $TMPDIR/tmp_file.tmp $FILE


  #
  # Sanity check
  #
  # grep -h -c BEGIN_C_DECLS $FILE | grep -v ':1$'
  # grep -h -c END_C_DECLS $FILE | grep -v ':1$'
done < contrib/ompi_cplusplus.txt
