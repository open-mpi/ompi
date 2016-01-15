#!/bin/sh
#
# Copyright (c) 2015      Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
# This script is used to measure PMIx performance.
#
# --exec: scenario to run as sync or "test1 test2"
# --mpidir: path to mpi installation (/usr default)
# --parse: path to collected results
# HOWTO:
# 1 .Set test matrix using variables $node_list, ppn_list, test_list
# 2. Allocate nodes:
#   $salloc --nodelist=node[1-4]
#   or
#   $salloc -N4
# 3. Launch script:
#   $./opmi-time.sh --exec="test1 test2" --mpidir=<open>
#   $./opmi-time.sh --mpidir=<open>
#
# Output location is test name folder
# Output file formats
# ()_base.log
# timestamp (usec)      hostnode label
# 1441715028369350	mir14    start
# 1441715030540656	mir14    end
#
# ()_out.log
# timestamp (usec)      rank    node
# 1441715030460727	0	mir9
# 1441715030460628	1	mir10
#
# ()_result.log
# time  rank    node
# 2.089	3	mir12
# 2.093	2	mir11
#
# report.log
# nodes ppn     mintime maxtime
# 4	1	2.089	2.093


# Settings
###############################################################################

node_list=(2 4)
ppn_list=(1 2)
test_list="test1 test2 test3 test4 test5 test6 test7 test8 test9 test10 test11 test12 test13"

# Declarations
###############################################################################

prefix=pmix
module=${BASH_SOURCE[0]}

# Command line parsing
###############################################################################

opt=""
while [ "$#" -gt 0 ]; do
  case "$1" in

    --parse=*) parse="${1#*=}"; shift 1;;
    --exec=*) exec="${1#*=}"; shift 1;;
    --mpidir=*) mpidir="${1#*=}"; shift 1;;
    --parse|--exec|--mpidir) echo "$1 requires an argument" >&2; exit 1;;

    -*) echo "unknown option: $1" >&2; exit 1;;
    *) shift 1;;
  esac
done

# The scenario of measurement
if [ -n "$exec" ]; then
  test_list="$exec"
fi

# The mpi path
mpidir=${mpidir:=/usr}

# Functions
###############################################################################

# format text
function do_format() {
  local is_format=true
  if [[ $is_format == true ]] ; then
    res=""
    for ((i=2; i<=$#; i++)) ; do
      case "${!i}" in
        "bold" ) res="$res\e[1m" ;;
        "underline" ) res="$res\e[4m" ;;
        "reverse" ) res="$res\e[7m" ;;
        "red" ) res="$res\e[91m" ;;
        "green" ) res="$res\e[92m" ;;
        "yellow" ) res="$res\e[93m" ;;
      esac
    done
    echo -e "$res$1\e[0m"
  else
    echo "$1"
  fi
}

# print message
function do_msg() {
  echo -e "$*" 2>&1 | tee -a $logfile
}

# print error message and exit script
function do_err() {
  echo -e $(do_format "$module failed. aborting. $*" "red" "bold") 2>&1 | tee -a $logfile
  exit 1
}

# print the seconds and current microseconds.
function do_timestamp() {
  do_msg "$(($(date +%s%N)/1000))\t$(hostname -s)" "$1"
}

# swap two files
function do_fswap() {
  if (( $# == 2 )); then
    mv "$1" /tmp/
    mv "$2" "`dirname $1`"
    mv "/tmp/`basename $1`" "`dirname $2`"
  else
    echo "Usage: swap <file1> <file2>"
    return 1
  fi
}

function do_cmd() {
  cmd="$*"
  do_msg "Doing:"
  do_msg "=================================================="
  do_msg "$*"
  eval $cmd >> $logfile 2>&1
  local status=$?
  if test "$status" != "0"; then
    echo "$module failed.  Log:"
	tail -20 $logfile
    cat $logfile
    exit $status
  fi
  do_msg "DONE"
  do_msg ""
}

function do_export() {
  do_msg "Exporting PATHs:"
  do_msg "=================================================="
  do_msg "$1"
  export PATH="$1/bin:${PATH}"
  export LD_LIBRARY_PATH="$1/lib:${LD_LIBRARY_PATH}"
  export MANPATH="$1/share/man:${MANPATH}"
  do_msg "DONE"
  do_msg ""
}

function do_nodeinfo() {
  do_msg "Node information:"
  do_msg "=================================================="
  do_msg $(hostname)
  do_msg $(cat /etc/issue | grep We)
  do_msg $(cat /proc/cpuinfo | grep 'model name' | sort -u | awk '{print $4, $5, $6, $7, $9}')
  do_msg $(cat /proc/cpuinfo | grep proce | wc | awk '{print $1}')
  do_msg $(uname -a | awk '{print $12}')
  do_msg $(cat /proc/meminfo | grep [M,m]em)
  do_msg $(uname -a | awk '{print $3}')
  do_msg $(ibstat | grep -e "CA type" -e "Firmware version")
  do_msg $(ibstatus | grep -e rate -e state | grep -v 'phys state')
  do_msg $(ofed_info | head -6 | grep OFED)
  do_msg "DONE"
  do_msg ""
}

function do_validate() {
  command -v mpiexec >/dev/null 2>&1 || { do_err "mpiexec is not found."; }
  command -v srun >/dev/null 2>&1 || { do_err "srun is not found."; }
  command -v salloc >/dev/null 2>&1 || { do_err "salloc is not found."; }
}

function do_check_pmix() {
  eval "srun --mpi=list 2>&1 | grep pmix"
}

function do_checksync_mpisync() {
  local status
  local tooldir=${tempdir}/mpisync
  local verbose=$1
  local option=$*

  do_msg "Checking synchronization using mpisync:"

  if [ ! -e ${tooldir} ]; then
    mkdir -p ${tooldir}
    cd ${tooldir}
    wget --no-check-certificate https://github.com/open-mpi/ompi/raw/master/ompi/tools/mpisync/mpigclock.c >> $logfile 2>&1
    wget --no-check-certificate https://github.com/open-mpi/ompi/raw/master/ompi/tools/mpisync/mpigclock.h >> $logfile 2>&1
    wget --no-check-certificate https://github.com/open-mpi/ompi/raw/master/ompi/tools/mpisync/hpctimer.c >> $logfile 2>&1
    wget --no-check-certificate https://github.com/open-mpi/ompi/raw/master/ompi/tools/mpisync/hpctimer.h >> $logfile 2>&1
    wget --no-check-certificate https://github.com/open-mpi/ompi/raw/master/ompi/tools/mpisync/sync.c >> $logfile 2>&1
    mpicc hpctimer.c mpigclock.c sync.c -o mpisync >> $logfile 2>&1
  fi
  if [ ! -e "$tooldir" ] || [ ! -f "$tooldir/mpisync" ]; then
    do_err "can not find $tooldir/mpisync"
  fi
  mpiexec -n $(($nodes)) -npernode 1 $mpioptions $tooldir/mpisync -o ${syncfile} ${option} 2>&1
  do_msg "Analysing ${syncfile}"
  cat ${syncfile} >> $logfile 2>&1
  diff=$(grep -v '^#' ${syncfile} | cut -f3 -d' ' | sort -n | awk 'BEGIN {min=1000000; max=0;}; { if($1<min && $1 != "") min = $1; if($1>max && $1 != "") max = $1; } END { printf("%0.06f %0.06f %0.06f", min, max, max-min) }') >> $logfile 2>&1
  do_msg "sync drift is equal: $diff"
  diff=`echo $diff | cut -f3 -d' '`
  status=$(if (( `bc <<< "$diff >= 0.001"` == 1 )); then echo "value $diff >= 0.001"; fi)
  if [ -n "$status" ] && [ -n $verbose -a "$verbose" == "on" ]; then
    do_err "mpisync reports issue with synchronization as $status"
  else
    do_msg "Warning: mpiperf reports issue with synchronization as $status"
  fi

  do_msg "DONE"
  do_msg ""
}

function do_checksync_mpiperf() {
  local status
  local tooldir=${tempdir}/mpiperf-0.0.3
  local verbose=$1

  do_msg "Checking synchronization using mpiperf:"

  if [ ! -f ${tempdir}/mpiperf-0.0.3.tar.gz ]; then
    wget http://mpiperf.cpct.sibsutis.ru/uploads/Main/mpiperf-0.0.3.tar.gz >> $logfile 2>&1
    tar zxvf mpiperf-0.0.3.tar.gz >> $logfile 2>&1
    cd $tooldir
    make >> $logfile 2>&1
  fi
  if [ ! -e "$tooldir" ] || [ ! -f "$tooldir/src/mpiperf" ]; then
    do_err "can not find $tooldir/src/mpiperf"
  fi
  mpiexec -n 1 $mpioptions $tooldir/src/mpiperf -T >> $logfile 2>&1
  if [ -z "$(mpiexec -n 1 $mpioptions $tooldir/src/mpiperf -j -t gettimeofday 2>&1 | tee -a $logfile | sed -n '/PASSED/p')" ]; then
    do_err "mpiperf does not support gettimeofday"
  fi
  mpiexec -n $(($nodes)) -npernode 1 $mpioptions $tooldir/src/mpiperf -t gettimeofday WaitPatternNull >> ${syncfile} 2>&1
  do_msg "Analysing ${syncfile}"
  cat ${syncfile} >> $logfile 2>&1
  status=$(grep -v '^#' ${syncfile} | awk -F ' ' '{ print $6 }' | while read i; do if (( `bc <<< "$i >= 1"` == 1 )); then echo "value $i >= 1.00"; break; fi; done)
  if [ -n "$status" ] && [ -n $verbose -a "$verbose" == "on" ]; then
    do_err "mpiperf reports issue with synchronization as $status"
  else
    do_msg "Warning: mpiperf reports issue with synchronization as $status"
  fi

  do_msg "DONE"
  do_msg ""
}

# $1 - sync filename
# $2 - verbose mode: on - exit in case synchronization values exceed a treshold and off - silent mode (default: off)
# $3+ - application additional options
function do_checksync() {
  if [ -z "$1" ]; then
    syncfile=${tempdir}/mpisync.log
  else
    syncfile=$1
  fi

  do_checksync_mpisync $2 "-a 0"
#  do_checksync_mpisync $2 "-a 1"
#  do_checksync_mpiperf
  do_msg "syncfile: $syncfile"
}

function do_analysis() {
  local testdir=$1
  local basefile=$2
  local outfile=$3
  local outfile1="${3}.1"
  local resultfile=${testdir}/${nodes}x${ppn}_result.log

  if [ ! -e $tesdir ]; then
    do_err "can not find testdir: $testdir"
  fi 
  if [ -z $basefile -o ! -f $basefile ]; then
    do_err "can not find basefile: $basefile"
  fi 
  if [ -z $outfile -o ! -f $outfile ]; then
    do_err "can not find outfile: $outfile"
  fi
  if [ "$(cat $outfile | wc -l)" != "$(($nodes * $ppn))" ]; then
    do_msg "Warning: number of lines in $outfile ($(cat $outfile | wc -l)) is not equal ($nodes * $ppn)."
  fi
  start_t=`awk -F $'\t' '{ if (NR == 1) print $1 }' $basefile`

  # Add sync value in output file
  while read line; do
    if [[ ! $line =~ ^[0-9] ]]; then
      do_msg "Warning: ignoring line: $line."
      continue
    fi
    local n=$(echo $line | cut -f3 -d' ')
    local v1=$(echo $line | cut -f1 -d' ')
    local v2=0

    if [ ! -z $syncfile -o -f $syncfile ]; then
      v2=$(echo "scale=2; ($(grep $n $syncfile | cut -f3 -d' ') * 1000000)" | bc -l)
      # Round float value to int
      v2=$(echo ${v2%%.*})
      v2=${v2:=0}
    fi
    echo -e "$(($v1 + $v2))\t${v2}\t${line}" >> $outfile1
  done < $outfile

  # Find maximum and minimum lines
  min_line=`sort -n $outfile1 | head -n1`
  max_line=`sort -n $outfile1 | tail -n1`
  if [ -z "$min_line" -o -z "$max_line" ]; then
    do_err "can not find max/min lines in : $outfile1"
  fi 
  min_t=$( echo "$min_line" | cut -f1 -d$'\t')
  max_t=$( echo "$max_line" | cut -f1 -d$'\t')
  echo -e "`bc -l <<< "scale=3; (($min_t - $start_t) / 1000000)"`\t`echo "$min_line" | cut -f4 -d$'\t'`\t`echo "$min_line" | cut -f5 -d$'\t'`" >> $resultfile 2>&1
  echo -e "`bc -l <<< "scale=3; (($max_t - $start_t) / 1000000)"`\t`echo "$max_line" | cut -f4 -d$'\t'`\t`echo "$max_line" | cut -f5 -d$'\t'`" >> $resultfile 2>&1

  echo -e "\n# Used synchronization file: $syncfile" >> $outfile1

  do_report $testdir $resultfile
}

function do_report() {
  local testdir=$1
  local resultfile=$2
  local reportfile=${testdir}/report.log

  if [ -z $resultfile -o ! -f $resultfile ]; then
    do_err "can not find resultfile: $resultfile"
  fi 
  min_t=`awk -F $'\t' '{ if (NR == 1) print $1 }' $resultfile`
  max_t=`awk -F $'\t' '{ if (NR == 2) print $1 }' $resultfile`
  echo -e "${nodes}\t${ppn}\t${min_t}\t${max_t}" >> $reportfile 2>&1
}

function do_postresult() {
  cd $tempdir/..
  tar -zcvf $PWD/pmix.$$.tar.gz $tempdir > /dev/null 2>&1
}

include_timestamp_func=$(cat <<END_MSG
#include <dlfcn.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/time.h>

static inline void timestamp(FILE *file)
{
    struct timeval tv;
    char name[256];
    char *host_name = NULL;
    char *domain = NULL;
    int procid = -1;
    char *str = NULL;

    gettimeofday(&tv, NULL); 

    if (gethostname(name, sizeof(name)) != 0)
        strcpy(name, "localhost");
    host_name = strdup(name);
    domain = strchr(host_name, '.');
    if (domain)
        *domain = '\0';

    str = getenv("SLURM_PROCID");
    procid = ( str ? strtol(str, NULL, 10) : -1);
    fprintf(file, "%lld\t%d\t%s\n", tv.tv_sec * 1000000LL + tv.tv_usec, procid, host_name);
    fflush(file);
}

END_MSG
)

function do_exec() {

# The number of nodes (see SLURM_NNODES)
nodes=${SLURM_NNODES}
nodes=${nodes:=2}

# The number of tasks per node (see SLURM_NTASKS_PER_NODE or SLURM_TASKS_PER_NODE)
ppn=${SLURM_NTASKS_PER_NODE}
ppn=${ppn:=1}

mpioptions=' -novm -mca btl_openib_warn_default_gid_prefix 0 -mca mpi_add_procs_cutoff 100000 '
slurmoptions=' OMPI_MCA_btl_openib_warn_default_gid_prefix=0 OMPI_MCA_mpi_add_procs_cutoff=100000 '

if [ -z "$(env | grep SLURM)" ]; then
  do_err "Do not see allocated nodes by SLURM. Probably salloc -N option is not set"
fi

#if [ "${SLURM_NPROCS}" != "$(($nodes * $ppn))" ]; then
#  do_err "SLURM_NPROCS=${SLURM_NPROCS} is not equal ($nodes * $ppn). Probably salloc -N option is not set"
#fi

do_msg ""
do_msg "Configuration:"
do_msg "=================================================="
do_msg "tempdir: $tempdir"
do_msg "logfile: $logfile"
do_msg "mpi: $mpidir"
do_msg "exec: $exec"
do_msg "nodes: $nodes"
do_msg "ppn: $ppn"
do_msg "mpioptions: $mpioptions"
do_msg "slurmoptions: $slurmoptions"
do_msg "node list: $node_list"
do_msg "ppn list: $ppn_list"
do_msg "test list: $test_list"
do_msg ""

do_export $mpidir
do_nodeinfo
do_validate

if [ -f "${tempdir}/mpisync.log" ]; then
  syncfile=${tempdir}/mpisync.log
  do_msg "found sync data at  ${syncfile}"
elif [ -f "${tempdir}/mpiperf.log" ]; then
  syncfile=${tempdir}/mpiperf.log
  do_msg "found sync data at  ${syncfile}"
else
  do_msg "sync data is not found"
fi

# Launch scenario
node_list_len=${#node_list[*]}
ppn_list_len=${#ppn_list[*]}

for ((i=0; $i < $node_list_len; i=$((i=$i+1)))); do
  for ((j=0; $j < $ppn_list_len; j=$((j=$j+1)))); do
    for test in $test_list; do
      nodes=${node_list[$i]}
      ppn=${ppn_list[$j]}
      if [ "$test" = "sync" ]; then
        do_checksync "${tempdir}/${nodes}x${ppn}_mpisync00.log" "off"
      else
        do_checksync "${tempdir}/${nodes}x${ppn}_mpisync_before.log" "off"
        eval "do_${test}"
        do_checksync "${tempdir}/${nodes}x${ppn}_mpisyn_after.log" "off"
      fi
    done
  done
done

do_postresult
}

# $1 - result location
function do_parse() {
  local parsedir=$1
  local result_list
  local test_list
  local parsefile

  for result in `ls -1 $workdir`; do
    if [ ! -d "${parsedir}/${result}" ]; then
      continue
    fi
    for test in `ls -1 "${parsedir}/${result}" | grep -v mpisync`; do
      if [ ! -d "${parsedir}/${result}/${test}" ]; then
        continue
      fi
      result_list="${result_list} ${result}"
      test_list="${test_list} ${test}"
    done
  done
    
  result_list=`echo $result_list | tr " " "\n" | sort | uniq | tr "\n" " "`
  test_list=`echo $test_list | tr " " "\n" | sort | uniq | tr "\n" " "`

  do_msg "results: $result_list"
  do_msg "tests: $test_list"

  for test in $test_list; do
    parsefile="${parsedir}/parse_${test}.log"
    for result in $result_list; do
       echo -e "\n${result}:" >> $parsefile 2>&1
       echo -e "nodes\tppn\tmin\tmax" >> $parsefile 2>&1
       cat "${parsedir}/${result}/${test}/report.log" >> $parsefile 2>&1
    done
  done
}

# Pure application srun launch
#####################################################
function do_test1
{
  local status
  local scenario=test1
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func

int main()
{
    timestamp(stdout);
    return 0;
}
END_MSG

  gcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  srun -n$(($nodes * $ppn)) -N$nodes --ntasks-per-node=$ppn ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "srun pure overhead" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# Pure application mpiexec launch
#####################################################
function do_test2
{
  local status
  local scenario=test2
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func

int main()
{
    timestamp(stdout);
    return 0;
}
END_MSG

  gcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  mpiexec -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "mpiexec pure overhead" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# Pure application oshrun launch
#####################################################
function do_test3
{
  local status
  local scenario=test3
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func

int main()
{
    timestamp(stdout);
    return 0;
}
END_MSG

  gcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  oshrun -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "osrun pure overhead" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# MPI_init application srun/pmi2 launch
#####################################################
function do_test4
{
  local status
  local scenario=test4
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "mpi.h"
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
    timestamp(stdout);
    MPI_Finalize();
    return 0;
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  env $slurmoptions srun -n$(($nodes * $ppn)) -N$nodes --ntasks-per-node=$ppn --mpi=pmi2 ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "srun --mpi=pmi2:MPI_Init" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# start_pes application srun/pmi2 launch
#####################################################
function do_test5
{
  local status
  local scenario=test5
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "shmem.h"
int main(int argc, char* argv[])
{
    start_pes(0);
    timestamp(stdout);
    return 0;
}
END_MSG

  oshcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  env $slurmoptions srun -n$(($nodes * $ppn)) -N$nodes --ntasks-per-node=$ppn --mpi=pmi2 ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "srun --mpi=pmi2:start_pes" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# MPI_Init application mpiexec launch
#####################################################
function do_test6
{
  local status
  local scenario=test6
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "mpi.h"
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
    timestamp(stdout);
    MPI_Finalize();
    return 0;
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  mpiexec -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "mpiexec:MPI_Init" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# start_pes application oshrun launch
#####################################################
function do_test7
{
  local status
  local scenario=test7
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "shmem.h"
int main(int argc, char* argv[])
{
    start_pes(0);
    timestamp(stdout);
    return 0;
}
END_MSG

  oshcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  oshrun -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "osrun:start_pes" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# pure application mpiexec:orte_daemon
#####################################################
function do_test8
{
  local status
  local scenario=test8
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
#include "mpi.h"
int main(int argc, char* argv[])
{
    return 0;
}
END_MSG

  cat > lib_$scenario.c <<END_MSG
#define _GNU_SOURCE
$include_timestamp_func

int orte_daemon(int argc, char *argv[])
{
    static int (*_orte_daemon)(int argc, char *argv[]) = NULL;

    if (!_orte_daemon) {
        _orte_daemon=dlsym(RTLD_NEXT,"orte_daemon");
        if (!_orte_daemon) {
            fprintf(stderr, "Error in 'dlsym': %s\n", dlerror());
            exit(1);
        } else {
            FILE *fd = NULL;
            char filename[1024];
            char *str = getenv("SLURM_PROCID");

            if (str) {
                sprintf(filename, "%s.%s", "$outfile", str);
                fd = fopen(filename, "a");
                if (fd) {
                    timestamp(fd);
                    fclose(fd);
                }
            }
        }
    }

    return _orte_daemon(argc, argv);
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1
  gcc lib_$scenario.c -o $scenario.so -shared -fPIC -ldl >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  LD_PRELOAD=$PWD/$scenario.so mpiexec -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  eval cat $outfile.* >> $outfile
  rm $outfile.*
  echo -e "mpiexec:orte_daemon" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# pure application oshrun:orte_daemon
#####################################################
function do_test9
{
  local status
  local scenario=test9
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
#include "mpi.h"
int main(int argc, char* argv[])
{
    return 0;
}
END_MSG

  cat > lib_$scenario.c <<END_MSG
#define _GNU_SOURCE
$include_timestamp_func

int orte_daemon(int argc, char *argv[])
{
    static int (*_orte_daemon)(int argc, char *argv[]) = NULL;

    if (!_orte_daemon) {
        _orte_daemon=dlsym(RTLD_NEXT,"orte_daemon");
        if (!_orte_daemon) {
            fprintf(stderr, "Error in 'dlsym': %s\n", dlerror());
            exit(1);
        } else {
            FILE *fd = NULL;
            char filename[1024];
            char *str = getenv("SLURM_PROCID");

            if (str) {
                sprintf(filename, "%s.%s", "$outfile", str);
                fd = fopen(filename, "a");
                if (fd) {
                    timestamp(fd);
                    fclose(fd);
                }
            }
        }
    }

    return _orte_daemon(argc, argv);
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1
  gcc lib_$scenario.c -o $scenario.so -shared -fPIC -ldl >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  LD_PRELOAD=$PWD/$scenario.so oshrun -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  eval cat $outfile.* >> $outfile
  rm $outfile.*
  echo -e "oshrun:orte_daemon" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# pure application mpiexec:orte_rml_base_update_contact_info
#####################################################
function do_test10
{
  local status
  local scenario=test10
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
#include "mpi.h"
int main(int argc, char* argv[])
{
    return 0;
}
END_MSG

  cat > lib_$scenario.c <<END_MSG
#define _GNU_SOURCE
$include_timestamp_func

int orte_rml_base_update_contact_info(void * data)
{
    static int (*_real_func)(void* data) = NULL;

    if (!_real_func) {
        _real_func=dlsym(RTLD_NEXT,"orte_rml_base_update_contact_info");
        if (!_real_func) {
            fprintf(stderr, "Error in 'dlsym': %s\n", dlerror());
            exit(1);
        } else {
            FILE *fd = NULL;
            char filename[1024];
            char *str = getenv("SLURM_PROCID");
 
            if (str) {
                sprintf(filename, "%s.%s", "$outfile", str);
                fd = fopen(filename, "a");
                if (fd) {
                    timestamp(fd);
                    fclose(fd);
                }
            }
        }
    }

    return _real_func(data);
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1
  mpicc lib_$scenario.c -o $scenario.so -shared -fPIC -ldl >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  LD_PRELOAD=$PWD/$scenario.so mpiexec -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  eval "cat $outfile.* >> $outfile" >> $logfile 2>&1
  rm $outfile.* >> $logfile 2>&1
  echo -e "mpiexec:orte_rml_base_update_contact_info" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# pure application oshrun:orte_rml_base_update_contact_info
#####################################################
function do_test11
{
  local status
  local scenario=test11
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
#include "shmem.h"
int main(int argc, char* argv[])
{
    return 0;
}
END_MSG

  cat > lib_$scenario.c <<END_MSG
#define _GNU_SOURCE
$include_timestamp_func

int orte_rml_base_update_contact_info(void * data)
{
    static int (*_real_func)(void* data) = NULL;

    if (!_real_func) {
        _real_func=dlsym(RTLD_NEXT,"orte_rml_base_update_contact_info");
        if (!_real_func) {
            fprintf(stderr, "Error in 'dlsym': %s\n", dlerror());
            exit(1);
        } else {
            FILE *fd = NULL;
            char filename[1024];
            char *str = getenv("SLURM_PROCID");
 
            if (str) {
                sprintf(filename, "%s.%s", "$outfile", str);
                fd = fopen(filename, "a");
                if (fd) {
                    timestamp(fd);
                    fclose(fd);
                }
            }
        }
    }

    return _real_func(data);
}
END_MSG

  oshcc $scenario.c -o $scenario.out >> $logfile 2>&1
  oshcc lib_$scenario.c -o $scenario.so -shared -fPIC -ldl >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  LD_PRELOAD=$PWD/$scenario.so oshrun -n $(($nodes * $ppn)) -npernode $ppn $mpioptions ./$scenario.out
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  eval "cat $outfile.* >> $outfile" >> $logfile 2>&1
  rm $outfile.* >> $logfile 2>&1
  echo -e "oshrun:orte_rml_base_update_contact_info" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# MPI_Init application mpiexec:srun/pmix
#####################################################
function do_test12
{
  local status
  local scenario=test12
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  do_check_pmix
  if [ $? -eq 0 ]; then
    do_msg "slurm has pmix plugin"
  else
    do_msg "skipping this test : slurm does not have pmix plugin"
    return 1
  fi

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "mpi.h"
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
    timestamp(stdout);
    MPI_Finalize();
    return 0;
}
END_MSG

  mpicc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  env $slurmoptions srun -n$(($nodes * $ppn)) -N$nodes --ntasks-per-node=$ppn --mpi=pmix ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "srun --mpi=pmix:MPI_Init" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# start_pes application oshrun:srun/pmix
#####################################################
function do_test13
{
  local status
  local scenario=test13
  local testdir=${tempdir}/$scenario
  local outfile=${testdir}/${nodes}x${ppn}_out.log
  local basefile=${testdir}/${nodes}x${ppn}_base.log

  do_msg "Running $scenario ${nodes}x${ppn} :"

  do_check_pmix
  if [ $? -eq 0 ]; then
    do_msg "slurm has pmix plugin"
  else
    do_msg "skipping this test : slurm does not have pmix plugin"
    return 1
  fi

  mkdir -p $testdir
  cd $testdir

  cat > $scenario.c <<END_MSG
$include_timestamp_func
#include "shmem.h"
int main(int argc, char* argv[])
{
    start_pes(0);
    timestamp(stdout);
    return 0;
}
END_MSG

  oshcc $scenario.c -o $scenario.out >> $logfile 2>&1

  # Do test
  do_timestamp "start" 2>&1 | tee -a $basefile
  env $slurmoptions srun -n$(($nodes * $ppn)) -N$nodes --ntasks-per-node=$ppn --mpi=pmix ./$scenario.out >> $outfile 2>&1
  test $? -eq 0 && status=OK || status=FAIL
  do_timestamp "end" 2>&1 | tee -a $basefile
  if [ "$status" == "FAIL" ]; then
    do_err "can not launch a test"
  fi

  echo -e "srun --mpi=pmix:start_pes" > ${testdir}/info.log 2>&1
  do_analysis $testdir $basefile $outfile

  do_msg "DONE"
}

# Main
###############################################################################


# Check if --exec option is passed ($exec is defined)
if test ${exec+defined}; then
  tempdir=$PWD/tmp/${prefix}.$$
  logfile=${tempdir}/${prefix}-time.log

  mkdir -p $tempdir
  rm -f $logfile
  cd $tempdir

  do_exec
fi

# Check if --parse option is passed ($parse is defined)
if test ${parse+defined}; then
  if [ -z "$parse" ]; then
    tempdir=$PWD/tmp
  else
    tempdir=$parse
  fi
  logfile=${tempdir}/${prefix}-parse.log

  mkdir -p $tempdir
  rm -f $logfile
  cd $tempdir

  do_parse "$tempdir"
fi

exit 0
