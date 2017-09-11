#!/bin/bash

#
# Copyright (c) 2016-2017 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Author Clément Foyer <clement.foyer@inria.fr>
#
# This script launches the test_overhead test case for 2, 4, 8, 12,
# 16, 20 and 24 processes, once with the monitoring component enabled,
# and once without any monitoring. It then parses and aggregates the
# results in order to create heatmaps. To work properly, this scripts
# needs sqlite3, sed, awk and gnuplot. It also needs the rights to
# write/create directories in the working path. Temporary files can be
# found in $resdir/.tmp. They are cleaned between two executions fo
# this script.
#
# This file create as an output one heatmap per operation
# tested. Currently, tested operations are :
#   - MPI_Send (software overhead)
#   - MPI_Send (ping-pong, to measure theoverhead with the communciation time)
#   - MPI_Bcast
#   - MPI_Alltoall
#   - MPI_Put
#   - MPI_Get
#

exe=test_overhead

# add common options
if [ $# -ge 1 ]
then
    mfile="-machinefile $1"
fi
common_opt="$mfile --bind-to core"

# dir
resdir=res
tmpdir=$resdir/.tmp
# files
base_nomon=$resdir/unmonitored
base_mon=$resdir/monitored
dbfile=$tmpdir/base.db
dbscript=$tmpdir/overhead.sql
plotfile=$tmpdir/plot.gp
# operations
ops=(send a2a bcast put get sendpp)

# no_monitoring(nb_nodes, exe_name, output_filename, error_filename)
function no_monitoring() {
    mpiexec -n $1 $common_opt --mca pml ^monitoring --mca osc ^monitoring --mca coll ^monitoring $2 2> $4 > $3
}

# monitoring(nb_nodes, exe_name, output_filename, error_filename)
function monitoring() {
    mpiexec -n $1 $common_opt --mca pml_monitoring_enable 1 --mca pml_monitoring_enable_output 3 --mca pml_monitoring_filename "prof/toto" $2 2> $4 > $3
}

# filter_output(filenames_list)
function filter_output() {
    for filename in "$@"
    do
        # remove extra texts from the output
        sed -i '/--------------------------------------------------------------------------/,/--------------------------------------------------------------------------/d' $filename
        # create all sub files as $tmpdir/$filename
        file=$(sed -e "s|$resdir/|$tmpdir/|" -e "s/\.dat/.csv/" <<< $filename)
        # split in file, one per kind of operation monitored
        awk "/^# MPI_Send/     {out=\"$(sed "s/\.$nbprocs/.send&/"   <<< $file)\"}; \
             /^# MPI_Bcast/    {out=\"$(sed "s/\.$nbprocs/.bcast&/"  <<< $file)\"}; \
             /^# MPI_Alltoall/ {out=\"$(sed "s/\.$nbprocs/.a2a&/"    <<< $file)\"}; \
             /^# MPI_Put/      {out=\"$(sed "s/\.$nbprocs/.put&/"    <<< $file)\"}; \
             /^# MPI_Get/      {out=\"$(sed "s/\.$nbprocs/.get&/"    <<< $file)\"}; \
             /^# MPI_Send_pp/  {out=\"$(sed "s/\.$nbprocs/.sendpp&/" <<< $file)\"}; \
            /^#/ { } ; !/^#/ {\$0=\"$nbprocs \"\$0; print > out};"                  \
            out=$tmpdir/tmp $filename
    done
    # trim spaces and replace them with comma in each file generated with awk
    for file in `ls $tmpdir/*.*.$nbprocs.csv`
    do
        sed -i 's/[[:space:]]\{1,\}/,/g' $file
    done
}

# clean previous execution if any
if [ -d $tmpdir ]
then
    rm -fr $tmpdir
fi
mkdir -p $tmpdir

# start creating the sql file for data post-processing
cat > $dbscript <<EOF
-- Enables mode Comma-Separated Values for input and output
.mode csv

-- Optionally enables column header display on output
.header off

-- Create one empty table for each of the input CSV files
EOF

for op in ${ops[*]}
do
    echo -e "drop table if exists ${op}_mon;\ndrop table if exists ${op}_nomon;" >> $dbscript
    echo -e "create table if not exists ${op}_mon (nbprocs integer, datasize integer, lat float, speed float, MBspeed float, media float, q1 float, q3 float, d1 float, d9 float, average float, maximum float, primary key (nbprocs, datasize) on conflict abort);\ncreate table if not exists ${op}_nomon (nbprocs integer, datasize integer, lat float, speed float, MBspeed float, media float, q1 float, q3 float, d1 float, d9 float, average float, maximum float, primary key (nbprocs, datasize) on conflict abort);" >> $dbscript
done

# main loop to launch benchmarks
for nbprocs in 2 4 8 12 16 20 24
do
    echo "$nbprocs procs..."
    output_nomon="$base_nomon.$nbprocs.dat"
    error_nomon="$base_nomon.$nbprocs.err"
    output_mon="$base_mon.$nbprocs.dat"
    error_mon="$base_mon.$nbprocs.err"
    # actually do the benchmarks
    no_monitoring $nbprocs $exe $output_nomon $error_nomon
    monitoring $nbprocs $exe $output_mon $error_mon
    # prepare data to insert them more easily into database
    filter_output $output_nomon $output_mon
    # insert into database
    echo -e "\n-- Import each CSV file in its corresponding table" >> $dbscript
    for op in ${ops[*]} 
    do
        echo -e ".import $(sed "s|$resdir/|$tmpdir/|" <<<$base_mon).${op}.${nbprocs}.csv ${op}_mon\n.import $(sed "s|$resdir/|$tmpdir/|" <<<$base_nomon).${op}.${nbprocs}.csv ${op}_nomon" >> $dbscript
    done
done

echo "Fetch data..."
echo -e "\n-- Perform some select query" >> $dbscript
for op in ${ops[*]}
do
    cat >> $dbscript <<EOF
-- set file output
.output $tmpdir/${op}.dat
-- do query for overheads
select ${op}_mon.nbprocs as nbprocs, ${op}_mon.datasize as datasize, ${op}_mon.media/${op}_nomon.media-1
from ${op}_mon inner join ${op}_nomon on (${op}_mon.nbprocs==${op}_nomon.nbprocs and ${op}_mon.datasize==${op}_nomon.datasize)
order by nbprocs, datasize;

EOF
done
cat >> $dbscript <<EOF
-- reset output to stdout
.output stdout
-- create view for all overheads
create temporary view medians as
select NULL as ovh
EOF
for op in ${ops[*]}
do
    cat >> $dbscript <<EOF
union all select all ${op}_mon.media / ${op}_nomon.media - 1 as ovh from ${op}_mon inner join ${op}_nomon on (${op}_mon.nbprocs == ${op}_nomon.nbprocs and ${op}_mon.datasize == ${op}_nomon.datasize)
EOF
done
cat >> $dbscript <<EOF
;

select '# average of overheads: ', avg(ovh) from medians where ovh is not null;
select '# median of overheads: ', avg(ovh) from (
        select ovh from medians
        where ovh is not null
        order by ovh
        limit 2 - (select count(*) from medians where ovh is not null) % 2
        offset(select (count(*) - 1) / 2 from medians where ovh is not null)
);

-- End of the script
.quit
EOF

# data post processing + create output files
sqlite3 $dbfile < $dbscript

# create plotting script
cat > $plotfile <<EOF
set terminal pngcairo
#set key inside bottom right box
set key bmargin box
set key off
#set grid
set view map
set pm3d
#set origin 2,0

set xlabel "Nb procs"
set ylabel "Message size (bytes)"
set cblabel "Monitoring overhead (overhead ratio)"
set cbtics format '%.1f'

set ytics ( "4 B" 4, "16 B" 16, "128 B" 128, "1 KB" 1024, "8 KB" 8*1024, "128 KB" 128*1024, "1 MB" 1024*1024 )
set xtics ( 2, 4, 8, 12, 16, 20, 24 ) 
set xrange [2:24]
#set cbrange [0:1]
set nologscale x
set logscale y 2

$(
for op in ${ops[*]} 
do 
    # finish to prepare the data files
    sed -i -e "s/,/ /g" $tmpdir/${op}.dat
    awk -F ' ' -v val=0 '{ if (val<$1) { val=$1 ; print " " > out ; print $0 > out } else { print $0 > out } }' out=$tmpdir/${op}.dat $tmpdir/${op}.dat
    echo -e "set output '$resdir/${op}.png'\nsplot '$tmpdir/${op}.dat' using (\$1):(\$2):(\$3) with pm3d"
done)
EOF

echo "Generating graphs..."

gnuplot < $plotfile

echo "Done."
