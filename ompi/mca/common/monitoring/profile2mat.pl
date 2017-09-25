#!/usr/bin/perl -w

#
# Copyright (c) 2013-2015 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2013-2016 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Author Emmanuel Jeannot <emmanuel.jeannot@inria.fr>
#
# Take a profile file and aggregates all the recorded communicaton into matrices.
# It generated a matrices for teh number of messages, (msg),
# for the total bytes transmitted (size) and
# the average nulber of bytes per messages (avg)
#
# The output matix is symetric
#
# If possible it creates file with "internal" tags (collexctive and eta data),
# "external" tags (point to point messages)  and "all" (every messgaes).
#
# ensure that this script as the executable right: chmod +x ...
#


if($#ARGV < 0){
   die("Usage: $0 <\".prof\" filename>\n");
}else{
   $filename=$ARGV[0];
}

profile($filename,"I|E|S|R|C","all");
if ( profile($filename,"E","external") ){
    profile($filename,"I","internal");
    profile($filename,"S|R","osc");
    profile($filename,"C","coll");
}

sub profile{
   my $filename= $_[0];
   my $filter= $_[1];
   my $suffix= $_[2];
   my $done = 0;

   $outfile=$filename;
   $outfile=~s/\.prof$/_size_$suffix\.mat/;


   open IN,"<$filename";
   $n=0;
   @mat1=();
   @mat2=();
   @mat3=();
   $i=0;
   while (<IN>) {
      $i++;
      if (($f,$p1,$p2,$s,$m)=/^($filter)\s+(\d+)\s+(\d+)\s+(\d+)\D+(\d+)/){
	 $done = 1;
	 $f++;
	 #print "$p1 | $p2 | $s | $m\n";
	 $mat1[$p1][$p2]+=$s;
	 $mat1[$p2][$p1]+=$s;
	 $mat2[$p1][$p2]+=$m;
	 $mat2[$p2][$p1]+=$m;
	 $n=$p1 if ($p1>$n);
	 $n=$p2 if ($p2>$n);
      }else {
	 # print("file $filename line $i: $_\n");
      }
   }
   close IN;

   #print "$done\n";

   foreach $i (0..$n) {
      foreach $j (0..$n) {
	 $mat1[$i][$j]+=0;
	 $mat2[$i][$j]+=0;
	 $mat1[$i][$j]/=2;
	 $mat2[$i][$j]/=2;
	 if ($mat2[$i][$j]){
	    $mat3[$i][$j]=$mat1[$i][$j]/$mat2[$i][$j] ;
	    #printf"%f\t%f\t%f\n",$mat1[$i][$j],$mat2[$i][$j],$mat3[$i][$j];
	 }else{
	    $mat3[$i][$j]=0;
	 }
      }
   }


   if ($done) {
      print "$filename -> $suffix\n";
      save_file($outfile,$n,\@mat1);
      $outfile=~s/_size/_msg/;
      save_file($outfile,$n,\@mat2);
      $outfile=~s/_msg/_avg/;
      save_file($outfile,$n,\@mat3);
      print"\n";
   }
   return $done;
}


sub save_file{
   my $outfile=$_[0];
   my $n=$_[1];
   my @mat=@{$_[2]};
   $s=$n+1;
   print "$outfile\n";
   open OUT,">$outfile";
   foreach $i (0..$n) {
      foreach $j (0..$n) {
	 printf OUT "%.0f ",$mat[$i][$j];
      }
      print OUT "\n";
   }
 #  print"\n------------\n\n";
   close OUT;
}
