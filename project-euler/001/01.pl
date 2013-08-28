#!/usr/bin/perl 

# If we list all the natural numbers below 10 that are multiples
# of 3 or 5, we get 3, 5, 6 and 9. 
# The sum of these multiples is 23.
#
# Find the sum of all the multiples of 3 or 5 below 1000.

my $sum = 0;
my $max = 999;
my $n   = 0;

sub filterFun{
my $num = shift;
 if( ($num % 5 == 0) or ($num % 3 == 0) ){
 $sum += $num;
 #print "$num,";
 }
}

foreach(1 .. $max){
filterFun($_);

}

print "\n$sum\n";
