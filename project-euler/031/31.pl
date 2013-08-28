#!/usr/bin/perl -w

use strict;

print p31(5,(5,2,1));

sub p31 {
my $tn = shift;
my @stack = @_;

if ($tn == 0){
	return 0;
}
#unless (@stack){
#	return 0;
#}


my $c  = pop @stack;
my $nc = $tn - $c;

if ($nc == 0) {
	return $c;
#	return ($c,p31($tn,@stack) );
}

if ($nc > 0) {
	return ($c,p31($nc,@_) );
}

}
