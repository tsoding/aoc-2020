#!/usr/bin/env perl

use strict;
use warnings;
use Math::Trig;

sub part_1($) {
    my ($file_path) = @_;

    my @dirs = (
        [1, 0],                 # east
        [0, 1],                 # north
        [-1, 0],                # west
        [0, -1]                 # south
    );

    my %names = (
        'E' => 0,
        'N' => 1,
        'W' => 2,
        'S' => 3
    );

    my %turns = (
        'L' => 1,
        'R' => -1
    );

    my $x = 0;
    my $y = 0;
    my $d = 0;
    
    open(FH, '<', $file_path) or die $!;
    while (<FH>) {
        if (m/([NSEWLRF])([0-9]+)/) {
            my $cmd = $1;
            my $arg = int($2);

            if ($cmd =~ m/[NSEW]/) {
                my $dx = @{$dirs[$names{$cmd}]}[0];
                my $dy = @{$dirs[$names{$cmd}]}[1];
                $x += $dx * $arg;
                $y += $dy * $arg;
            } elsif ($cmd =~ m/[LR]/) {
                $d = ($d + $turns{$cmd} * ($arg / 90)) % 4;
            } elsif ($cmd =~ m/[F]/) {
                my $dx = @{$dirs[$d]}[0];
                my $dy = @{$dirs[$d]}[1];
                $x += $dx * $arg;
                $y += $dy * $arg;
            }
        } else {
            die "Invalid command $_\n";
        }
    }
    close(FH);

    return abs($x) + abs($y);
}

sub part_2($) {
    my ($file_path) = @_;

    my %names = (
        'E' => [1, 0],
        'N' => [0, 1],
        'W' => [-1, 0],
        'S' => [0, -1]
    );

    my $ship_x = 0;
    my $ship_y = 0;
    my $wp_x = 10;
    my $wp_y = 1;
    
    open(FH, '<', $file_path) or die $!;
    # print("ship_x: $ship_x, ship_y: $ship_y, wp_x: $wp_x, wp_y: $wp_y\n");
    while (<FH>) {
        if (m/([NSEWLRF])([0-9]+)/) {
            my $cmd = $1;
            my $arg = int($2);
            if ($cmd =~ m/[NSEW]/) {
                $wp_x += $names{$cmd}->[0] * $arg;
                $wp_y += $names{$cmd}->[1] * $arg;
            } elsif ($cmd =~ m/[LR]/) {
                if ($cmd eq 'R') {
                    $arg = -$arg;
                }

                my $angle = $arg * pi() / 180.0;
                my $new_wp_x = cos($angle) * $wp_x - sin($angle) * $wp_y;
                my $new_wp_y = sin($angle) * $wp_x + cos($angle) * $wp_y;
                $wp_x = $new_wp_x;
                $wp_y = $new_wp_y;
            } elsif ($cmd =~ m/[F]/) {
                $ship_x += $wp_x * $arg;
                $ship_y += $wp_y * $arg;
            }
        } 
        # print("ship_x: $ship_x, ship_y: $ship_y, wp_x: $wp_x, wp_y: $wp_y\n");
    }
    close(FH);

    return abs($ship_x) + abs($ship_y);
}

sub solve_file($) {
    my ($file_path) = @_;
    print("Input file: $file_path\n");
    printf("Part 1: %d\n", part_1($file_path));
    printf("Part 2: %d\n", part_2($file_path));
}

for (@ARGV) {
    solve_file($_)
}
