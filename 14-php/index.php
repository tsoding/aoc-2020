<?php

function make_xmask($mask_str) {
    $result = 0;
    $n = strlen($mask_str);
    for ($i = 0; $i < $n; ++$i) {
        if ($mask_str[$i] == 'X') {
            $result = ($result << 1) | 1;
        } else {
            $result = ($result << 1) | 0;
        }
    }
    return $result;
}

function make_smask($mask_str) {
    $result = 0;
    $n = strlen($mask_str);
    for ($i = 0; $i < $n; ++$i) {
        if ($mask_str[$i] == '1') {
            $result = ($result << 1) | 1;
        } else {
            $result = ($result << 1) | 0;
        }
    }
    return $result;
}

function part_1($file_path) {
    $lines = explode("\n", file_get_contents($file_path));
    $xmask = 0;
    $smask = 0;
    $mem = array();
    foreach ($lines as $line) {
        if (strlen($line) > 0) {
            $op = explode("=", $line);
            $name = trim($op[0]);
            $arg = trim($op[1]);
            if ($name === "mask") {
                $xmask = make_xmask($arg);
                $smask = make_smask($arg);
            } else {
                $addr = intval(substr($name, 4, strlen($name) - 5));
                $value = intval($arg);
                $mem[$addr] = ($value & $xmask) | $smask;
            }
        }
    }

    $result = 0;
    foreach ($mem as $byte) {
        $result += $byte;
    }

    return $result;
}

function count_ones($mask) {
    $result = 0;
    while ($mask > 0) {
        if ($mask & 1 == 1) {
            $result += 1;
        }
        $mask = $mask >> 1;
    }
    return $result;
}

function spread_xmask($xmask, $x) {
    $rev_result = 0;

    // XX0XX0
    // 001101

    for ($i = 0; $i < 36; ++$i) {
        $rev_result = $rev_result << 1;

        if (($xmask & 1) == 1) {
            $rev_result = $rev_result | ($x & 1);
            $x = $x >> 1;
        }

        $xmask = $xmask >> 1;
    }

    $result = 0;
    for ($i = 0; $i < 36; ++$i) {
        $result = ($result << 1) | ($rev_result & 1);
        $rev_result = $rev_result >> 1;
    }

    return $result;
}

function part_2($file_path) {
    $lines = explode("\n", file_get_contents($file_path));
    $xmask = 0;
    $smask = 0;
    $n = 0;
    $mem = array();

    foreach ($lines as $line) {
        if (strlen($line) > 0) {
            $op = explode("=", $line);
            $name = trim($op[0]);
            $arg = trim($op[1]);
            if ($name === "mask") {
                $xmask = make_xmask($arg);
                $smask = make_smask($arg);
                $n = pow(2, count_ones($xmask));
            } else {
                $addr = intval(substr($name, 4, strlen($name) - 5));
                $value = intval($arg);
                $eaddr = ($addr | $smask) & (~$xmask);

                for ($x = 0; $x < $n; ++$x) {
                    $mem[$eaddr | spread_xmask($xmask, $x)] = $value;
                }
            }
        }
    }

    $result = 0;
    foreach ($mem as $byte) {
        $result += $byte;
    }

    return $result;
}

function solve_file($file_path) {
    echo("Input file: $file_path\n");
    echo("Part 1: " . part_1($file_path) . "\n");
    echo("Part 2: " . part_2($file_path) . "\n");
}

foreach (array_slice($argv, 1) as $file_path) {
    solve_file($file_path);
}
