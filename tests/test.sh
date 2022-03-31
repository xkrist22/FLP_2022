#!/bin/bash

GREEN=$'\e[0;32m'
RED=$'\e[0;31m'
NC=$'\e[0m'
FAILED="${RED}FAILED${NC}, diff output: "
PASSED="${GREEN}PASSED${NC}"
echo "=============== CFG2CNF test ==============="


# setup
mkdir res


# test if program parse CFG and prints it OK
./flp21-fun -i tests/1.in >res/1.res
DIFF=$(diff <(sort tests/1.in) <(sort res/1.res))
echo "Test 1: parsing correct CFG"
if [ "$DIFF" != "" ]; then
    echo $PASSED
else
    echo $FAILED
    echo $DIFF
fi
echo


# test if program removes easy rules and prints result OK
./flp21-fun -1 tests/2.in >res/2.res
DIFF=$(diff <(sort tests/2.out) <(sort res/2.res))
echo "Test 2: removing simple rules"
if [ "$DIFF" != "" ]; then
    echo $PASSED
else
    echo $FAILED
    echo $DIFF
fi
echo


# test BKG2CNF
./flp21-fun -i tests/1.in >res/3.res
DIFF=$(diff <(sort tests/1.out) <(sort res/3.res))
echo "Test 3: CFG to CNF"
if [ "$DIFF" != "" ]; then
    echo $PASSED
else
    echo $FAILED
    echo $DIFF
fi
echo


# test arguments parsing
./flp21-fun tests/3.in >res/4.res
DIFF=$(diff <(sort tests/3.out) <(sort res/4.res))
echo "Test 4: missing -i/-1/-2 arg init -2,"
echo "takes file and another BKG2CNF test"
if [ "$DIFF" != "" ]; then
    echo $PASSED
else
    echo $FAILED
    echo $DIFF
fi
echo


# teardown
rm -rf res


