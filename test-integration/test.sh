#!/bin/bash

RED='\033[1;31m'
YELLOW='\033[0;33m'
GREEN='\033[1;32m'
NC='\033[0m'

cabal build

cd test-integration/tests
tests=`ls *.go | sed 's/\.go$//'`
cd ../..

# TODO : Independent testing

for test in $tests; do
    cat test-integration/tests/${test}.go | cabal run --verbose=0 > test-integration/tests/${test}.res
    
    diff test-integration/tests/${test}.res test-integration/tests/${test}.out &>/dev/null
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}[Test \"${test}\"] - OK${NC}"
    else
        cat -An test-integration/tests/${test}.out >test-integration/tests/${test}.out.show
        cat -An test-integration/tests/${test}.res >test-integration/tests/${test}.res.show
        
        echo -e "${RED}[Test \"${test}\"] - FAIL${NC}\n"
        echo -e "${YELLOW}Expected:${NC}\n${YELLOW}Actual:${NC}\n" | xargs -d '\n' printf '%-68s  %-68s\n'
        diff -y -W 120 test-integration/tests/${test}.out.show test-integration/tests/${test}.res.show
        
        rm test-integration/tests/*.res
        rm test-integration/tests/*.show
        
        exit 1
    fi
done

rm test-integration/tests/*.res

exit 0
