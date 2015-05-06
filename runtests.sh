#! /bin/sh

check_result() {
    STATUS=$?
    TEST=$1
    if [ "$STATUS" != "0" ]; then
        echo "$TEST tests failed"
        exit $STATUS
    fi
}

cd haskell
cabal test
check_result "Haskell"
cd ..

cd python
python setup.py test
check_result "Python"
cd ..

cd php
phpunit --bootstrap autoload.php .
check_result "PHP"
cd ..

cd java
cd proton-utils
mvn clean install
cd ../proton
mvn clean install
check_result "Java"
cd ../..
