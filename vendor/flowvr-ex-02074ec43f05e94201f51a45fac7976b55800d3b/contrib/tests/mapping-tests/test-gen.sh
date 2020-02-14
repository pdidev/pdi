#! /bin/sh

# generate reference results  for tests
test.sh   test1 primes_all1.csv 0 -gen;
test.sh   test2 primes_all1.csv 1 -gen;
test.sh   test3 primes_all1.csv 2 -gen;
test.sh   test4 primes_all1.csv 3 -gen;
test.sh   test5 primes_all1.csv 4 -gen;
test.sh   test6 primes_all1.csv 5 -gen;
test.sh   test7 primes_parallel.csv 4 -gen;
test.sh   test8 primes_parallel.csv 5 -gen;
test.sh   test9 -L 0 -gen;
test.sh   test10 -L 1 -gen;
test.sh   test11 -L 2 -gen;
test.sh   test12 -L 3 -gen;
test.sh   test13 -L 4 -gen;
test.sh   test14 -L 5 -gen;
#test.sh   test15 primes_localhost0.csv 4 -gen;
test.sh   test16 primes_localhost1.csv 4 -gen;
test.sh   test17 primes_mapfilter0.csv 4 -gen;
#test.sh   test18 primes_mapmodule0.csv 4 -gen;
#test.sh   test19 primes_map0.csv 4 -gen;
#test.sh   test20 primes_multiplesingleton0.csv 0 -gen;
test.sh   test21 primes_propagecomposite0.csv 6 -gen;
test.sh   test22 primes_signaland.csv 8 -gen;
test.sh   test23 primes_parallelfromport.csv 9 -gen;
