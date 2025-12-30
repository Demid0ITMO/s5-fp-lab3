  $ echo "1 2\n2 3" > input
  $ ../bin/main.exe --alg linear --step 0.5 < input
  linear > 1.5 2.5
  linear > 2 3

  $ echo "1 1\n2 4\n3 9\n4 16" > input   
  $ ../bin/main.exe --alg newton --step 0.5 -n 4 < input
  newton > 1.5 6.75
  newton > 2 9
  newton > 2.5 12.75
  newton > 3 21
  newton > 3.5 36.75
  newton > 4 63

  $ echo "0 0\n1 1\n2 4\n3 9" > input
  $ ../bin/main.exe --alg both --step 1 -n 3 < input
  linear > 1 1
  newton > 1 1
  linear > 2 4
  newton > 2 4
  linear > 3 9
  newton > 3 9
