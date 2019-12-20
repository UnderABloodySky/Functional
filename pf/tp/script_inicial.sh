cat A-small-practice.in | ghc -e main tic-tac-toe-tomek.hs > file.out

ghc -o main tic-tac-toe-tomek.hs && cat A-small-practice.in | ./main > file.out
