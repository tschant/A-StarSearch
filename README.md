A-StarSearch
============

Implementation of A* search algorithm in both Racket and Python.
Racket uses both a null heuristic and the Manhattan distance to solve an 8 puzzle.
Python uses only the Manhattan distance, but has user input to start each puzzle solution.

To start the racket program: open racket and hit run. 
To run all the test puzzle type in (test-all).
To run only one type (tile-puzzle test# '()).
  replace '#' with a number from 1 to 6
