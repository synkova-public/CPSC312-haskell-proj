# CPSC312-haskell-proj
Contributors: Phoenix Synkova, Sveta Sodol, Robert Desjardins
</p>Course: CPSC 312 2017W1

</p>Project Overview:
</p>Our goal was to make a version of Conway's Game of Life. We created an implementation with different starting states for the user to use it, while also aiming to make it a robust version because we are using user inputs to set specific parameters, and we did not want it to be easily corrupted by a person playing it.

</p>The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, alive or dead, or "populated" or "unpopulated". Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:

    Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any live cell with more than three live neighbours dies, as if by overpopulation.
    Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

Source: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life 
