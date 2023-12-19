# project-cis5520: Capture the Mine

Name: Yang Dong, Cuong Nguyen
PennKeys: flankado, cuongnd

### General Description

Our project’s main aim is to create a networked Minesweeper game in Haskell that offers two-player modes. It comes with a twist compared to the classic Minesweeper: instead of avoiding mines, you’re out to uncover them by clicking. Each mine you successfully mark earns you a point and you get to keep your turn. If you click on an empty area, your turn gets passed to the other player. The player with the most points wins.

The game adds a surprising layer of strategy to the mix, which is why we’ve named it ’Capture the Mine!’

To make this happen, we’ll need to work on these main aspects: generating the game board, developing the game’s logic and gameplay, setting up networking (for the two-player mode). Plus, we’re planning to include graphics in the terminal.

In order to represent the game’s state, we’ll use State Monad to keep track of the scores. It will cover the game board, individual cells, and mines. We used the random number generator to randomly generate games everytime players start a new game. We’ll also take care of user input, check for win or loss conditions, and update the board accordingly. We’re also going to explore the world of concurrency in Haskell to create a networked version of Minesweeper that supports two players.

## File Overview: 

### Logic.hs and SinglePlayer.hs: 

Logic.hs module covers the basic logic of the game, including how boards, clues, and mines are generated given the size of the board, the wining conditions, and how exploring a location should behave during a game. We also used State Monad to keep track of the scores of individual players and current player's turn. Besides, we also defined colors inside this module to enable the coloring of the terminal. 

SinglePlayer.hs module has the logic of game loops and it contains a function that can be used to create and start a game. 

### Multiplayer.hs: 

The server uses socket programming and concurrency to manage simultaneous connections from players. It establishes a listening socket, accepts player connections, and coordinates game sessions, allowing players to take turns exploring a grid containing hidden mines. The game state is synchronized using mutable variables (MVar), and the server broadcasts updates to all connected clients through channels. The code incorporates functions for updating game state, printing messages to players, and handling player moves. Each player's actions, such as exploring grid locations, contribute to the game's progress. The server manages game termination conditions, such as a player winning or a draw, and appropriately communicates these outcomes to connected players. Additionally, the code includes error handling and comments for clarity.

### Parser.hs:

This module defines a parser module that implements basic parsing functionality. It includes a custom monadic type 'Parser' with instances for Functor, Applicative, and Alternative type classes. The module also imports modules such as Control.Applicative, Control.Monad, Data.Char, Logic, Test.QuickCheck, and Text.Read. Notable functions include 'satisfy' for character validation, 'char' for parsing specific characters, 'oneDigit' for parsing single digits as integers, 'sepBy' for parsing a sequence of elements separated by a delimiter, and 'locationParser' for parsing a custom data type 'Location' from a list of integers separated by spaces. The code makes use of do-notation for monadic composition. The implementation demonstrates a basic yet versatile parsing framework in Haskell. With this module, the game a identify whether the input of a user is valid or not. 

### BoardPrint.hs

This module is responsible for printing the Minesweeper game board with various styles and colors. It imports modules such as Data.List, Data.Maybe, Helpers, and Logic. The code includes functions for formatting and coloring text, defining the appearance of different statuses on the Minesweeper board (e.g., mines, unexplored tiles, clues), and displaying the game board matrix with a border. Notably, the 'coloredText' function applies ANSI color codes to the text, the 'tile' function generates formatted representations for different board statuses, and 'showMatrixWith' outputs the entire Minesweeper board with appropriate styling. The code also includes helper functions for centering text within a specified width and adding borders to the output. Overall, this module is focused on creating visually appealing and informative representations of Minesweeper game boards in the console.

### Helpers.hs

This module provides utility functions to support various operations in a Minesweeper game. The module includes functions for handling locations on the game board, creating and manipulating matrices, and performing boundary checks. Here's a brief overview of the key functions:

  - surrounding: This function takes the width, height, and a location (x, y) as input and returns a list of surrounding locations. It filters out locations that are outside the bounds of the specified width and height.
  - inBounds: This function checks if a given location (x, y) is within the bounds defined by the specified width and height.
  - matrixMaker: This helper function generates a matrix of dimensions width x height, where all elements are initialized to a given value.
  - replaceMatrixIndex: This helper function replaces the element at a specified location (x, y) in a matrix with a new element.
  - matrixMap: This function applies a given function to every element of a matrix, providing a new matrix as the result.

The module also imports functionality for unit testing using the HUnit and QuickCheck libraries, including assertions, test case definitions, and test execution. Overall, the "Helpers" module provides foundational functions to manage and manipulate game board-related data structures in the context of a Minesweeper game.

### Main.hs

The entry point for the executable is in [Main.hs](app/Main.hs). 

### Spec.hs

All of the test cases are in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.