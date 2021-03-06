################################################################################
Game of Life
--------------------------------------
© Lorenzo Martinico, Piotr Jander 2014
################################################################################


Overview
1. Installation.
2. Usage.
3. Features.
4. Contributions

#1 Installation

Before attempting to compile the program, make sure that the packages
`matrix' and `gloss' are installed.

The package has a structure:

\GameOfLife
	\bin-*
		...
	\patterns
		...
	\src
		Grid.hs
		Main.hs

Suppose you want to compile the program for Windows. Then
1. Create a folder "bin-win32" in "GameOfLife".
2. Go to "src".
3. Run

> ghc -o ../bin-win32/Life -outputdir ../bin-win32 Main.hs

This will create the output files in "GameOfLife/bin-win32". They will include
an executable file "Life".

#2 Usage

To run the program, open "bin-*" in terminal, where * is your platform. Then run,
for example,

> Life 10 vacuum-cleaner

This command will open a window in which the system's evolution will be
animated. The program will get the pattern from
"GameOfLife/patterns/vacuum-cleaner.rle" and animate it at the speed of
10 frames per second. You can drag the canvas and zoom in/out with the mouse
to adjust the view.

You can also type

> Life patterns

to see a list of supplied patterns.

Once the display window is open, the user can drag around to move the viewport and use the mouse wheel to zoom in and out.

To close the current window and return to the command line, simply press ESC

#3 Features

The program can:

* read pattern data from RLE files.
* animate the evolution of a big system in real time.
* chaotically colours the grid; each time the program is run, it uses a random
  palette of colors from the file `palettes'.

It also has, however, some limitations:

* it can't animate very big patterns at a very high fps rate.
* it can only animate systems which shrink or whose size stays about the same;
  this is because the grid is represented as a matrix of fixed size.
  
  #4 Contribuitions
  
  While we have designed the enginge for converting RLE files to animated drawing,
  most of the example patterns included in our program are taken by the libraries of the Golly Game of Life simulator.
  The authors of each pattern are credited in the appropriate file.
  When no author is included, the code was written by us (but it's still based on "stock" game of life patterns).
