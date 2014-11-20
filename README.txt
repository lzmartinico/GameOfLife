################################################################################
Game of Life
--------------------------------------
© Lorenzo Martinico, Piotr Jander 2014
################################################################################

010    The `Game of Life' program,
001    well,
111    plays the game of life.

Overview
1. Installation.
2. Usage.
3. Capabilities.
4. Limitations.
5. Required packages.

#1 Installation

(This is for you as much as for Don, especially that compilation part ;) )

(Assuming all the required packages are installed.)

The package has a structure:

\GameOfLife
	\bin-win32
		...
	\patterns
		...
	\src
		Grid.hs
		Main.hs

So the package includes compiled binaries for 32-bit Windows, which can be used
straight away.

But if you want to use the program on a different platform, you need to
compile it.

Suppose you want to compile the program for Mac. Then
1. Create a folder "bin-mac" in "GameOfLife".
2. Go to "src".
3. Run

> ghc -o ../bin-mac/Life -outputdir ../bin-mac Main.hs

This will create the output files in "GameOfLife/bin-mac". They will include
an executable file "Life".

#2 Usage

To run the program, go to "bin-*", where * is your platform. Then run,
for example

> Life 10 vacuum-cleaner

This command will open a window in which the system's evolution will be
animated. The program will get the pattern from
"GameOfLife/patterns/vacuum-cleaner.rle" and animate it at the speed of
10 frames per second.
