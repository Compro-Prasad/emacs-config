Compro's Emacs configuration
=

## Introduction

This is my Emacs configuration greatly inspired by the Emacs community itself.
It has been re-written many times and might change drastically in the future
too. For now I am using packages by knowing them properly and there are no
useless packages installed which I did install in the previous versions.


## Tree structure

	.mc-lists.el
	custom.el
	emacs-internals.el
	init.el
    README.md

### `.mc-lists.el`

This file stores information about what commands to run once and what commands
to run multiple times when using multiple cursors.

###  `custom.el`

All customizations made from the Emacs `customize` interface go in this file
which are loaded during startup.

### `emacs-internals.el`

All customizations that can be done without the help of packages are listed here.

### `init.el`

All bootstraping code and additional package configuration goes here.

### `README.md`

The file that introduces you to my configuration.
