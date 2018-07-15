Compro's Emacs configuration
---

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

All customization made from the Emacs `customize` interface go in this file
which are loaded during startup.

### `emacs-internals.el`

All customization that can be done without the help of packages are listed here.

### `init.el`

All bootstrapping code and additional package configuration goes here.

### `README.md`

The file that introduces you to my configuration.


## Packages

### [straight.el](https://github.com/raxod502/straight.el)
This is a replacement for the inbuilt **package.el** in Emacs. Basically it is a
package installer which clones actual packages and builds them for Emacs. It
still is advantageous for me because:
- Pulling changes is lighter on the bandwidth as compared to whole tarballs.
- Managing versions as an user is super easy.
- Forking and modifying the actual package becomes easy given **Git** is at
  your disposal.

### [use-package](https://github.com/jwiegley/use-package)
This makes package installation and configuring them a breeze. It is a macro
that wraps most of the tasks into basic key value pairs(which can also be lists
of tasks). Fortunately there are ways in which external package managers like
**straight.el** can introduce their functionalities into **use-package** so that
installing package is consistent across package managers.

### [hungry-delete](https://github.com/nflath/hungry-delete)
Hungry delete is the most satisfying thing that has a too easy shortcut. It
deletes all spaces between point and the last/first non-space character.

### [Magit](https://github.com/magit/magit)
The git playground that helps you learn and use **Git** in Emacs like never
before. Use `C-x g` to enter into the status buffer. Pressing `q` in status
buffer reveals the power of **Magit**.

### [magithub](https://github.com/vermiculus/magithub)
A plugin for **Magit** that interacts with GitHub to manage repositories, pull
requests and issues without any issues. When in status buffer use `H` to access
the **magithub** menu.

### [expand-region](https://github.com/magnars/expand-region.el)
A region selector that happens to use the code semantics around the point to
select a region in an iterative manner.
- `C-=` to expand region around the point
- `C-+` to contract region around the point

### [Projectile](https://github.com/bbatsov/projectile)
A project management tool for Emacs. Emacs now also ships with

- `project-find-file`
- `project-find-regexp`
- `project-or-external-find-file`
- `project-or-external-find-regexp`

which are basically sufficient to get the work done. But **Projectile** provides
tonnes of other features like:

- `C-c p k`:   Killing a project's buffers
- `C-c p p`:   Switch to a project
- `C-c p b`:   Switch to a buffer in the current project
- `C-c p f`:   Find to a file in current project
- `C-c p !`:   Invoke a shell command from project's root
- `C-c p c`:   Build and run your project
- `C-c p s s`: Search in your project
- `C-c p e`:   Find a recently visited file
- `C-c p a`:   Switch between similar files(Ex - .h and .c)

and many more which I don't use. The searching functionality I mentioned is
provided by [ag](https://github.com/Wilfred/ag.el).

### [switch-window](https://github.com/dimitri/switch-window)
Switching windows is pain using just `C-x o`. Thus this package solves this
problem very well. Press `M-\` to switch windows. When there are more than two
windows it numbers them. Pressing that number switches you to that window.
