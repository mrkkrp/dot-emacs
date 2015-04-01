# Emacs Configuration Files

These are my Emacs and GNUS configuration files. I wrote these files slowly
borrowing useful things that I saw in Emacs configuration files of other
people and official GNU Emacs manuals. However, I've written quite a bit of
stuff myself, because I couldn't find decent solutions.

The `.emacs` file defines function to search Internet with DuckDuckGo. It
generates such URLs so DuckDuckGo operates in HTML mode, without any
proprietary Java Script. Also the URLs include parameters to remove ads
(well, a sponsored link in results is not a big deal anyway). It also forces
DuckDuckGo work only via HTTPS.

Also, here is some automation to install all necessary packages and some
automation to compile/recompile SLIME (works smoothly with Quicklisp).

I have written a function to upgrade all packages without displaying
`*Packages*` buffer. This function directly finds all obsolete packages,
asks if user want to upgrade and if he/she really wants to, install fresh
versions of packages automatically deleting obsolete versions.

To send emails with Emacs and read emails with GNUS, you need to create a
file called `.authinfo.gpg` in your home directory. As its extension
suggests, you better encrypt this sort of information. The file should have
the following form:

```
machine HOST port NUMBER login NAME password VALUE
```

You need two such lines: one for SMTP and another one for IMAP. For example,
I use:

```
machine smtp.openmailbox.org port 587 login myemail@opmbx.org password "foo"
machine imap.openmailbox.org port 993 login myemail@opmbx.org password "foo"
```

To use spell-checking you need to install `aspell` from repositories of your
favorite GNU/Linux distribution.

## Shortcuts

Don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. Prefer single keys and key sequences to
key-chords. Even if it's not faster, it's good for your health.

Shortcut           | Description
--------           | -----------
<kbd>C-c c</kbd>   | comment region
<kbd>C-c u</kbd>   | uncomment region
<kbd>C-c r</kbd>   | revert current buffer
<kbd>C-c p</kbd>   | purge all buffers (except for 'basic')
<kbd>C-c s</kbd>   | search online with DuckDuckGo
<kbd>C-c g</kbd>   | upgrade all packages
<kbd>C-c b</kbd>   | byte compile initialization file
<kbd>C-c e</kbd>   | open initialization file
<kbd>C-c t</kbd>   | open org agenda file
<kbd>C-c a</kbd>   | org agenda (week)
<kbd>C-c i</kbd>   | correct word before point
<kbd>M-p</kbd>     | transpose line up
<kbd>M-n</kbd>     | transpose line down
<kbd>F2</kbd>      | save buffer
<kbd>F5</kbd>      | find file (also Dired)
<kbd>F6</kbd>      | find file in other window (also Dired)
<kbd>F7</kbd>      | toggle French input method
<kbd>F8</kbd>      | toggle Russian input method
<kbd>F9</kbd>      | kill current buffer
<kbd>F10</kbd>     | delete other windows
<kbd>F11</kbd>     | switch to buffer
<kbd>F12</kbd>     | save buffers and kill terminal (exit)
<kbd>escape</kbd>  | delete window
<kbd>C-return</kbd>| duplicate line
<kbd>S-up</kbd>    | move buffer up
<kbd>S-down</kbd>  | move buffer down
<kbd>S-left</kbd>  | move buffer left
<kbd>S-right</kbd> | move buffer right
<kbd>menu ,</kbd>  | push mark
<kbd>menu .</kbd>  | jump to marked position without popping
<kbd>menu /</kbd>  | middle of the buffer
<kbd>menu <</kbd>  | beginning of the buffer
<kbd>menu ></kbd>  | end of the buffer
<kbd>menu a p</kbd>| apropos
<kbd>menu c a</kbd>| Calc
<kbd>menu c i</kbd>| Cider Jack-in
<kbd>menu c l</kbd>| Calendar
<kbd>menu d a</kbd>| show date in the minibuffer
<kbd>menu d i</kbd>| Diff
<kbd>menu e r</kbd>| ERC
<kbd>menu g d</kbd>| GDB
<kbd>menu g l</kbd>| goto line
<kbd>menu g n</kbd>| GNUS
<kbd>menu h r</kbd>| split window (horizontal)
<kbd>menu l b</kbd>| list buffers
<kbd>menu l i</kbd>| SLIME
<kbd>menu l p</kbd>| list packages
<kbd>menu m a</kbd>| magit status
<kbd>menu m n</kbd>| man
<kbd>menu q r</kbd>| query replace
<kbd>menu s c</kbd>| Scheme
<kbd>menu s h</kbd>| shell
<kbd>menu s l</kbd>| sort lines
<kbd>menu s s</kbd>| switch to scratch buffer
<kbd>menu s t</kbd>| insert date
<kbd>menu t e</kbd>| tetris
<kbd>menu v r</kbd>| split window (vertical)
<kbd>C-c r</kbd>   | SLIME: restart inferior Lisp
<kbd>C-c h</kbd>   | SLIME: Hyper Spec lookup
<kbd>C-c i</kdb>   | SLIME: load ASDF system switch to package
<kbd>C-c h</kbd>   | Haskell: Hoogle query
<kbd>C-c C-l</kbd> | C mode: compile project
<kbd>C-f</kbd>     | ido mode: next completion
<kbd>C-b</kbd>     | ido mode: previous completion
<kbd>b</kbd>       | Dired: up directory

## License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL, version 3.
