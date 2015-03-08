# Emacs Configuration Files

These are my Emacs and GNUS configuration files. I wrote these files slowly
borrowing useful things that I saw in Emacs configuration files of other
people and official GNU Emacs manuals.

The `.emacs` file defines function to search Internet with DuckDuckGo. It
generates such URLs so DuckDuckGo operates in HTML mode, without any
proprietary Java Script. Also the URLs include parameters to remove ads
(well, a sponsored link in results is not a big deal anyway). It also forces
DuckDuckGo work only via HTTPS.

Also, here is some automation to install all necessary packages and some
automation to compile/recompile SLIME (works smoothly with Quicklisp). If
you don't have local copy of Common Lisp Hyper Spec it can download it via
FTP, untar it, and make it work.

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

New shortcuts:

Shortcut           | Description
--------           | -----------
<kbd>C-c ,</kbd>   | go to beginning of buffer
<kbd>C-c .</kbd>   | go to end of buffer
<kbd>C-c c</kbd>   | comment region
<kbd>C-c u</kbd>   | uncomment region
<kbd>C-c r</kbd>   | revert current buffer
<kbd>C-c p</kbd>   | purge all buffers (except for 'basic')
<kbd>C-c s</kbd>   | search online with DuckDuckGo
<kbd>C-c g</kbd>   | upgrade all packages
<kbd>C-c M-h</kbd> | Haskell mode
<kbd>C-c M-j</kbd> | Cider jack in
<kbd>C-c M-l</kbd> | SLIME
<kbd>C-c M-s</kbd> | run Scheme
<kbd>C-c b</kbd>   | byte compile initialization file
<kbd>C-c e</kbd>   | open initialization file
<kbd>C-c t</kbd>   | open org agenda file
<kbd>C-c a</kbd>   | org agenda (week)
<kbd>C-c i</kbd>   | correct word before point
<kbd>M-p</kbd>     | transpose line up
<kbd>M-n</kbd>     | transpose line down
<kbd>M-g</kbd>     | magit status
<kbd>F5</kbd>      | find file
<kbd>F6</kbd>      | Dired in other window
<kbd>F8</kbd>      | toggle Russian input
<kbd>C-c r</kbd>   | SLIME: restart inferior Lisp
<kbd>C-c h</kbd>   | SLIME: Hyper Spec lookup
<kbd>C-c i</kdb>   | SLIME: load ASDF system switch to package
<kbd>C-c h</kbd>   | Haskell: Hoogle query
<kbd>C-c C-l</kbd> | C mode: compile project
<kbd>M-]</kbd>     | Calendar: forward month
<kbd>M-[</kbd>     | Calendar: backward month

## Aliases

Use them with <kbd>M-x</kbd>.

Alias | Original command
----- | ----------------
`cl`  | `calendar`
`lp`  | `list-packages`
`qr`  | `query-replace`
`sh`  | `shell`

## License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL, version 3.
