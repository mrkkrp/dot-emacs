# Emacs Configuration Files

What can I say… I'm productive like a devil now. Emacs is something that
encourages its users to think about their productivity and once you started
to search for ways to improve your workflow and efficiency — sky is the
limit.

## Features

Some minor features I have implemented (nothing special):

* automatic installation of all necessary packages;
* automation to compile/recompile SLIME (works smoothly with Quicklisp);
* transposition and duplication of lines;
* purging of buffers (except for "basic" ones);
* searching online with DuckDuckGo (you can use Google if you can tolerate
  spying);
* upgrading of all packages without displaying of `*Packages*` buffer;
* smart switching between default input method, French, and Russian;
* some mode line wizardly (can't use `smart-mode-line`, sorry);
* many little hacks that make everything work smoothly and look sexy.

## Visual

I use `solarized-dark` theme. It's the only theme I can use, seriously. My
font of choice is `Ubuntu Mono`, because it's easy to get on Arch Linux and
it supports all scripts that I need: Latin, Cyrillic, Greek, and who knows
what else (I like strange symbols), it's also quite pretty. (Use Infinality
if you use Arch Linux, your eyes deserve it.) I had been a big fan of
Inconsolata before I switched to Ubuntu Mono. The reason for switching is
that there is no normal version of the font supporting Cyrillic script (yes,
I've tried modifications à la Inconsolata LCG, they suck or I'm not
sufficiently dedicated person to make them look normally). I also enable
`hl-line-mode` in modes like `dired` for prettiness (see code).

## GNUS

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

If you do text editing professionally and you have no ergonomic keyboard,
get one. I use "Truly Ergonomic Keyboard" (no, they don't pay me for the
advertising). If you're using a laptop, remap <kbd>CapsLock</kbd> to
<kbd>Ctrl</kbd> now! Your pinky will thank you later.

Don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. Prefer single keys and key sequences to
key-chords. Even if it's not faster, it's better for your health.

I make use of <kbd>F</kbd> keys. There are not so many of them (I could bind
only 9 commands this way, since <kbd>F1</kbd>, <kbd>F3</kbd>, and
<kbd>F4</kbd> are already used by Emacs) so we have to use them
wisely. <kbd>F</kbd> keys are precious because single key pressing is the
most efficient thing you can do and on most keyboards you can easily reach
the keys.

Next, Emacs allows us to define some custom key bindings that start with
<kbd>C-c</kbd> prefix. I've assigned some commands this way. The bad thing
about these shortcuts is that you have to start them with a chord
<kbd>C-c</kbd>, while it's way better than <kbd>C-c C-o C-l</kbd> (such
shortcuts shouldn't be used at all!), I don't like to press several keys
simultaneously (unless such a combination is self-sufficient, like
<kbd>C-n</kbd>). Also, there are not so many combinations starting with this
common prefix, if we want to avoid too long key sequences.

Here key sequences starting with "introducing key" come into play. We can
choose single key, whose seul rôle will be starting key sequences. How long
should every such a key sequence be?  Of course we want it to be as short as
possible, but we cannot use only one key after the introducing key, because
total number of combinations won't be satisfactory. But we can use two keys
after introducing key (<kbd>menu</kbd> in my case, if you don't have
<kbd>menu</kbd> on your keyboard you can remap something with help of
specialized software), then we get 26 × 26 = 676 combinations! Not bad at
all. There are enough combinations for you to prefer those that have some
mnemonic value. Give this technique a try and you will see how productive
you can be!

Shortcut            | Description
--------            | -----------
<kbd>C-c c</kbd>    | expand abbreviation before point
<kbd>C-c r</kbd>    | revert current buffer
<kbd>C-c p</kbd>    | purge all buffers (except for 'basic')
<kbd>C-c s</kbd>    | search online with DuckDuckGo
<kbd>C-c g</kbd>    | upgrade all packages
<kbd>C-c b</kbd>    | byte compile initialization files
<kbd>C-c e</kbd>    | open initialization file
<kbd>C-c t</kbd>    | open org agenda file
<kbd>C-c a</kbd>    | org agenda (week)
<kbd>C-c i</kbd>    | correct word before point
<kbd>C-'</kbd>      | switch to other buffer
<kbd>M-p</kbd>      | transpose line up
<kbd>M-n</kbd>      | transpose line down
<kbd>F2</kbd>       | save buffer
<kbd>F5</kbd>       | find file (also Dired)
<kbd>F6</kbd>       | find file in other window (also Dired)
<kbd>F7</kbd>       | toggle French input method
<kbd>F8</kbd>       | toggle Russian input method
<kbd>F9</kbd>       | kill current buffer
<kbd>F10</kbd>      | delete other windows
<kbd>F11</kbd>      | switch to buffer
<kbd>F12</kbd>      | save buffers and kill terminal (exit)
<kbd>escape</kbd>   | delete window
<kbd>C-return</kbd> | duplicate line
<kbd>S-up</kbd>     | move buffer up
<kbd>S-down</kbd>   | move buffer down
<kbd>S-left</kbd>   | move buffer left
<kbd>S-right</kbd>  | move buffer right
<kbd>menu ,</kbd>   | push mark
<kbd>menu .</kbd>   | jump to marked position without popping
<kbd>menu /</kbd>   | middle of the buffer
<kbd>menu <</kbd>   | beginning of the buffer
<kbd>menu ></kbd>   | end of the buffer
<kbd>menu a b</kbd> | toggle abbrev mode
<kbd>menu a p</kbd> | apropos
<kbd>menu a r</kbd> | align regexp
<kbd>menu c a</kbd> | Calc
<kbd>menu c i</kbd> | Cider Jack-in
<kbd>menu c l</kbd> | Calendar
<kbd>menu c r</kbd> | copy rectangle
<kbd>menu c s</kbd> | set coding system
<kbd>menu d a</kbd> | show date in the minibuffer
<kbd>menu d i</kbd> | Diff
<kbd>menu e r</kbd> | ERC
<kbd>menu g d</kbd> | GDB
<kbd>menu g l</kbd> | goto line
<kbd>menu g n</kbd> | GNUS
<kbd>menu h r</kbd> | split window (horizontal)
<kbd>menu k r</kbd> | kill rectangle
<kbd>menu l b</kbd> | list buffers
<kbd>menu l i</kbd> | SLIME
<kbd>menu l p</kbd> | list packages
<kbd>menu m a</kbd> | magit status
<kbd>menu m n</kbd> | man
<kbd>menu q r</kbd> | query replace
<kbd>menu r n</kbd> | rectangle number lines
<kbd>menu s c</kbd> | Scheme
<kbd>menu s h</kbd> | shell
<kbd>menu s l</kbd> | sort lines
<kbd>menu s r</kbd> | string rectangle
<kbd>menu s s</kbd> | switch to scratch buffer
<kbd>menu s t</kbd> | insert date
<kbd>menu t e</kbd> | tetris
<kbd>menu v r</kbd> | split window (vertical)
<kbd>menu y r</kbd> | yank rectangle
<kbd>C-c r</kbd>    | SLIME: restart inferior Lisp
<kbd>C-c h</kbd>    | SLIME: Hyper Spec lookup
<kbd>C-c i</kdb>    | SLIME: load ASDF system switch to package
<kbd>C-c h</kbd>    | Haskell: Hoogle query
<kbd>C-c C-l</kbd>  | C mode: compile project
<kbd>C-f</kbd>      | ido mode: next completion
<kbd>C-b</kbd>      | ido mode: previous completion
<kbd>b</kbd>        | Dired: up directory
<kbd>z</kbd>        | Dired: change to WDired mode

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
