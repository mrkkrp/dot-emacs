# ⌨ Emacs Configuration Files

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/dot-emacs.svg?branch=master)](https://travis-ci.org/mrkkrp/dot-emacs)

*This is my personal configuration, that means it's highly experimental. It
 requires Emacs trunk to work because it uses features that are missing in
 released versions of Emacs. If you would like to try this, clone Emacs
 repository and build it yourself.*

Emacs is something that encourages its users to think about their
productivity and once you started to search for ways to improve your
workflow and efficiency — sky is the limit.

* [Packages](#packages)
* [Features](#features)
* [Key Remapping](#key-remapping)
* [Sticky Keys](#sticky-keys)
* [Modal Editing](#modal-editing)
* [Key Bindings](#key-bindings)
* [Keyboard Layouts and Abbreviations](#keyboard-layouts-and-abbreviations)
* [GNUS](#gnus)
* [Appearance](#appearance)
* [License](#license)

## Packages

Here is collection of packages that I use (in alphabetical order):

Package/Repo | Description
------------ | -----------
[ace-link](https://github.com/abo-abo/ace-link) |  Quickly follow links
[ace-popup-menu](https://github.com/mrkkrp/ace-popup-menu) † | Replace GUI popup menus
[ace-window](https://github.com/abo-abo/ace-window) | Quickly switch windows
[aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode) | Keep code always indented
[auctex](http://git.savannah.gnu.org/cgit/emacs/elpa.git/?h=externals/auctex) | Integrated environment for *TeX*
[avy](https://github.com/abo-abo/avy) | Move cursor effectively
[cider](https://github.com/clojure-emacs/cider) | Clojure IDE
[common-lisp-snippets](https://github.com/mrkkrp/common-lisp-snippets) † | Yasnippets for Common Lisp
[cyphejor](https://github.com/mrkkrp/cyphejor) † | Shorten names of major modes
[ebal](https://github.com/mrkkrp/ebal) † | Emacs interface to Cabal
[f](https://github.com/rejeep/f.el) | Modern API for working with files and dirs
[fix-word](https://github.com/mrkkrp/fix-word) † | Convenient word transformation
[flycheck](https://github.com/flycheck/flycheck) | On-the-fly syntax checking
[flycheck-color-mode-line](https://github.com/flycheck/flycheck-color-mode-line) | Colorize mode line according to Flycheck status
[flycheck-haskell](https://github.com/flycheck/flycheck-haskell) | Flycheck: Cabal projects and sandboxes
[ghc](https://github.com/kazu-yamamoto/ghc-mod) | Improve Haskell REPL experience
[gitignore-mode](https://github.com/magit/git-modes) | Major mode for editing .gitignore files
[haskell-mode](https://github.com/haskell/haskell-mode) | A Haskell editing mode
[highlight-line](https://github.com/mrkkrp/highlight-line) † | Highlight lines in list-like buffers
[highlight-symbol](https://github.com/nschum/highlight-symbol.el) | Automatic and manual symbol highlighting
[hl-todo](https://github.com/tarsius/hl-todo) | Highlight TODO and similar keywords
[ido-hacks](https://github.com/scottjad/ido-hacks) | Put more IDO in your IDO
[ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) | Use IDO (nearly) everywhere
[ido-vertial-mode](https://github.com/creichert/ido-vertical-mode.el) | Makes IDO-mode display vertically
[js2-mode](https://github.com/mooz/js2-mode) | Improved JavaScript editing mode
[kill-or-bury-alive](https://github.com/mrkkrp/kill-or-bury-alive) † | Precise control over buffer killing in Emacs
[magit](https://github.com/magit/magit) | A Git porcelain inside Emacs
[markdown-mode](http://daringfireball.net/projects/markdown/) | Major mode for Markdown-formatted text files
[mk-abbrev](https://github.com/mrkkrp/mk-abbrev) † | Peculiar way to use Emacs abbrevs
[modalka](https://github.com/mrkkrp/modalka) † | Easily introduce native modal editing of your own design
[org](http://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/packages/org) | Outline-based template notes management
[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) | Highlight brackets according to their depth
[rich-minority](https://github.com/Malabarba/rich-minority) | Clean-up and beautify the list of minor-modes
[shakespeare-mode](https://github.com/CodyReichert/shakespeare-mode) | Support for Hamlet, Lucius, and Julius templates
[skewer-mode](https://github.com/skeeto/skewer-mode) | Live web development with Emacs
[slime](https://github.com/slime/slime) | Superior Lisp Interaction Mode for Emacs
[smart-mode-line](https://github.com/Malabarba/smart-mode-line) | A powerful and beautiful mode-line for Emacs
[smartparens](https://github.com/Fuco1/smartparens) | Tricks for working with all kinds of parenthesis
[smex](https://github.com/nonsequitur/smex) | M-x interface with IDO-style fuzzy matching
[solarized-theme](https://github.com/bbatsov/solarized-emacs) | The Solarized color theme
[speed-type](https://github.com/hagleitn/speed-type) | Practice touch typing in Emacs
[vimish-fold](https://github.com/mrkkrp/vimish-fold) † | Fold text like in Vim
[visual-regexp](https://github.com/benma/visual-regexp.el) | Regexp replace with interactive visual feedback
[whole-line-or-region](https://github.com/purcell/whole-line-or-region) | Operate on current line if region undefined
[yaml-mode](https://github.com/yoshiki/yaml-mode) | Major mode for editing YAML serialization format
[yasnippet](https://github.com/capitaomorte/yasnippet) | Yet another snippet extension for Emacs
[ztree](https://github.com/fourier/ztree) | Show directory structure as a tree
[zygospore](https://github.com/louiskottmann/zygospore.el) | Reversible version of `delete-other-windows`
[zzz-to-char](https://github.com/mrkkrp/zzz-to-char) † | Fancy version of `zap-to-char` command

† These packages I wrote myself, I can't be sure, but it seems like other
  people find them useful too.

Thanks to all Emacs developers, GNU team and all the people who hack Emacs
and publish their code so others can use it. The whole thing is something
you can contribute to, improve it and help other by doing so. This is truly
exiting.

## Features

Some minor features I have implemented (nothing special, interesting things
are usually published as separate packages, so people can benefit from my
hacking):

* automatic installation of all necessary packages (including packages from
  directly cloned git repositories);

* many peculiar editing primitives;

* searching online with DuckDuckGo (it also knows how to prefix my search
  query depending on major mode, so if I edit Python source, my query will
  start with `"python "` automatically, it's configurable thing);

* upgrading of all packages without displaying of `*Packages*` buffer;

* smart switching between default input method, French, and Russian.

## Key Remapping

If you do text editing professionally and you have no ergonomic keyboard,
get one. I use «Truly Ergonomic Keyboard» (no, they don't pay me for the
advertising… I think they should).

It's common knowledge that if you use a laptop, you should remap <kbd>⇪ Caps
Lock</kbd> to <kbd>⎈ Ctrl</kbd> (at least if you're an Emacs user). It's
undoubtedly true, but what about <kbd>↹ Tab</kbd> key and <kbd>⌫
Backspace</kbd>? There are three things to consider:

1. You need to press <kbd>⌫ Backspace</kbd> more often or just as often as
   <kbd>↹ Tab</kbd> (when editing code, in other cases you usually don't
   need <kbd>↹ Tab</kbd> at all).

2. On standard laptop keyboard <kbd>↹ Tab</kbd> occupies very comfortable
   position right above <kbd>⇪ Caps Lock</kbd>, while <kbd>⌫ Backspace</kbd>
   is far away and you need to *change* position of your hand a little to
   reach it.

3. There is no other way in Emacs to invoke `backward-delete-char` command,
   than via <kbd>⌫ Backspace</kbd>, there is no handy alternative key
   binding for it, while <kbd>↹ Tab</kbd> has comfortable alternative
   <kbd>C-i</kbd>.

So why not swap <kbd>↹ Tab</kbd> key and <kbd>⌫ Backspace</kbd>? If you're a
GNU/Linux user, here is `~/.Xmodmap` file that may be helpful:

```
! Laptop setting:
! First, make CapsLock third control:
remove Lock = Caps_Lock
remove Control = Control_L
keysym Caps_Lock = Control_L
add Control = Control_L
! Second, swap Tab and BackSpace:
keysym Tab = BackSpace
keysym BackSpace = Tab
```

I recommend changing shortcut for «window cycling», it's usually <kbd>⎇
Alt</kbd> + <kbd>↹ Tab</kbd>, now it should be <kbd>⎇ Alt</kbd> + <kbd>⌫
Backspace</kbd>, because this key combination is comfortable and shouldn't
change.

## Sticky Keys

I'm a big fan of «Sticky Keys» feature. Most major operating systems and
desktop environments provide this feature, it allows to press modifier keys,
such as <kbd>⎈ Ctrl</kbd>, <kbd>⎇ Alt</kbd>, and <kbd>⇧ Shift</kbd>
sequentially, instead of pressing multiple keys at a time — that's how all
computers should work by default. Anyway, for some commands I hold <kbd>⎈
Ctrl</kbd> (navigation commands and a few others), but there are not so many
of them.

If you ask yourself if I have RSI or some kind disability, I have to say
that if you care about your health, you don't need to wait until you have
RSI to optimize your interaction with computer.

## Modal Editing

Currently I'm using `modalka-mode` that allows to edit text in modal
fashion. Modal editing is fundamentally better for health and more efficient
in general. Initially I considered learning `evil` to edit in «vim-style»,
but I estimated resulting effect not superior to editing with familiar Emacs
key bindings for basic movement (given I don't know to learn something).

I'm using the following translation map for modal editing:

Input          | Result
-----          | ------
<kbd>SPC</kbd> | <kbd>C-SPC</kbd>
<kbd>,</kbd>   | <kbd>C-,</kbd>
<kbd>-</kbd>   | <kbd>C--</kbd>
<kbd>/</kbd>   | <kbd>C-/</kbd>
<kbd>:</kbd>   | <kbd>M-;</kbd>
<kbd>;</kbd>   | <kbd>C-;</kbd>
<kbd>?</kbd>   | <kbd>M-/</kbd>
<kbd>0</kbd>   | <kbd>C-0</kbd>
<kbd>1</kbd>   | <kbd>C-1</kbd>
<kbd>2</kbd>   | <kbd>C-2</kbd>
<kbd>3</kbd>   | <kbd>C-3</kbd>
<kbd>4</kbd>   | <kbd>C-4</kbd>
<kbd>5</kbd>   | <kbd>C-5</kbd>
<kbd>6</kbd>   | <kbd>C-6</kbd>
<kbd>7</kbd>   | <kbd>C-7</kbd>
<kbd>8</kbd>   | <kbd>C-8</kbd>
<kbd>9</kbd>   | <kbd>C-9</kbd>
<kbd>a</kbd>   | <kbd>C-a</kbd>
<kbd>b</kbd>   | <kbd>C-b</kbd>
<kbd>c c</kbd> | <kbd>C-c C-c</kbd>
<kbd>c k</kbd> | <kbd>C-c C-k</kbd>
<kbd>c v</kbd> | <kbd>C-c C-v</kbd>
<kbd>d</kbd>   | <kbd>C-d</kbd>
<kbd>e</kbd>   | <kbd>C-e</kbd>
<kbd>f</kbd>   | <kbd>C-f</kbd>
<kbd>g</kbd>   | <kbd>C-g</kbd>
<kbd>h</kbd>   | <kbd>M-h</kbd>
<kbd>i</kbd>   | <kbd>C-i</kbd>
<kbd>j</kbd>   | <kbd>M-j</kbd>
<kbd>k</kbd>   | <kbd>C-k</kbd>
<kbd>l</kbd>   | <kbd>C-l</kbd>
<kbd>m</kbd>   | <kbd>C-m</kbd>
<kbd>n</kbd>   | <kbd>C-n</kbd>
<kbd>o</kbd>   | <kbd>C-o</kbd>
<kbd>p</kbd>   | <kbd>C-p</kbd>
<kbd>q</kbd>   | <kbd>M-q</kbd>
<kbd>r</kbd>   | <kbd>C-r</kbd>
<kbd>s</kbd>   | <kbd>C-s</kbd>
<kbd>t</kbd>   | <kbd>C-t</kbd>
<kbd>u</kbd>   | <kbd>C-u</kbd>
<kbd>v</kbd>   | <kbd>C-v</kbd>
<kbd>w</kbd>   | <kbd>C-w</kbd>
<kbd>x ;</kbd> | <kbd>C-x C-;</kbd>
<kbd>x e</kbd> | <kbd>C-x C-e</kbd>
<kbd>x o</kbd> | <kbd>C-x C-o</kbd>
<kbd>y</kbd>   | <kbd>C-y</kbd>
<kbd>z</kbd>   | <kbd>M-z</kbd>
<kbd>A</kbd>   | <kbd>M-SPC</kbd>
<kbd>B</kbd>   | <kbd>M-b</kbd>
<kbd>C</kbd>   | <kbd>M-c</kbd>
<kbd>D</kbd>   | <kbd>M-d</kbd>
<kbd>E</kbd>   | <kbd>M-e</kbd>
<kbd>F</kbd>   | <kbd>M-f</kbd>
<kbd>G</kbd>   | <kbd>C-`</kbd>
<kbd>J</kbd>   | <kbd>C-]</kbd>
<kbd>K</kbd>   | <kbd>M-k</kbd>
<kbd>L</kbd>   | <kbd>M-l</kbd>
<kbd>M</kbd>   | <kbd>M-m</kbd>
<kbd>N</kbd>   | <kbd>M-n</kbd>
<kbd>O</kbd>   | <kbd>M-o</kbd>
<kbd>P</kbd>   | <kbd>M-p</kbd>
<kbd>R</kbd>   | <kbd>M-r</kbd>
<kbd>T</kbd>   | <kbd>M-t</kbd>
<kbd>U</kbd>   | <kbd>M-u</kbd>
<kbd>V</kbd>   | <kbd>M-v</kbd>
<kbd>W</kbd>   | <kbd>M-w</kbd>
<kbd>Y</kbd>   | <kbd>M-y</kbd>
<kbd>Z</kbd>   | <kbd>C-z</kbd>

## Key Bindings

I don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. I prefer single keys and key sequences over
key-chords. Even if it's not faster, it's better for health.

I make use of <kbd>F</kbd> keys. There are not so many of them (I could bind
only 9 commands this way, since <kbd>F1</kbd>, <kbd>F3</kbd>, and
<kbd>F4</kbd> are already used by Emacs) so we have to use them
wisely. <kbd>F</kbd> keys are precious because single key pressing is the
most efficient thing you can do and on most keyboards you can easily reach
the keys.

Next, Emacs allows us to define some custom key bindings: <kbd>C-c</kbd>
prefix followed by a single key. I've assigned some commands this way. The
bad thing about these shortcuts is that you have to start them with a chord
<kbd>C-c</kbd> (this paragraph is a bit obsolete I wrote it before I started
using «Sticky Keys»), while it's way better than <kbd>C-c C-o C-l</kbd>
(such shortcuts shouldn't be used at all!), I don't like to press several
keys simultaneously (unless such a combination is self-sufficient, like
<kbd>C-n</kbd>). Also, there are not so many combinations starting with this
common prefix, if we want to avoid too long key sequences.

Here key sequences starting with «introducing key» come into play. We can
choose single key, whose seul rôle will be starting key sequences. How long
should every such a key sequence be?  Of course, we want it to be as short
as possible, but we cannot use only one key after the introducing key,
because total number of combinations won't be satisfactory. But we can use
two keys after the introducing key (<kbd>▤ Menu</kbd> in my case, if you
don't have <kbd>▤ Menu</kbd> on your keyboard you can remap something with
help of specialized software), then we get 26 × 26 = 676 combinations! (In
practice you get even more because of punctuation and numbers.) Not bad at
all. There are enough combinations for us to prefer those that have some
mnemonic value. Give this technique a try and you will see how productive
you can be!

Shortcut            | Description
--------            | -----------
<kbd>C-'</kbd>      | switch to other buffer
<kbd>C-,</kbd>      | avy: goto char
<kbd>C-SPC</kbd>    | mark command (rectangular with prefix)
<kbd>C-]</kbd>      | cute indent command
<kbd>C-c C-o</kbd>  | find file at point (works for URLs too)
<kbd>C-c a</kbd>    | org agenda (week)
<kbd>C-c b</kbd>    | byte-compile initialization files
<kbd>C-c e</kbd>    | open directory with initialization files
<kbd>C-c h</kbd>    | lookup language specific documentation online
<kbd>C-c p</kbd>    | purge all buffers (except for 'basic')
<kbd>C-c r</kbd>    | revert current buffer (restart/reset REPL in some modes)
<kbd>C-c s</kbd>    | search online with DuckDuckGo
<kbd>C-c t</kbd>    | open org agenda file
<kbd>C-z</kbd>      | copy rest of the line
<kbd>M-c</kbd>      | fix word: capitalize
<kbd>M-e</kbd>      | replace last S-expression with its result
<kbd>M-h</kbd>      | mark word
<kbd>M-j</kbd>      | join the next line and the current one
<kbd>M-l</kbd>      | fix word: downcase
<kbd>M-n</kbd>      | transpose line down
<kbd>M-o</kbd>      | ace link
<kbd>M-p</kbd>      | transpose line up
<kbd>M-r</kbd>      | duplicate line
<kbd>M-u</kbd>      | fix word: upcase
<kbd>M-x</kbd>      | SMEX
<kbd>M-z</kbd>      | zzz up to char
<kbd>F2</kbd>       | save buffer
<kbd>F5</kbd>       | find file (also Dired)
<kbd>F6</kbd>       | find file in other window (also Dired)
<kbd>F7</kbd>       | toggle French input method
<kbd>F8</kbd>       | toggle Russian input method
<kbd>F9</kbd>       | kill or bury alive
<kbd>F10</kbd>      | delete other windows (reversible)
<kbd>F11</kbd>      | switch to buffer
<kbd>F12</kbd>      | exit Emacs
<kbd>escape</kbd>   | delete window
<kbd>return</kbd>   | Modalka mode
<kbd>menu menu</kbd>| smex (translation to <kbd>M-x</kbd>)
<kbd>menu ,</kbd>   | beginning of the buffer
<kbd>menu .</kbd>   | end of the buffer
<kbd>menu - -</kbd> | center line
<kbd>menu SPC</kbd> | mk abbrev: insert
<kbd>menu a f</kbd> | toggle auto-fill mode
<kbd>menu a g</kbd> | toggle aggressive indent mode
<kbd>menu a p</kbd> | apropos
<kbd>menu a r</kbd> | align regexp
<kbd>menu a s</kbd> | write file (save file as…)
<kbd>menu b j</kbd> | jump to bookmark
<kbd>menu b k</kbd> | jump to bookmark in other window
<kbd>menu b l</kbd> | list all bookmarks
<kbd>menu b s</kbd> | set new bookmark
<kbd>menu c a</kbd> | Calc
<kbd>menu c c</kbd> | copy entire current buffer
<kbd>menu c i</kbd> | Cider Jack-in
<kbd>menu c l</kbd> | Calendar
<kbd>menu c r</kbd> | copy rectangle
<kbd>menu c s</kbd> | set coding system
<kbd>menu c w</kbd> | count words
<kbd>menu d a</kbd> | show date in the minibuffer
<kbd>menu d b</kbd> | double buffer: show current buffer in other window
<kbd>menu d c</kbd> | describe char
<kbd>menu d d</kbd> | show default directory in the minibuffer
<kbd>menu d i</kbd> | Diff
<kbd>menu e ;</kbd> | evaluate expression
<kbd>menu e b</kbd> | erase buffer
<kbd>menu e e</kbd> | evaluate last s-expression
<kbd>menu e r</kbd> | ERC
<kbd>menu e v</kbd> | eval current buffer
<kbd>menu f f</kbd> | find Emacs Lisp function
<kbd>menu f l</kbd> | list Flycheck errors
<kbd>menu f n</kbd> | put file name into kill ring
<kbd>menu f o</kbd> | set specified font
<kbd>menu f v</kbd> | find Emacs Lisp variable
<kbd>menu g d</kbd> | GDB
<kbd>menu g l</kbd> | go to line (avy)
<kbd>menu g n</kbd> | GNUS
<kbd>menu g r</kbd> | recursive grep
<kbd>menu h a</kbd> | highlight symbol: remove all
<kbd>menu h e</kbd> | Ebal: execute
<kbd>menu h i</kbd> | Ebal: init
<kbd>menu h r</kbd> | split window (horizontal)
<kbd>menu h s</kbd> | highlight symbol
<kbd>menu i r</kbd> | indent region
<kbd>menu j r</kbd> | skewer REPL
<kbd>menu j s</kbd> | run skewer
<kbd>menu k r</kbd> | kill rectangle
<kbd>menu l b</kbd> | list buffers
<kbd>menu l i</kbd> | SLIME
<kbd>menu l p</kbd> | list packages
<kbd>menu m a</kbd> | magit dispatch popup
<kbd>menu m c</kbd> | magit clone
<kbd>menu m d</kbd> | markdown mode
<kbd>menu m i</kbd> | magit init
<kbd>menu m k</kbd> | make current project
<kbd>menu m m</kbd> | switch to messages buffer
<kbd>menu m n</kbd> | man
<kbd>menu m s</kbd> | magit status
<kbd>menu n n</kbd> | narrow to region
<kbd>menu n w</kbd> | widen
<kbd>menu p a</kbd> | package autoremove
<kbd>menu p f</kbd> | install package from file/directory
<kbd>menu p i</kbd> | install package from package repository
<kbd>menu p j</kbd> | run Django development server
<kbd>menu p r</kbd> | print current buffer
<kbd>menu p u</kbd> | upgrade all packages
<kbd>menu p y</kbd> | run Python
<kbd>menu q e</kbd> | query replace regexp
<kbd>menu q r</kbd> | query replace
<kbd>menu r b</kbd> | report Emacs bug
<kbd>menu r c</kbd> | copy to register
<kbd>menu r i</kbd> | insert register
<kbd>menu r n</kbd> | rectangle number lines
<kbd>menu r r</kbd> | reverse region (lines)
<kbd>menu s a</kbd> | select the whole buffer (mnemonic: «select all»)
<kbd>menu s h</kbd> | Emacs shell
<kbd>menu s l</kbd> | sort lines
<kbd>menu s n</kbd> | sort numeric fields
<kbd>menu s p</kbd> | speed type text
<kbd>menu s r</kbd> | string rectangle
<kbd>menu s s</kbd> | switch to scratch buffer
<kbd>menu s t</kbd> | insert date
<kbd>menu t h</kbd> | switch to custom color theme
<kbd>menu u t</kbd> | untabify
<kbd>menu v a</kbd> | vimish fold: avy
<kbd>menu v e</kbd> | show version in minibuffer
<kbd>menu v f</kbd> | vimish fold
<kbd>menu v r</kbd> | split window (vertical)
<kbd>menu v v</kbd> | vimish fold: delete
<kbd>menu x i</kbd> | install current project (via `install.sh`)
<kbd>menu x u</kbd> | uninstall current project (via `uninstall.sh`)
<kbd>menu y a</kbd> | reload all snippets
<kbd>menu y p</kbd> | insert the primary selection at the point
<kbd>menu y r</kbd> | yank rectangle
<kbd>C-c i</kdb>    | SLIME: load ASDF system switch to package
<kbd>b</kbd>        | Dired: up directory
<kbd>e</kbd>        | Dired: open with external application
<kbd>i</kbd>        | Dired: show images in current directory
<kbd>w</kbd>        | Dired: change to WDired mode
<kbd>z</kbd>        | Dired: show directory as a tree
<kbd>C-c C-l</kbd>  | Markdown: export
<kbd>C-c C-v</kbd>  | Markdown: preview
<kbd>C-c C-l</kbd>  | TeX: export to PDF
<kbd>C-c C-v</kbd>  | TeX: preview (as PDF via OS-preferred tool)
<kbd>C-c C-l</kbd>  | Texinfo: export
<kbd>C-c C-v</kbd>  | Texinfo: preview
<kbd>C-backspace</kbd> | Smartparens: backward kill S-expression
<kbd>M-b</kbd>      | Smartparens: backward S-expression
<kbd>M-d</kbd>      | Smartparens: kill S-expression
<kbd>M-f</kbd>      | Smartparens: forward S-expression
<kbd>M-h</kbd>      | Smartparens: select next S-expression
<kbd>M-k</kbd>      | Smartparens: hybrid kill
<kbd>M-t</kbd>      | Smartparens: add to previous S-expression

## Keyboard Layouts and Abbreviations

It's true that standard keyboards have too few keys. We should be able to
input many-many different fancy symbols and switch languages very easily,
with one key pressing. I use <kbd>F7</kbd> and <kbd>F8</kbd> for this
task. Every key performs switching to input method of some language or
disables input method of that language when this it's already active (it
also switches dictionaries used for spell checking, note that you need to
install `aspell` for that). This way I don't need to «cycle» through all the
languages — a horrible thing.

For peculiar use case of Emacs abbreviations see
[mk-abbrev](https://github.com/mrkkrp/mk-abbrev). It now has its own
repository. It's possible that this will be implemented in more flashy way
some day.

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

Also, I've taught GNUS to restore my window configuration when I exit it.

## Appearance

I use `solarized-dark` theme. It's the only theme I feel comfortable
with. My font of choice is `DejaVu Sans Mono`, because it's free, easy to
get on Arch Linux and it supports all scripts that I need: Latin, Cyrillic,
Greek, and who knows what else (I like strange symbols), it's also quite
pretty. I had been a big fan of `Inconsolata` (and then `Ubuntu Mono`)
before I've switched to `DejaVu Sans Mono`. The reason for switching is that
there is no normal version of the font supporting Cyrillic script (yes, I've
tried modifications à la Inconsolata LCG, they suck or I'm not sufficiently
dedicated person to make them look normally).

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
