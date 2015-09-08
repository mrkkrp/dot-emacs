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
* [Key Bindings](#key-bindings)
* [Keyboard Layouts and Abbreviations](#keyboard-layouts-and-abbreviations)
* [GNUS](#gnus)
* [Appearance](#appearance)
* [License](#license)

## Packages

Here is collection of packages that I use (in alphabetical order):

Package/Repo | Source | Description
------------ | ------ | -----------
[ace-link](https://github.com/abo-abo/ace-link) | [![MELPA](http://melpa.org/packages/ace-link-badge.svg)](http://melpa.org/#/ace-link) | Quickly follow links
[ace-popup-menu](https://github.com/mrkkrp/ace-popup-menu) † | [![MELPA](http://melpa.org/packages/ace-popup-menu-badge.svg)](http://melpa.org/#/ace-popup-menu) | Replace GUI popup menus
[ace-window](https://github.com/abo-abo/ace-window) | [![MELPA](http://melpa.org/packages/ace-window-badge.svg)](http://melpa.org/#/ace-window) | Quickly switch windows
[aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode) | [![MELPA](http://melpa.org/packages/aggressive-indent-badge.svg)](http://melpa.org/#/aggressive-indent) | Keep code always indented
[auctex](http://git.savannah.gnu.org/cgit/emacs/elpa.git/?h=externals/auctex) | [ELPA](http://elpa.gnu.org/packages/auctex.html) | Integrated environment for *TeX*
[avy](https://github.com/abo-abo/avy) | [![MELPA](http://melpa.org/packages/avy-badge.svg)](http://melpa.org/#/avy) | Move cursor effectively
[buffer-move](https://github.com/lukhas/buffer-move) | [![MELPA](http://melpa.org/packages/buffer-move-badge.svg)](http://melpa.org/#/buffer-move) | Move buffers easily
[cider](https://github.com/clojure-emacs/cider) | [![MELPA](http://melpa.org/packages/cider-badge.svg)](http://melpa.org/#/cider) | Clojure IDE
[common-lisp-snippets](https://github.com/mrkkrp/common-lisp-snippets) † | [![MELPA](http://melpa.org/packages/common-lisp-snippets-badge.svg)](http://melpa.org/#/common-lisp-snippets) | Yasnippets for Common Lisp
[ebal](https://github.com/mrkkrp/ebal) † | [![MELPA](http://melpa.org/packages/ebal-badge.svg)](http://melpa.org/#/ebal) | Emacs interface to Cabal
[f](https://github.com/rejeep/f.el) | [![MELPA](http://melpa.org/packages/f-badge.svg)](http://melpa.org/#/f) | Modern API for working with files and dirs
[fix-word](https://github.com/mrkkrp/fix-word) † | [![MELPA](http://melpa.org/packages/fix-word-badge.svg)](http://melpa.org/#/fix-word) | Convenient word transformation
[flycheck](https://github.com/flycheck/flycheck) | [![MELPA](http://melpa.org/packages/flycheck-badge.svg)](http://melpa.org/#/flycheck) | On-the-fly syntax checking
[flycheck-color-mode-line](https://github.com/flycheck/flycheck-color-mode-line) | [![MELPA](http://melpa.org/packages/flycheck-color-mode-line-badge.svg)](http://melpa.org/#/flycheck-color-mode-line) | Colorize mode line according to Flycheck status
[flycheck-haskell](https://github.com/flycheck/flycheck-haskell) | [![MELPA](http://melpa.org/packages/flycheck-haskell-badge.svg)](http://melpa.org/#/flycheck-haskell) | Flycheck: Cabal projects and sandboxes
[ghc](https://github.com/kazu-yamamoto/ghc-mod) | [![MELPA](http://melpa.org/packages/ghc-badge.svg)](http://melpa.org/#/ghc) | Improve Haskell REPL experience
[gitignore-mode](https://github.com/magit/git-modes) | [![MELPA](http://melpa.org/packages/gitignore-mode-badge.svg)](http://melpa.org/#/gitignore-mode) | Major mode for editing .gitignore files
[haskell-mode](https://github.com/haskell/haskell-mode) | [![MELPA](http://melpa.org/packages/haskell-mode-badge.svg)](http://melpa.org/#/haskell-mode) | A Haskell editing mode
[highlight-line](https://github.com/mrkkrp/highlight-line) † | — | Highlight lines in list-like buffers
[highlight-symbol](https://github.com/nschum/highlight-symbol.el) | [![MELPA](http://melpa.org/packages/highlight-symbol-badge.svg)](http://melpa.org/#/highlight-symbol) | Automatic and manual symbol highlighting
[hl-todo](https://github.com/tarsius/hl-todo) | [![MELPA](http://melpa.org/packages/hl-todo-badge.svg)](http://melpa.org/#/hl-todo) | Highlight TODO and similar keywords
[ido-hacks](https://github.com/scottjad/ido-hacks) | [![MELPA](http://melpa.org/packages/ido-hacks-badge.svg)](http://melpa.org/#/ido-hacks) | Put more IDO in your IDO
[ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) | [![MELPA](http://melpa.org/packages/ido-ubiquitous-badge.svg)](http://melpa.org/#/ido-ubiquitous) | Use IDO (nearly) everywhere
[ido-vertial-mode](https://github.com/creichert/ido-vertical-mode.el) | [![MELPA](http://melpa.org/packages/ido-vertical-mode-badge.svg)](http://melpa.org/#/ido-vertical-mode) | Makes IDO-mode display vertically
[kill-or-bury-alive](https://github.com/mrkkrp/kill-or-bury-alive) † | [![MELPA](http://melpa.org/packages/kill-or-bury-alive-badge.svg)](http://melpa.org/#/kill-or-bury-alive) | Precise control over buffer killing in Emacs
[magit](https://github.com/magit/magit) | [![MELPA](http://melpa.org/packages/magit-badge.svg)](http://melpa.org/#/magit) | A Git porcelain inside Emacs
[markdown-mode](http://daringfireball.net/projects/markdown/) | [![MELPA](http://melpa.org/packages/markdown-mode-badge.svg)](http://melpa.org/#/markdown-mode) | Major mode for Markdown-formatted text files
[mk-abbrev](https://github.com/mrkkrp/mk-abbrev) † | — | Peculiar way to use Emacs abbrevs
[multiple-cursors](https://github.com/magnars/multiple-cursors.el) | [![MELPA](http://melpa.org/packages/multiple-cursors-badge.svg)](http://melpa.org/#/multiple-cursors) | Multiple cursors for Emacs
[org](http://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/packages/org) | [ELPA](http://elpa.gnu.org/packages/org.html) | Outline-based template notes management
[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) | [![MELPA](http://melpa.org/packages/rainbow-delimiters-badge.svg)](http://melpa.org/#/rainbow-delimiters) | Highlight brackets according to their depth
[slime](https://github.com/slime/slime) | [![MELPA](http://melpa.org/packages/slime-badge.svg)](http://melpa.org/#/slime) | Superior Lisp Interaction Mode for Emacs
[smart-mode-line](https://github.com/Malabarba/smart-mode-line) | [![MELPA](http://melpa.org/packages/smart-mode-line-badge.svg)](http://melpa.org/#/smart-mode-line) | A powerful and beautiful mode-line for Emacs
[smartparens](https://github.com/Fuco1/smartparens) | [![MELPA](http://melpa.org/packages/smartparens-badge.svg)](http://melpa.org/#/smartparens) | Tricks for working with all kinds of parenthesis
[smex](https://github.com/nonsequitur/smex) | [![MELPA](http://melpa.org/packages/smex-badge.svg)](http://melpa.org/#/smex) | M-x interface with IDO-style fuzzy matching
[solarized-theme](https://github.com/bbatsov/solarized-emacs) | [![MELPA](http://melpa.org/packages/solarized-theme-badge.svg)](http://melpa.org/#/solarized-theme) | The Solarized color theme
[visual-regexp](https://github.com/benma/visual-regexp.el) | [![MELPA](http://melpa.org/packages/visual-regexp-badge.svg)](http://melpa.org/#/visual-regexp) | Regexp replace with interactive visual feedback
[whole-line-or-region](https://github.com/purcell/whole-line-or-region) | [![MELPA](http://melpa.org/packages/whole-line-or-region-badge.svg)](http://melpa.org/#/whole-line-or-region) | Operate on current line if region undefined
[yaml-mode](https://github.com/yoshiki/yaml-mode) | [![MELPA](http://melpa.org/packages/yaml-mode-badge.svg)](http://melpa.org/#/yaml-mode) | Major mode for editing YAML serialization format
[yasnippet](https://github.com/capitaomorte/yasnippet) | [![MELPA](http://melpa.org/packages/yasnippet-badge.svg)](http://melpa.org/#/yasnippet) | Yet another snippet extension for Emacs
[ztree](https://github.com/fourier/ztree) | [![MELPA](http://melpa.org/packages/ztree-badge.svg)](http://melpa.org/#/ztree) | Show directory structure as a tree
[zygospore](https://github.com/louiskottmann/zygospore.el) | [![MELPA](http://melpa.org/packages/zygospore-badge.svg)](http://melpa.org/#/zygospore) | Reversible version of `delete-other-windows`
[zzz-to-char](https://github.com/mrkkrp/zzz-to-char) † | [![MELPA](http://melpa.org/packages/zzz-to-char-badge.svg)](http://melpa.org/#/zzz-to-char) | Fancy version of `zap-to-char` command

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

Also note that <kbd>↵ Return</kbd> key is pretty useless, because we have
<kbd>C-j</kbd> and <kbd>C-m</kbd> key bindings. Since <kbd>↵ Return</kbd>
key has two semantics: end of input and new line, it's logical that we have
two flavors of <kbd>↵ Return</kbd> key here. But what's about the <kbd>↵
Return</kbd> key itself? It's of no use, unless GPG pops up with its thing
(work is being done to enter passphrase for GPG via minibuffer, Emacs
way). At the same time <kbd>↵ Return</kbd> occupies a comfortable position
at the keyboard, so it's stupid not to use it for something useful. For me,
<kbd>↵ Return</kbd> key invokes `avy-goto-char`, and look, this is really
convenient! Try it!

Shortcut            | Description
--------            | -----------
<kbd>C-'</kbd>      | switch to other buffer
<kbd>C-;</kbd>      | correct previous misspelled word (long reaching)
<kbd>C-c C-o</kbd>  | find file at point (work for URLs too)
<kbd>C-c a</kbd>    | org agenda (week)
<kbd>C-c b</kbd>    | byte compile initialization files
<kbd>C-c e</kbd>    | open initialization file
<kbd>C-c h</kbd>    | lookup language specific documentation online
<kbd>C-c i</kbd>    | correct word before point
<kbd>C-c p</kbd>    | purge all buffers (except for 'basic')
<kbd>C-c r</kbd>    | revert current buffer (restart/reset REPL in some modes)
<kbd>C-c s</kbd>    | search online with DuckDuckGo
<kbd>C-c t</kbd>    | open org agenda file
<kbd>C-z</kbd>      | copy rest of the line
<kbd>M-c</kbd>      | fix word: capitalize
<kbd>M-e</kbd>      | replace last S-expression with its result
<kbd>M-g</kbd>      | mark word
<kbd>M-j</kbd>      | join the next line and the current one
<kbd>M-l</kbd>      | fix word: downcase
<kbd>M-n</kbd>      | transpose line down
<kbd>M-o</kbd>      | ace link
<kbd>M-p</kbd>      | transpose line up
<kbd>M-r</kbd>      | duplicate line
<kbd>M-u</kbd>      | fix word: upcase
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
<kbd>return</kbd>   | go to char (avy)
<kbd>S-up</kbd>     | move buffer up
<kbd>S-down</kbd>   | move buffer down
<kbd>S-left</kbd>   | move buffer left
<kbd>S-right</kbd>  | move buffer right
<kbd>menu menu</kbd>| smex (translation to <kbd>M-x</kbd>)
<kbd>menu ,</kbd>   | beginning of the buffer
<kbd>menu .</kbd>   | end of the buffer
<kbd>menu /</kbd>   | rectangular selection
<kbd>menu - -</kbd> | center line
<kbd>menu SPC</kbd> | special insertion of abbreviation
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
<kbd>menu e v</kbd> | eval buffer
<kbd>menu f f</kbd> | find Emacs Lisp function
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
<kbd>menu j a</kbd> | multiple cursors: mark all like this
<kbd>menu j e</kbd> | multiple cursors: edit ends of lines
<kbd>menu j i</kbd> | multiple cursors: insert numbers
<kbd>menu j l</kbd> | multiple cursors: edit lines
<kbd>menu j n</kbd> | multiple cursors: mark next like this
<kbd>menu j p</kbd> | multiple cursors: mark previous like this
<kbd>menu k r</kbd> | kill rectangle
<kbd>menu l b</kbd> | list buffers
<kbd>menu l i</kbd> | SLIME
<kbd>menu l p</kbd> | list packages
<kbd>menu m a</kbd> | magit dispatch popup
<kbd>menu m c</kbd> | magit clone
<kbd>menu m d</kbd> | markdown mode
<kbd>menu m i</kbd> | magit init
<kbd>menu m k</kbd> | make current project
<kbd>menu m n</kbd> | man
<kbd>menu m s</kbd> | magit status
<kbd>menu n n</kbd> | narrow to region
<kbd>menu n w</kbd> | widen
<kbd>menu p a</kbd> | package autoremove
<kbd>menu p f</kbd> | install package from file/directory
<kbd>menu p i</kbd> | install package
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
<kbd>menu s c</kbd> | Scheme
<kbd>menu s h</kbd> | Emacs shell
<kbd>menu s l</kbd> | sort lines
<kbd>menu s n</kbd> | sort numeric fields
<kbd>menu s r</kbd> | string rectangle
<kbd>menu s s</kbd> | switch to scratch buffer
<kbd>menu s t</kbd> | insert date
<kbd>menu t e</kbd> | tetris
<kbd>menu t h</kbd> | switch to custom color theme
<kbd>menu u t</kbd> | untabify
<kbd>menu v a</kbd> | vimish fold: avy
<kbd>menu v e</kbd> | show version in minibuffer
<kbd>menu v f</kbd> | vimish fold
<kbd>menu v r</kbd> | split window (vertical)
<kbd>menu v u</kbd> | vimish fold: unfold all
<kbd>menu v v</kbd> | vimish fold: refold
<kbd>menu x i</kbd> | install current project (via `install.sh`)
<kbd>menu x u</kbd> | uninstall current project (via `uninstall.sh`)
<kbd>menu y a</kbd> | reload all snippets
<kbd>menu y p</kbd> | insert the primary selection at the point
<kbd>menu y r</kbd> | yank rectangle
<kbd>C-c i</kdb>    | SLIME: load ASDF system switch to package
<kbd>C-c C-c</kbd>  | Haskell: Cabal interface
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
<kbd>M-g</kbd>      | Smartparens: select next S-expression
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
repository.

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
