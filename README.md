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

I write almost exclusively in Haskell now, with bits of some scripting
languages and Emacs Lisp, so setups for other languages may be kinda
obsolete.

* [Packages](#packages)
* [Features](#features)
* [Key Remapping](#key-remapping)
* [Sticky Keys](#sticky-keys)
* [Dvorak Layout](#dvorak-layout)
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
[fix-input](https://github.com/mrkkrp/fix-input) † | Type Russian in Emacs with Dvorak layout on system level
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
[mustache-mode](https://github.com/mustache/emacs) | Major mode for Mustache
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

† These packages I wrote myself, it seems like other people find them useful
  too.

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

* upgrading of all packages without displaying of `*Packages*` buffer.

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

## Dvorak Layout

I recently switched to the Dvorak layout and it's fantastic. Typing is a lot
more comfortable now. I've created the
[`fix-word`](https://github.com/mrkkrp/fix-word) package to preserve my
ability to type Russian in Emacs when Dvorak is my layout in OS level. Nice!

## Modal Editing

Currently I'm using `modalka-mode` that allows to edit text in modal
fashion. Modal editing is fundamentally better for health and more efficient
in general. Initially I considered learning `evil` to edit in «vim-style»,
but I estimated resulting effect not superior to editing with familiar Emacs
key bindings.

See my key translation map in `mk/mk-global.el` file.

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
to use «Sticky Keys»), while it's way better than <kbd>C-c C-o C-l</kbd>
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

See my key-bindings in `mk/mk-global.el` file.

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
repository. It's possible that this will be implemented in more a flashy way
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

Copyright © 2015–2016 Mark Karpov

Distributed under GNU GPL, version 3.
