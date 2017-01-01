# ⌨ Emacs Configuration Files

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/dot-emacs.svg?branch=master)](https://travis-ci.org/mrkkrp/dot-emacs)

Emacs is something that encourages its users to think about their
productivity and once you started to search for ways to improve your
workflow and efficiency — sky is the limit.

I write almost exclusively in Haskell now, with bits of some scripting
languages and Emacs Lisp, so setup for other languages may be kinda
obsolete.

* [Packages](#packages)
* [Features](#features)
* [The keyboard](#the-keyboard)
* [Sticky keys](#sticky-keys)
* [Dvorak layout](#dvorak-layout)
* [Modal editing](#modal-editing)
* [Key bindings](#key-bindings)
* [Keyboard layouts and abbreviations](#keyboard-layouts-and-abbreviations)
* [MU4E](#mu4e)
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
[auctex](https://elpa.gnu.org/packages/auctex.html) | Integrated environment for *TeX*
[avy](https://github.com/abo-abo/avy) | Move cursor effectively
[avy-menu](https://github.com/mrkkrp/avy-menu) † | Avy-powered popup menu
[char-menu](https://github.com/mrkkrp/char-menu) † | Fast insertion of arbitrary symbols
[cider](https://github.com/clojure-emacs/cider) | Clojure IDE
[common-lisp-snippets](https://github.com/mrkkrp/common-lisp-snippets) † | Yasnippets for Common Lisp
[counsel](https://github.com/abo-abo/swiper) | Various completion functions using Ivy
[cyphejor](https://github.com/mrkkrp/cyphejor) † | Shorten names of major modes
[ebal](https://github.com/mrkkrp/ebal) † | Emacs interface to Cabal
[f](https://github.com/rejeep/f.el) | Modern API for working with files and dirs
[fix-input](https://github.com/mrkkrp/fix-input) † | Type Russian in Emacs with Dvorak layout on system level
[fix-word](https://github.com/mrkkrp/fix-word) † | Convenient word transformation
[flycheck](https://github.com/flycheck/flycheck) | On-the-fly syntax checking
[flycheck-color-mode-line](https://github.com/flycheck/flycheck-color-mode-line) | Colorize mode line according to Flycheck status
[flycheck-haskell](https://github.com/flycheck/flycheck-haskell) | Flycheck: Cabal projects and sandboxes
[flyspell-lazy](https://github.com/rolandwalker/flyspell-lazy) | Improve Flyspell responsiveness using idle timers
[ghc](https://github.com/kazu-yamamoto/ghc-mod) | Improve Haskell REPL experience
[git-link](https://github.com/sshaw/git-link) | Get GitHub URL for a buffer location
[gitignore-mode](https://github.com/magit/git-modes) | Major mode for editing .gitignore files
[gnugo](https://elpa.gnu.org/packages/gnugo.html) | Emacs frontend for GNUGO
[haskell-mode](https://github.com/haskell/haskell-mode) | A Haskell editing mode
[hasky-extensions](https://github.com/hasky-mode/hasky-extensions) † | Easily toggle Haskell language extensions
[highlight-symbol](https://github.com/nschum/highlight-symbol.el) | Automatic and manual symbol highlighting
[hl-todo](https://github.com/tarsius/hl-todo) | Highlight TODO and similar keywords
[js2-mode](https://github.com/mooz/js2-mode) | Improved JavaScript editing mode
[kill-or-bury-alive](https://github.com/mrkkrp/kill-or-bury-alive) † | Precise control over buffer killing in Emacs
[magit](https://github.com/magit/magit) | A Git porcelain inside Emacs
[markdown-mode](http://daringfireball.net/projects/markdown/) | Major mode for Markdown-formatted text files
[modalka](https://github.com/mrkkrp/modalka) † | Easily introduce native modal editing of your own design
[mustache-mode](https://github.com/mustache/emacs) | Major mode for Mustache
[org](https://elpa.gnu.org/packages/org.html) | Outline-based template notes management
[rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) | Highlight brackets according to their depth
[rich-minority](https://github.com/Malabarba/rich-minority) | Clean-up and beautify the list of minor-modes
[shakespeare-mode](https://github.com/CodyReichert/shakespeare-mode) | Support for Hamlet, Lucius, and Julius templates
[skewer-mode](https://github.com/skeeto/skewer-mode) | Live web development with Emacs
[slime](https://github.com/slime/slime) | Superior Lisp Interaction Mode for Emacs
[smart-mode-line](https://github.com/Malabarba/smart-mode-line) | A powerful and beautiful mode-line for Emacs
[smartparens](https://github.com/Fuco1/smartparens) | Tricks for working with all kinds of parenthesis
[solarized-theme](https://github.com/bbatsov/solarized-emacs) | The Solarized color theme
[swiper](https://github.com/abo-abo/swiper) | Isearch with overview
[typit](https://github.com/mrkkrp/typit) † | A cool typing game
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
you can contribute to, improve it and help other by doing so.

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

## The keyboard

Any work with a text editor begins with keyboard. To avoid suffering pain
now or in the future, do editing fast and easily, one has to get a *proper*
keyboard. Most people do not understand the value of proper keyboard and use
traditional, nonsensical keyboard designs. Do not do that, get a proper
keyboard now.

Proper keyboard should satisfy the following criteria:

* It should not have staggered keys. Keys should form a matrix. This way you
  avoid stretching your fingers *without any reason at all*.

* It should be split or have enough space between each hand's cluster. This
  allows to keep hands in natural position.

* It should have mechanical switches. Of course it should.

* It should have clusters for thumbs because thumbs are the strongest
  fingers. On the other hand it should not force the user use pinkies too
  much.

* It should be symmetric with two <kbd>⇧ Shift</kbd> keys, <kbd>⎈ Ctrl</kbd>
  keys, and two <kbd>⎇ Alt</kbd> keys, so when one hand taps a regular key,
  the other hand always has a modifier to add to that regular key.

* Ideally, it should have a bowl-shaped surface so hands can easily reach
  any key without stretching.

Only two companies produce keyboards that satisfy all these requirements:
[Maltron](https://maltron.com) and [Kinesis](https://kinesis-ergo.com). I
use the
[Kinesis Advantage](https://www.kinesis-ergo.com/shop/advantage-for-pc-mac/)
keyboard and so this setup is tailored to be used with this keyboard. I
would not use any other keyboard but that. The default layout is so good
that I only swapped <kbd>⎋ Esc</kbd> and <kbd>⇪ Caps Lock</kbd>.

## Sticky keys

I'm a big fan of the “sticky Keys” feature. Most major operating systems and
desktop environments provide this feature, it allows to press modifier keys,
such as <kbd>⎈ Ctrl</kbd>, <kbd>⎇ Alt</kbd>, and <kbd>⇧ Shift</kbd>
sequentially, instead of pressing multiple keys at a time — that's how all
computers should work by default. Anyway, for some commands I hold <kbd>⎈
Ctrl</kbd> (navigation commands and a few others), but there are not so many
of them.

## Dvorak layout

I use the Dvorak layout to type English and it's fantastic. Typing is a lot
more comfortable and efficient. I've created the
[`fix-input`](https://github.com/mrkkrp/fix-input) package to preserve my
ability to type Russian in Emacs when Dvorak is my layout in OS level. Nice!

## Modal editing

Currently I'm using the [`modalka-mode`](https://github.com/mrkkrp/modalka)
that allows to edit text in modal fashion. Modal editing is fundamentally
better for health and more efficient in general. Initially I considered
learning `evil` to edit “vi-style”, but I estimated resulting effect not
superior to editing with familiar Emacs key bindings.

See my key translation map in `mk/mk-global.el` file.

## Key Bindings

I don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. I prefer single keys and key sequences over
key-chords. Even if it's not faster, it's better for health.

With my Kinesis Advantage keyboard, I make use of keys in thumb clusters. I
measured that I hit the “save” button often enough to put it to prominent
position and that I did. My thumb clusters also contain functions like “find
file”, “switch to buffer”, “switch window”, and a key that start a lot of
sequential commands that I will describe in a moment.

Next, Emacs allows us to define some custom key bindings: <kbd>C-c</kbd>
prefix followed by a single key. I've assigned some commands this way. The
bad thing about these shortcuts is that you have to start them with a
chord/key combination <kbd>C-c</kbd>, while it's way better than <kbd>C-c
C-o C-l</kbd> (such shortcuts shouldn't be used at all!), I don't like to
press several keys simultaneously (unless such a combination is
self-sufficient, like <kbd>C-n</kbd>). Also, there are not so many
combinations starting with this common prefix, if we want to avoid too long
key sequences.

Here key sequences starting with “introducing key” come into play. We can
choose single key, whose seul rôle will be starting key sequences. How long
should every such a key sequence be? Of course, we want it to be as short as
possible, but we cannot use only one key after the introducing key, because
total number of combinations won't be satisfactory. But we can use two keys
after the introducing key (<kbd>Page Down</kbd> in my case), then we get 26
× 26 = 676 combinations! (In practice you get even more because of
punctuation and numbers.) Not bad at all. There are enough combinations for
us to prefer those that have some mnemonic value. Give this technique a try
and you will see how productive you can be!

See my key-bindings in `mk/mk-global.el` file.

## Keyboard Layouts and Abbreviations

It's true that standard keyboards have too few keys. We should be able to
input many-many different fancy symbols and switch languages very easily,
with one key pressing. I use <kbd>F8</kbd> for this task. That key performs
switching to Russian input method or disables it when this it's already
active (it also switches dictionaries used for spell checking, note that you
need to install `aspell` for that).

To insert various Unicode characters I use
[`char-menu`](https://github.com/mrkkrp/char-menu) package, which see.

## MU4E

I currently use [mu4e](http://www.djcbsoftware.nl/code/mu/) as mail client
and I'm quite satisfied with it (previously I used GNUS, and MU4E is a great
improvement IMO). You can see my configuration in `mk/mk-mu4e.el`. Here I'm
going to explain how to make it work.

First of all, you need to install `offlineimap` (it's written in Python, so
you will need to get Python too):

```
# pacman -S offlineimap
```

Create `~/.offlineimaprc` file. I use something like this (I'm on Arch
Linux, change this as necessary if you're using another Linux distro):

```
[general]
accounts = main
pythonfile = ~/.emacs.d/.offlineimap.py

[Account main]
localrepository = main_local
remoterepository = main_remote
status_backend = sqlite

[Repository main_local]
type = Maildir
localfolders = ~/Maildir

[Repository main_remote]
type = IMAP
remotehost = imap.openmailbox.org
remoteuser = user@openmailbox.org
remotepasseval = get_password("imap.openmailbox.org", "user@openmailbox.org", "993")
ssl = yes
maxconnections = 1
realdelete = no
folderfilter = lambda f: f != "Spam"
holdconnectionopen = true
keepalive = 60
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
```

Note that I put custom Python bits in `~/.emacs.d/.offlineimap.py` and it's
in this repo, just copy it.

Create `~/.authinfo.gpg` that should look like this:

```
machine imap.openmailbox.com login user@openmailbox.com port 993 password yourpass
machine smtp.openmailbox.com login user@openmailbox.com port 587 password yourpass
```

Keep it encrypted with GPG (Emacs should figure out that you want to use GPG
and encrypt it automatically).

Now launch `offlineimap` and wait while it syncs your email box:

```
$ offlineimap
```

At this point you also need `mu` software itself (`mu4e` client comes with
it):

```
# yaourt -S mu
```

Tell `mu` index your mail box:

```
$ mu index --maildir=~/Maildir
```

To send mails with `smtpmail.el` and use `gnutls`, we need install the
package:

```
# pacman -S gnutls
```

Done. Now you can try <kbd>M-x mu4e</kbd>. I've made some costmetic
adjustments, but it mostly works out-of-box very well.

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
