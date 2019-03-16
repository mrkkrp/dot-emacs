# Emacs Configuration

[![CircleCI](https://circleci.com/gh/mrkkrp/dot-emacs/tree/master.svg?style=svg)](https://circleci.com/gh/mrkkrp/dot-emacs/tree/master)

Emacs is something that encourages its users to think about their
productivity and once you started to search for ways to improve your
workflow and efficiency—sky is the limit.

Currently, this setup is heavily tailored towards ergonomics and comfortable
typing. While some people start to suffer from RSI as you're reading this
text, I'm more productive than ever.

* [Features](#features)
* [The keyboard](#the-keyboard)
* [Sticky keys](#sticky-keys)
* [Dvorak layout](#dvorak-layout)
* [Modal editing](#modal-editing)
* [Key bindings](#key-bindings)
* [Appearance](#appearance)
* [License](#license)

## Features

Some minor features I have implemented (nothing special, interesting things
are usually published as packages, so people can benefit from my hacking):

* automatic installation of all necessary packages on startup;

* many peculiar editing primitives;

* searching online with DuckDuckGo (it also knows how to prefix my search
  query depending on major mode, so if I edit a Python source, my query will
  start with `"python "` automatically, it's a configurable thing);

* upgrading of all packages without displaying the `*Packages*` buffer.

## The keyboard

Any work with text begins with a keyboard. To avoid suffering pain now or in
the future, do editing fast and easily, one has to get a *proper* keyboard.
Most people do not understand the value of proper keyboard and use
traditional, nonsensical keyboard designs. Do not do that, *get a proper
keyboard now*.

I believe that a proper keyboard should satisfy the following criteria:

* It should not have staggered keys. Keys should form a matrix. This way you
  avoid stretching your fingers in weird ways.

* It should be split or have enough space between each hand's cluster. This
  allows to keep hands in the natural position.

* It should have mechanical switches. Of course it should.

* It should have clusters for thumbs because thumbs are the strongest
  fingers. On the other hand, it should not force the user use pinkies too
  much.

* It should be symmetric with two <kbd>⇧ Shift</kbd> keys, <kbd>⎈ Ctrl</kbd>
  keys, and two <kbd>⎇ Alt</kbd> keys, so when one hand hints a regular key,
  the other hand always has a modifier to add to that key if necessary.

* Ideally, it should have a bowl-shaped surface so hands can easily reach
  any key without stretching.

Only two companies produce keyboards that satisfy all these requirements:
[Maltron](https://maltron.com) and [Kinesis](https://kinesis-ergo.com). I
use the [Kinesis Advantage 2
QD](https://www.kinesis-ergo.com/shop/advantage2-qd/) keyboard ([a bigger
picture](https://www.kinesis-ergo.com/wp-content/uploads/2016/07/kb600qd-oh-1977x1024.png))
and so this setup is tailored to be used with it. I would not use any other
keyboard. The default layout is so good that I only swapped <kbd>⎋ Esc</kbd>
and <kbd>⇪ Caps Lock</kbd>.

## Sticky keys

I'm a big fan of the “sticky keys” feature. Most major operating systems and
desktop environments provide this feature. It allows to press modifier keys,
such as <kbd>⎈ Ctrl</kbd>, <kbd>⎇ Alt</kbd>, and <kbd>⇧ Shift</kbd>
sequentially, instead of pressing multiple keys at a time—that's how all
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
learning `evil` to edit “vi-style”, but I estimated resulting effect to be
not superior to editing with familiar Emacs key bindings.

See my key translation map may be found in the `mk/mk-global.el` file.

## Key Bindings

I don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. I prefer single keys and key sequences over
key-chords. Even if it's not faster, it's better for my health.

With my **Kinesis Advantage 2 QD** keyboard, I make use of the keys in thumb
clusters. I measured that I hit the “save” button often enough to put it in
a prominent position and so I did. My thumb clusters also contain functions
like “find file”, “switch to buffer”, “switch window”, and a key that starts
a lot of sequential commands that I will describe in a moment.

Next, Emacs allows us to define some custom key bindings: <kbd>C-c</kbd>
prefix followed by a single key. I've assigned some commands this way. The
bad thing about these shortcuts is that you have to start them with a
chord/key combination <kbd>C-c</kbd>, while it's way better than <kbd>C-c
C-o C-l</kbd> (such shortcuts shouldn't be used at all!), I don't like to
press several keys simultaneously (unless such a combination is
self-sufficient, like <kbd>C-n</kbd>). Also, there are not so many
combinations starting with this common prefix, if we want to avoid too long
key sequences.

Here key sequences starting with an “introducing key” come into play. We can
choose a single key, whose seul rôle will be starting key sequences. How
long should every such a key sequence be? Of course, we want it to be as
short as possible, but we cannot use only one key after the introducing key,
because total number of combinations won't be satisfactory. But we can use
two keys after the introducing key (<kbd>Page Down</kbd> in my case), then
we get 26 × 26 = 676 combinations! (In practice, we get even more because of
punctuation and numbers.) Not bad at all. There are enough combinations for
us to prefer those that have some mnemonic value. Give this technique a try
and you will see how productive you can be!

## Appearance

I use the `solarized-dark` theme. It's the only theme I feel comfortable
with. My font of choice is `DejaVu Sans Mono`, because it's free and it
supports all scripts that I need: Latin, Cyrillic, Greek, and whatnot. It's
also quite pretty. I was a big fan of `Inconsolata` (and `Ubuntu Mono` after
that) before I switched to `DejaVu Sans Mono`. The reason for switching is
that there is no normal version of the font supporting Cyrillic script (yes,
I've tried modifications à la Inconsolata LCG, they suck or I'm not
sufficiently dedicated person to make them look properly).

## License

Copyright © 2015–2019 Mark Karpov

Distributed under GNU GPL, version 3.
