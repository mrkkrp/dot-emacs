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
* purging of buffers (except for «basic» ones);
* searching online with DuckDuckGo (you can use Google if you can tolerate
  spying);
* upgrading of all packages without displaying of `*Packages*` buffer;
* smart switching between default input method, French, and Russian;
* some mode line wizardly (can't use `smart-mode-line`, sorry);
* many little hacks that make everything work smoothly and look sexy.

## Key Remapping

If you do text editing professionally and you have no ergonomic keyboard,
get one. I use «Truly Ergonomic Keyboard» (no, they don't pay me for the
advertising).

It's common knowledge that if you use a laptop, you should remap <kbd>Caps
Lock</kbd> to <kbd>Ctrl</kbd> (at least if you're an Emacs user). It's
undoubtedly true, but what about <kbd>Tab ↹</kbd> key and <kbd>⌫
Backspace</kbd>? There are three things to consider:

1. You need to press <kbd>⌫ Backspace</kbd> more often or just as often as
   <kbd>Tab ↹</kbd> (when editing code, in other cases you usually don't
   need <kbd>Tab ↹</kbd> at all).

2. On standard laptop keyboard <kbd>Tab ↹</kbd> occupies very comfortable
   position right above <kbd>Caps Lock</kbd>, while <kbd>⌫ Backspace</kbd>
   is far away and you need to *change* position of your hand a little to
   reach it.

3. There are no other way in Emacs to invoke `backward-delete-char` command,
   than via <kbd>⌫ Backspace</kbd>, there is no handy alternative key
   binding for it, while <kbd>Tab ↹</kbd> has comfortable alternative
   <kbd>C-i</kbd>.

So why not swap <kbd>Tab ↹</kbd> key and <kbd>⌫ Backspace</kbd>? If you're a
Linux user, here is `~/.Xmodmap` file that may be helpful:

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

I recommend changing shortcut for «window cycling», it's usually <kbd>Alt +
Tab ↹</kbd>, now it should be <kbd>Alt + ⌫ Backspace</kbd>, because this key
combination is comfortable and shouldn't change.

## Key Bindings

Don't use hairy default Emacs shortcuts. All frequently used commands must
be as simple as possible. Prefer single keys and key sequences over
key-chords. Even if it's not faster, it's better for your health.

I make use of <kbd>F</kbd> keys. There are not so many of them (I could bind
only 9 commands this way, since <kbd>F1</kbd>, <kbd>F3</kbd>, and
<kbd>F4</kbd> are already used by Emacs) so we have to use them
wisely. <kbd>F</kbd> keys are precious because single key pressing is the
most efficient thing you can do and on most keyboards you can easily reach
the keys.

Next, Emacs allows us to define some custom key bindings: <kbd>C-c</kbd>
prefix followed by a single key. I've assigned some commands this way. The
bad thing about these shortcuts is that you have to start them with a chord
<kbd>C-c</kbd>, while it's way better than <kbd>C-c C-o C-l</kbd> (such
shortcuts shouldn't be used at all!), I don't like to press several keys
simultaneously (unless such a combination is self-sufficient, like
<kbd>C-n</kbd>). Also, there are not so many combinations starting with this
common prefix, if we want to avoid too long key sequences.

Here key sequences starting with «introducing key» come into play. We can
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
<kbd>menu SPC</kbd> | special insertion of abbreviation
<kbd>menu a b</kbd> | toggle abbrev mode
<kbd>menu a p</kbd> | apropos
<kbd>menu a r</kbd> | align regexp
<kbd>menu a s</kbd> | write file (save file as…)
<kbd>menu c a</kbd> | Calc
<kbd>menu c i</kbd> | Cider Jack-in
<kbd>menu c l</kbd> | Calendar
<kbd>menu c r</kbd> | copy rectangle
<kbd>menu c s</kbd> | set coding system
<kbd>menu d a</kbd> | show date in the minibuffer
<kbd>menu d c</kbd> | describe char
<kbd>menu d i</kbd> | Diff
<kbd>menu e e</kbd> | evaluate last s-expression
<kbd>menu e r</kbd> | ERC
<kbd>menu e s</kbd> | Emacs Shell
<kbd>menu g d</kbd> | GDB
<kbd>menu g l</kbd> | goto line
<kbd>menu g n</kbd> | GNUS
<kbd>menu g r</kbd> | recursive grep
<kbd>menu h r</kbd> | split window (horizontal)
<kbd>menu k r</kbd> | kill rectangle
<kbd>menu l b</kbd> | list buffers
<kbd>menu l i</kbd> | SLIME
<kbd>menu l p</kbd> | list packages
<kbd>menu m a</kbd> | magit status
<kbd>menu m n</kbd> | man
<kbd>menu q e</kbd> | query replace regexp
<kbd>menu q r</kbd> | query replace
<kbd>menu r n</kbd> | rectangle number lines
<kbd>menu s c</kbd> | Scheme
<kbd>menu s h</kbd> | shell
<kbd>menu s l</kbd> | sort lines
<kbd>menu s r</kbd> | string rectangle
<kbd>menu s s</kbd> | switch to scratch buffer
<kbd>menu s t</kbd> | insert date
<kbd>menu t e</kbd> | tetris
<kbd>menu v e</kbd> | show version in minibuffer
<kbd>menu v r</kbd> | split window (vertical)
<kbd>menu y r</kbd> | yank rectangle
<kbd>C-c r</kbd>    | SLIME: restart inferior Lisp
<kbd>C-c h</kbd>    | SLIME: Hyper Spec lookup
<kbd>C-c i</kdb>    | SLIME: load ASDF system switch to package
<kbd>C-c h</kbd>    | Haskell (including Cabal): Hoogle query
<kbd>C-c C-l</kbd>  | C mode: compile project
<kbd>b</kbd>        | Dired: up directory
<kbd>z</kbd>        | Dired: change to WDired mode

## Keyboard Layouts and Abbreviations

It's true that standard keyboards have too few keys. We should be able to
input many-many different fancy symbols and switch languages very easily,
with one key pressing. I use <kbd>F7</kbd> and <kbd>F8</kbd> for this
task. Every key performs switching to input method of some language or
disables input method of that language when this it's already active (it
also switches dictionaries used for spell checking, note that you need to
install `aspell` for that). This way I don't need to «cycle» through all the
languages — horrible thing.

But there are also quite a few characters that don't belong to any
particular keyboard layout or language. To input them I use Emacs
abbreviations. All my abbreviations start with ‘8’ simply because this key
is easy to reach and not so many normal words start with ‘8’.

Here is the transformation table:

Abbreviation  | Result Character | Name of the character
------------  | ---------------- | ---------------------
`8apeq`       | ≈                | approximately equal
`8bot`        | ⊥                | bottom
`8bull`       | •                | bullet
`8copy`       | ©                | copyright sign
`8dagg`       | †                | dagger
`8dagr`       | ‡                | crossed dagger
`8dash`       | —                | em dash
`8dda`        | ⇓                | double downwards arrow
`8degr`       | °                | degree
`8dla`        | ⇐                | double leftwards arrow
`8dqu`        | “”               | double quotation marks
`8dra`        | ⇒                | double rightwards arrow
`8dua`        | ⇑                | double upwards arrow
`8elli`       | …                | ellipsis
`8guil`       | «»               | guillemets
`8hash`       | #                | number sign or hash sign
`8id`         | ≡                | identical to
`8ineg`       | ∫                | integral
`8ineq`       | ≠                | inequality
`8inf`        | ∞                | infinity
`8intr`       | ·                | interpunct
`8mnpl`       | ∓                | minus-plus
`8mult`       | ×                | multiplication
`8nabl`       | ∇                | nabla
`8num`        | №                | numero sign
`8obel`       | ÷                | obelus
`8plmn`       | ±                | plus-minus
`8pnd`        | £                | pound
`8prod`       | ∏                | product
`8qed`        | ■                | quod erat demonstrandum
`8root`       | √                | root
`8rub`        | ₽                | Russian ruble
`8sda`        | ↓                | simple downwards arrow
`8sect`       | §                | section
`8sla`        | ←                | simple leftwards arrow
`8squ`        | ‘’               | single quotation marks
`8sra`        | →                | simple rightwards arrow
`8sua`        | ↑                | simple upwards arrow
`8sum`        | ∑                | summation

There is also entire Greek alphabet:

Abbreviation | Letter | Abbreviation  | Letter
------------ | ------ | ------------  | ------
`8Alpha`     | Α      | `8alpha`      | α
`8Beta`      | Β      | `8beta`       | β
`8Gamma`     | Γ      | `8gamma`      | Y
`8Delta`     | Δ      | `8delta`      | δ
`8Epsilon`   | Ε      | `8epsilon`    | ε
`8Zeta`      | Ζ      | `8zeta`       | ζ
`8Eta`       | Η      | `8eta`        | η
`8Theta`     | Θ      | `8theta`      | θ
`8Iota`      | Ι      | `8iota`       | ι
`8Kappa`     | Κ      | `8kappa`      | κ
`8Lambda`    | Λ      | `8lambda`     | λ
`8Mu`        | Μ      | `8mu`         | μ
`8Nu`        | Ν      | `8nu`         | ν
`8Xi`        | Ξ      | `8xi`         | ξ
`8Omicron`   | Ο      | `8omicron`    | ο
`8Pi`        | Π      | `8pi`         | π
`8Rho`       | Ρ      | `8rho`        | ρ
`8Sigma`     | Σ      | `8sigma`      | σ
`8Tau`       | Τ      | `8tau`        | τ
`8Upsilon`   | Υ      | `8upsilon`    | υ
`8Phi`       | Φ      | `8phi`        | φ
`8Chi`       | Χ      | `8chi`        | χ
`8Psi`       | Ψ      | `8psi`        | ψ
`8Omega`     | Ω      | `8omega`      | ω

Final sigma ς is written as `8fsigma`.

Note that abbreviations are better than key bindings à la <kbd>C-x 8
…</kbd>, because the key bindings are hard to remember and their number is
insufficient. So, abbreviations with readable names in the the way to go.

However the method of expansion of the abbreviations is not that good at
all. It's flawed at least in the following ways:

1. User cannot write and expand abbreviations in arbitrary context, she
   needs to surround them with «non-word-constituent-characters»: spaces or
   punctuation. So this thing won't work:

   ```
   “It's difficult8elli” ≠ “It's difficult…”
   ```

2. When using input method for non-Latin languages user needs to disable
   input method, type an abbreviation, enable input method again — this is
   rather awkward.

I've written a function that performs insertion of abbreviation and its
expansion. This function solves all the problems. Just type <kbd>menu
SPC</kbd> and enter abbreviation without leading ‘8’, finish typing with
<kbd>SPC</kbd> and abbreviation will be inserted and expanded. Point will be
placed after one-character expansions and in between such expansions as
‘«»’. Since user writes abbreviation in the minibuffer, current input method
is ignored.

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

## Appearance

I use `solarized-dark` theme. It's the only theme I feel comfortable
with. My font of choice is `Ubuntu Mono`, because it's easy to get on Arch
Linux and it supports all scripts that I need: Latin, Cyrillic, Greek, and
who knows what else (I like strange symbols), it's also quite pretty. I had
been a big fan of Inconsolata before I switched to Ubuntu Mono. The reason
for switching is that there is no normal version of the font supporting
Cyrillic script (yes, I've tried modifications à la Inconsolata LCG, they
suck or I'm not sufficiently dedicated person to make them look normally). I
also enable `hl-line-mode` in modes like `dired` for prettiness.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
