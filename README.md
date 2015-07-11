# ⌨ Emacs Configuration Files

Emacs is something that encourages its users to think about their
productivity and once you started to search for ways to improve your
workflow and efficiency — sky is the limit.

## Features

Some minor features I have implemented (nothing special):

* automatic installation of all necessary packages;

* automation to compile/recompile SLIME (works smoothly with Quicklisp);

* transposition and duplication of lines;

* purging of buffers (except for «basic» ones);

* searching online with DuckDuckGo (it also knows how to prefix my search
  query depending on major mode, so if I edit Python source, my query will
  start with `"python "` automatically, it's configurable thing);

* upgrading of all packages without displaying of `*Packages*` buffer;

* smart switching between default input method, French, and Russian;

* some mode line wizardry (can't use `smart-mode-line` or `powerline`);

* many little hacks that make everything work smoothly and look sexy.

## Key Remapping

If you do text editing professionally and you have no ergonomic keyboard,
get one. I use «Truly Ergonomic Keyboard» (no, they don't pay me for the
advertising).

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

3. There are no other way in Emacs to invoke `backward-delete-char` command,
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
should every such a key sequence be?  Of course, we want it to be as short
as possible, but we cannot use only one key after the introducing key,
because total number of combinations won't be satisfactory. But we can use
two keys after the introducing key (<kbd>▤ Menu</kbd> in my case, if you
don't have <kbd>▤ Menu</kbd> on your keyboard you can remap something with
help of specialized software), then we get 26 × 26 = 676 combinations! Not
bad at all. There are enough combinations for you to prefer those that have
some mnemonic value. Give this technique a try and you will see how
productive you can be!

Also note that <kbd>↵ Return</kbd> key is pretty useless, because you have
<kbd>C-j</kbd> and <kbd>C-m</kbd> key bindings. Since <kbd>↵ Return</kbd>
key has two semantics: end of input and new line, it's logical that we have
two flavors of <kbd>↵ Return</kbd> key here. But what's about the <kbd>↵
Return</kbd> key itself? It's of no use, unless GPG pops up with its
thing. At the same time <kbd>↵ Return</kbd> occupies a comfortable position
at the keyboard, so it's stupid not to use it for something useful. For me,
<kbd>↵ Return</kbd> key invokes `avy-goto-char`, and look, this is really
convenient! Try it!

Shortcut            | Description
--------            | -----------
<kbd>C-'</kbd>      | switch to other buffer
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
<kbd>M-c</kbd>      | fix word: capitalize
<kbd>M-e</kbd>      | replace last S-expression with its result
<kbd>M-j</kbd>      | join the next line and the current one
<kbd>M-l</kbd>      | fix word: downcase
<kbd>M-n</kbd>      | transpose line down
<kbd>M-o</kbd>      | ace link
<kbd>M-p</kbd>      | transpose line up
<kbd>M-r</kbd>      | duplicate line
<kbd>M-u</kbd>      | fix word: upcase
<kbd>F2</kbd>       | save buffer
<kbd>F5</kbd>       | find file (also Dired)
<kbd>F6</kbd>       | find file in other window (also Dired)
<kbd>F7</kbd>       | toggle French input method
<kbd>F8</kbd>       | toggle Russian input method
<kbd>F9</kbd>       | kill or bury alive
<kbd>F10</kbd>      | delete other windows
<kbd>F11</kbd>      | switch to buffer
<kbd>F12</kbd>      | exit Emacs
<kbd>escape</kbd>   | delete window
<kbd>return</kbd>   | avy go to char
<kbd>S-up</kbd>     | move buffer up
<kbd>S-down</kbd>   | move buffer down
<kbd>S-left</kbd>   | move buffer left
<kbd>S-right</kbd>  | move buffer right
<kbd>menu menu</kbd>| smex (translation to <kbd>M-x</kbd>)
<kbd>menu ,</kbd>   | beginning of the buffer
<kbd>menu .</kbd>   | end of the buffer
<kbd>menu /</kbd>   | rectangular selection
<kbd>menu 2</kbd>   | mark word
<kbd>menu 3</kbd>   | mark rest of line
<kbd>menu 5</kbd>   | mark paragraph
<kbd>menu SPC</kbd> | special insertion of abbreviation
<kbd>menu a b</kbd> | toggle abbrev mode
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
<kbd>menu g l</kbd> | goto line
<kbd>menu g n</kbd> | GNUS
<kbd>menu g r</kbd> | recursive grep
<kbd>menu h a</kbd> | highlight symbol: remove all
<kbd>menu h e</kbd> | hexl mode
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
<kbd>menu m a</kbd> | magit status
<kbd>menu m c</kbd> | magit clone
<kbd>menu m i</kbd> | magit init
<kbd>menu m d</kbd> | markdown mode
<kbd>menu m k</kbd> | make current project
<kbd>menu m n</kbd> | man
<kbd>menu n n</kbd> | narrow to region
<kbd>menu n w</kbd> | widen
<kbd>menu p a</kbd> | package autoremove
<kbd>menu p i</kbd> | install package
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
<kbd>menu v e</kbd> | show version in minibuffer
<kbd>menu v r</kbd> | split window (vertical)
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
<kbd>z</kbd>        | Dired: change to WDired mode
<kbd>C-c C-l</kbd>  | Markdown: export
<kbd>C-c C-v</kbd>  | Markdown: preview
<kbd>C-c C-l</kbd>  | TeX: export to PDF
<kbd>C-c C-v</kbd>  | TeX: preview (as PDF via OS-preferred tool)
<kbd>C-c C-l</kbd>  | Texinfo: export
<kbd>C-c C-v</kbd>  | Texinfo: preview
<kbd>C-backspace</kbd> | Smartparens: backward kill S-expression
<kbd>menu 2</kbd>   | Smartparens: select thing S-expression
<kbd>menu 4</kbd>   | Smartparens: add to previous S-expression
<kbd>M-d</kbd>      | Smartparens: kill S-expression
<kbd>M-k</kbd>      | Smartparens: hybrid kill
<kbd>M-f</kbd>      | Smartparens: forward S-expression
<kbd>M-b</kbd>      | Smartparens: backward S-expression

## Keyboard Layouts and Abbreviations

It's true that standard keyboards have too few keys. We should be able to
input many-many different fancy symbols and switch languages very easily,
with one key pressing. I use <kbd>F7</kbd> and <kbd>F8</kbd> for this
task. Every key performs switching to input method of some language or
disables input method of that language when this it's already active (it
also switches dictionaries used for spell checking, note that you need to
install `aspell` for that). This way I don't need to «cycle» through all the
languages — a horrible thing.

But there are also quite a few characters that don't belong to any
particular keyboard layout or language. To input them I use Emacs
abbreviations.

Here is the transformation table:

Abbreviation  | Result Character | Name of the character
------------  | ---------------- | ---------------------
`acc`        | ́                | accent mark
`apeq`       | ≈                | approximately equal
`bot`        | ⊥                | bottom
`bull`       | •                | bullet
`copy`       | ©                | copyright sign
`dagg`       | †                | dagger
`dagr`       | ‡                | crossed dagger
`dash`       | —                | em dash
`dda`        | ⇓                | double downwards arrow
`deg`        | °                | degree
`dla`        | ⇐                | double leftwards arrow
`dqu`        | “”               | double quotation marks
`dra`        | ⇒                | double rightwards arrow
`dua`        | ⇑                | double upwards arrow
`elli`       | …                | ellipsis
`fleu`       | ❧                | fleuron
`guil`       | «»               | guillemets
`hash`       | #                | number sign or hash sign
`id`         | ≡                | identical to
`ineg`       | ∫                | integral
`ineq`       | ≠                | inequality
`inf`        | ∞                | infinity
`inte`       | ‽                | interrobang
`intr`       | ·                | interpunct
`keyb`       | ⌨                | keyboard
`loze`       | ◊                | lozenge
`mnpl`       | ∓                | minus-plus
`mult`       | ×                | multiplication
`nabl`       | ∇                | nabla
`ndsh`       | –                | en dash
`num`        | №                | numero sign
`obel`       | ÷                | obelus
`plmn`       | ±                | plus-minus
`pnd`        | £                | pound
`prod`       | ∏                | product
`qed`        | ■                | quod erat demonstrandum
`root`       | √                | root
`rub`        | ₽                | Russian ruble
`sda`        | ↓                | simple downwards arrow
`sect`       | §                | section
`squi`       | ‹›               | single arrow guillements
`sla`        | ←                | simple leftwards arrow
`squ`        | ‘’               | single quotation marks
`sra`        | →                | simple rightwards arrow
`srcp`       | ℗                | sound recording copyright
`star`       | ★                | star
`sua`        | ↑                | simple upwards arrow
`sum`        | ∑                | summation

There is also entire Greek alphabet:

Abbreviation | Letter | Abbreviation  | Letter
------------ | ------ | ------------  | ------
`Alpha`     | Α      | `alpha`      | α
`Beta`      | Β      | `beta`       | β
`Gamma`     | Γ      | `gamma`      | Y
`Delta`     | Δ      | `delta`      | δ
`Epsilon`   | Ε      | `epsilon`    | ε
`Zeta`      | Ζ      | `zeta`       | ζ
`Eta`       | Η      | `eta`        | η
`Theta`     | Θ      | `theta`      | θ
`Iota`      | Ι      | `iota`       | ι
`Kappa`     | Κ      | `kappa`      | κ
`Lambda`    | Λ      | `lambda`     | λ
`Mu`        | Μ      | `mu`         | μ
`Nu`        | Ν      | `nu`         | ν
`Xi`        | Ξ      | `xi`         | ξ
`Omicron`   | Ο      | `omicron`    | ο
`Pi`        | Π      | `pi`         | π
`Rho`       | Ρ      | `rho`        | ρ
`Sigma`     | Σ      | `sigma`      | σ
`Tau`       | Τ      | `tau`        | τ
`Upsilon`   | Υ      | `upsilon`    | υ
`Phi`       | Φ      | `phi`        | φ
`Chi`       | Χ      | `chi`        | χ
`Psi`       | Ψ      | `psi`        | ψ
`Omega`     | Ω      | `omega`      | ω

Final sigma ς is written as `fsigma`.

Note that abbreviations are better than key bindings à la <kbd>C-x 8
…</kbd>, because the key bindings are hard to remember and their number is
insufficient. So, abbreviations with readable names in the the way to go.

However the method of expansion of the abbreviations is not that good at
all. It's flawed at least in the following ways:

1. User cannot write and expand abbreviations in arbitrary context, she
   needs to surround them with «non-word-constituent-characters»: spaces or
   punctuation. So this thing won't work:

   ```
   “It's difficultelli” ≠ “It's difficult…”
   ```

2. When using input method for non-Latin languages user needs to disable
   input method, type an abbreviation, enable input method again — this is
   rather awkward.

I've written a function that performs insertion of abbreviation and its
expansion. This function solves all the problems. Just type <kbd>menu
SPC</kbd> and enter abbreviation, finish typing with <kbd>SPC</kbd> and
abbreviation will be inserted and expanded. Point is usually placed after
expansions, but the function is smart enough to place point in the middle of
two-character expansions that are pairs of quotes, like ‘«»’; wrapping of
selected text also works for this kind of expansions. Since user writes
abbreviation in the minibuffer, current input method has no effect. Also,
user can repeat insertion of previously used abbreviations by pressing just
<kbd>menu SPC SPC</kbd>.

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
before I switched to `DejaVu Sans Mono`. The reason for switching is that
there is no normal version of the font supporting Cyrillic script (yes, I've
tried modifications à la Inconsolata LCG, they suck or I'm not sufficiently
dedicated person to make them look normally). I also enable `hl-line-mode`
in modes like `dired` for prettiness.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
