# Emacs Configuration Files

My Emacs and GNUS configuration files. I wrote these files slowly borrowing
useful things that I saw in Emacs configuration files of other people and
official GNU Emacs manuals. Something is taken from so-called 'Emacs
Prelude'. I don't like it, I think it has too many unnecessary lines of code
for me (OS X and Windows stuff, for example, who in sane mind would buy a
Mac or setup such an inferior system as MS Windows? But main disadvantage of
these systems is that they do not respect users' freedom).

Also, here is some automation to install all necessary packages and some
automation to compile/recompile SLIME (works smoothly with Quicklisp).

This `.emacs` file also defines function to search Internet with
DuckDuckGo. It generates such URLs so DuckDuckGo operates in HTML mode,
without any proprietary Java Script. Also the URLs include parameters to
remove ads (well, a sponsored link in results is not a big deal anyway). It
also forces DuckDuckGo work only via HTTPS. By the way, it's a good idea to
try GNU IceCat, because it has a feature 'HTTPS everywhere' and many other
useful features, way better than Google Chrome.

In this file you will find only really convenient things that I'm used
to. Well, your taste may be different, or course, but I hope that at least
partly this `.emacs` and `.gnus` files will be useful to you.

To use spell-checking you need to install `aspell` from repositories of your
favorite GNU/Linux distribution. It's a good idea to use fully free
GNU/Linux system that uses Linux-libre kernel and has no proprietary
software on it.

# License

Copyright (c) 2015 Mark Karpov

Distributed under GNU GPL, version 3.
