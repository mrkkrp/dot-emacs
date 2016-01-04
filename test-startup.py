#!/usr/bin/env python
#
# Test that my Emacs configuration starts and compiles. This should work in
# Python 2.7 and 3.X.

from __future__ import print_function
import subprocess

initial_startup = '''
(let ((debug-on-error t)
      (url-show-status nil)
      (user-emacs-directory default-directory)
      (user-init-file (expand-file-name "init.el"))
      (load-path (delq default-directory load-path)))
  (load-file user-init-file)
  (run-hooks (quote after-init-hook))
  (mk-compile-init-files))
'''

secondary_startup = '''
(let ((debug-on-error t)
      (url-show-status nil)
      (user-emacs-directory default-directory)
      (user-init-file (expand-file-name "init.el"))
      (load-path (delq default-directory load-path)))
  (load-file user-init-file)
  (run-hooks (quote after-init-hook)))
'''

def start_emacs(title, eval_str):
    """
    Try to start Emacs in batch mode and without window system. Fail loudly.
    """
    print('Starting', title)
    subprocess.check_call(['emacs', '-nw', '--batch', '--eval', eval_str])
    print('Done:', title, end='\n\n')

try:
    subprocess.check_call(['emacs', '--version']) ; print()
    start_emacs('initial startup', initial_startup)
    start_emacs('secondary startup', secondary_startup)
except subprocess.CalledProcessError as e:
    exit(e.returncode)
else:
    print('Startup testing finished successfully')
