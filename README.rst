|build-status|

This package provides thin bindings to libgit2. To use these bindings,
issue a call to (require 'libgit2). This will load the dynamic module,
or prompt the user to build it.

.. |build-status|
   image:: https://github.com/dickmao/libgit2.el/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/libgit2.el/actions
   :alt: Build Status

Install
=======
Install libgit2 as per your package manager.  Or `build it from source`_.  Then::

   git clone https://github.com/dickmao/libgit2.el.git
   make -C libgit2.el install

.. _build it from source: https://libgit2.org/docs/guides/build-and-link/
