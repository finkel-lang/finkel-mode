finkel-mode
===========

[![test](https://github.com/finkel-lang/finkel-mode/workflows/test/badge.svg)](https://github.com/finkel-lang/finkel-mode/actions?query=workflow%3Atest)
[![codecov](https://codecov.io/gh/finkel-lang/finkel-mode/branch/master/graph/badge.svg)](https://codecov.io/gh/finkel-lang/finkel-mode)

This repository contains Emacs Lisp codes for ``finkel-mode``, a major
mode for editing [Finkel][finkel] code with Emacs.


Installation
------------

At the time of writing, ``finkel-mode`` is not yet available as a
package from a public repository such as [MELPA][melpa]. So one needs to
install from the source.

To install manually, clone this repository:

```console
$ git clone https://github.com/finkel-lang/finkel-mode
```

and add the cloned directory to the ``load-path``. E.g.: add below to
``~/.emacs``:

```elisp
(add-to-list 'load-path "/path/to/the/cloned/repository/")
(autoload 'finkel-mode "finkel-mode" nil t)
```

Usage
-----

To start an interactive session, open a file with ``.fnk`` extension,
then hit ``Ctrl-z``.

This will ask the user to run a REPL via ``stack exec`` with given
``stack.yaml``, or via ``cabal v2-exec`` with given ``cabal.project``,
or use the ``finkel`` executable available in the system.


Contributing
------------

Contributions are welcome. Please see the
[CONTRIBUTING.md][contributing].


[finkel]: https://finkel.readthedocs.org
[melpa]: https://melpa.org/
[contributing]: https://github.com/finkel-lang/finkel-mode/blob/master/CONTRIBUTING.md#contributing
