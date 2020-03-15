finkel-mode
===========

This repository contains Emacs Lisp codes for ``finkel-mode``, a major
mode for editing [Finkel][finkel] source code with Emacs.


Installation
------------

At the time of writing, ``finkel-mode`` is not yet available as a
package from public repository such as [MELPA][melpa]. So one need to
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

[finkel]: https://finkel.readthedocs.org
[melpa]: https://melpa.org/
