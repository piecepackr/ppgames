ppgames: Piecepack game diagrams and rules
==========================================

``ppgames`` is an R_ package with functions that help generate piecepack_ game graphics, rulesets, and books as well as functions to parse `Portable Piecepack Notation (PPN) files <https://trevorldavis.com/piecepackr/portable-piecepack-notation.html>`_ and a `Fuji-san <https://www.ludism.org/ppwiki/Fuji-san>`_ solver.  This package uses the piecepackr_ package.

``ppgames`` is **superseded** in favor of the newer ppcli_, ppdf_, ppgamer_, ppn_, and pprules_ spinoff packages:

* These can all be installed with ``piecepackr::install_ppverse()``
* See <https://piecepackr.r-universe.dev/builds> for more info on the various "ppverse" packages
* Use ``ppcli::cat_piece()`` for ``cat_piece()``
* Use similar functions from ppdf_ for the various ``df_*()`` game setup functions
* Use ``ppgamer::solve_fujisan()`` for ``solve_fujisan()``
* Use ppn_ for various ``*_game()``, ``*_ppn()``, and ``*_move()`` PPN parsing and game visualization functions
* Use pprules_ for various ``save_*()`` piecepack_ ruleset generators

.. _piecepack: http://www.ludism.org/ppwiki/HomePage
.. _piecepackr: https://github.com/piecepackr/piecepackr
.. _ppcli: https://github.com/piecepackr/ppcli
.. _ppdf: https://github.com/piecepackr/ppdf
.. _ppgamer: https://github.com/piecepackr/ppgamer
.. _ppn: https://github.com/piecepackr/ppn
.. _pprules: https://github.com/piecepackr/pprules
.. _R: https://www.r-project.org/
