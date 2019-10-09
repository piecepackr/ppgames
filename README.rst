ppgames: Piecepack game diagrams and rules
==========================================
.. image:: https://travis-ci.org/trevorld/ppgames.png?branch=master
    :target: https://travis-ci.org/trevorld/ppgames
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/trevorld/ppgames?branch=master&svg=true 
    :target: https://ci.appveyor.com/project/trevorld/ppgames
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/trevorld/ppgames/master.svg
    :target: https://codecov.io/github/trevorld/ppgames?branch=master
    :alt: Coverage Status

.. image:: http://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: http://www.repostatus.org/#wip

``ppgames`` is an R_ package with functions that help generate piecepack_ game graphics, rulesets, and books as well as functions to parse "Portable Piecepack Notation" files and a Fuji-san solver.

.. _piecepack: http://www.ludism.org/ppwiki/HomePage
.. _piecepackr: https://github.com/trevorld/piecepackr
.. _R: https://www.r-project.org/

Installation
------------

To install the ``ppgames`` R package use the following commands in R_:

.. code:: r

   install.packages("remotes")
   remotes::install_github("trevorld/piecepackr")
   remotes::install_github("trevorld/ppgames")

If you want to make rulebooks you'll need ``xelatex`` and if you want to make rulesets you'll need a recent version of ``pandoc``.  The following instructions should work on Ubuntu:

.. code:: bash

    sudo apt install texlive-xetex
    sudo apt install cabal-install
    cabal update # add $HOME/.cabal/bin to $PATH
    cabal install pandoc

License
-------

Unless otherwise specified everything in this repo is licensed under the `CC BY-SA 4.0 license <https://creativecommons.org/licenses/by-sa/4.0/>`_.

Rulebooks and Rulesets
----------------------

This package provides some prototype configurable rulesets and one work-in-progress rulebook.  One can use ``save_ruleset`` and ``save_rulebook`` to generate them::

    cfg <- pp_cfg() # replace with your favoured configuration
    gk <- game_kit(cfgs=list(cfg=cfg))
    save_ruleset("nine-mens-morris", gk=gk)

    download.file("https://trevorldavis.com/piecepackr/share/demo_cfgs.RData", "demo_cfgs.RData")
    cfgs <- new.env()
    load("demo_cfgs.RData", envir=cfgs)
    gk <- game_kit(cfgs=list(cfg=cfgs$orthodox1))
    save_rulebook("the-historical-piecepacker", gk=gk)

Starting Boards
---------------

This package provides several starting boards in the data frame format accepted by ``piecepackr::pmap_piece`` and ``cat_piece`` that can also be used to setup games in the PPN format.  See the file ``NEWS.md`` for the complete list or look up the ``starting_boards`` man page in the package documentation.  It also contains some arbitrary rectangular board constructors that can be used to setup a variety of rectangular boards: ``df_rect_board_tiles`` as well as ``grid.board_rect_tiles`` and (to generate non-piecepack rectangular boards) ``grid.board_rect_cells`` and ``grid.board_rect_points``.

Plaintext Unicode Piecepack Diagrams
------------------------------------

This package provides a prototype plaintext Unicode diagram generator.  One can use ``cat_piece`` to print out diagrams to the terminal using the same ``data.frame`` input accepted by ``piecepackr::pmap_piece`` or one can use ``cat_move`` to print out diagrams using the parsed PPN games provided by ``read_ppn``::

    cat_piece(df_fide_chess())
    cat_piece(df_xiangqi())


Portable Piecepack Notation
---------------------------

This package provides a prototype `Portable Piecepack Notation <https://trevorldavis.com/piecepackr/portable-piecepack-notation.html>`_ parser.  One can use ``read_ppn`` to parse a PPN file and use ``animate_game``, ``plot_move``, and ``cat_move`` to visualize the moves in a parsed game::

    ppn <- read_ppn(system.file("extdata/ex1.ppn", package="ppgames"))
    game <- ppn[[1]]
    animate_game(game)
    plot_move(game)
    cat_move(game)

Game Solvers
------------

This package provides a Fuji-san solver ``solve_fujisan`` which can compute the shortest solution (if it exists) to a given Fuji-san puzzle and output the PPN text to record/visualize the solution.
