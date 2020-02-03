ppgames: Piecepack game diagrams and rules
==========================================
.. image:: https://travis-ci.org/piecepackr/ppgames.png?branch=master
    :target: https://travis-ci.org/piecepackr/ppgames
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/piecepackr/ppgames?branch=master&svg=true 
    :target: https://ci.appveyor.com/project/piecepackr/ppgames
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/piecepackr/ppgames/master.svg
    :target: https://codecov.io/github/piecepackr/ppgames?branch=master
    :alt: Coverage Status

.. image:: http://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: http://www.repostatus.org/#wip

``ppgames`` is an R_ package with functions that help generate piecepack_ game graphics, rulesets, and books as well as functions to parse "Portable Piecepack Notation" files and a Fuji-san solver.

.. _piecepack: http://www.ludism.org/ppwiki/HomePage
.. _piecepackr: https://github.com/piecepackr/piecepackr
.. _R: https://www.r-project.org/

.. contents::

Installation
------------

To install the ``ppgames`` R package use the following commands in R_:

.. code:: r

   install.packages("remotes")
   remotes::install_github("piecepackr/piecepackr")
   remotes::install_github("piecepackr/ppgames")

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

This package provides some prototype configurable rulesets and one work-in-progress rulebook.  One can use ``save_ruleset`` and ``save_rulebook`` to generate them:

.. code:: r

    library("piecepackr")
    library("ppgames")
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

.. code:: r

    cfg <- pp_cfg(list(suit_text="🌞,🌜,👑,⚜,꩜",
         suit_cex="0.6,0.7,0.75,0.9,0.9",
        suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
        suit_color="darkred,black,darkgreen,darkblue,black",
        border_lex=4, border_color="black", mat_width.tile_back=0.05, mat_color="white",
        invert_colors.suited = TRUE, edge_color.coin="tan", edge_color.tile="tan",
        shape.pawn="convex6", depth.pawn=1.0, height.pawn=0.75, width.pawn=0.75, dm_text.pawn=""
    ))
    pmap_piece(df_tablut(cfg), cfg=cfg, default.units="in", trans=op_transform, op_scale=0.7)

.. image:: https://www.trevorldavis.com/piecepackr/share/rules/tablut.png
    :alt: Diagram for a game of Tablut
    :align: center

Plaintext Unicode Piecepack Diagrams
------------------------------------

This package provides a prototype plaintext Unicode diagram generator.  One can use ``cat_piece`` to print out diagrams to the terminal using the same ``data.frame`` input accepted by ``piecepackr::pmap_piece`` or one can use ``cat_move`` to print out diagrams using the parsed PPN games provided by ``read_ppn``:

.. code:: r

    cat_piece(df_fide_chess())

.. image:: https://trevorldavis.com/share/piecepack/unicode_piecepack_alt_5.png
    :alt: Unicode text diagram for Chess
    :align: center

.. code:: r

    cat_piece(df_xiangqi())

.. image:: https://trevorldavis.com/share/piecepack/unicode_xiangqi.png
    :alt: Unicode text diagram for Xiangqi
    :align: center


Portable Piecepack Notation
---------------------------

This package provides a prototype `Portable Piecepack Notation <https://trevorldavis.com/piecepackr/portable-piecepack-notation.html>`_ parser.  One can use ``read_ppn`` to parse a PPN file and use ``animate_game``, ``plot_move``, and ``cat_move`` to visualize the moves in a parsed game:

.. code:: r

    game <- read_ppn(system.file("ppn/four-field-kono.ppn", package="ppgames"))[[1]]
    animate_game(game)
    plot_move(game)
    cat_move(game)

Game Solvers
------------

This package provides a Fuji-san solver ``solve_fujisan`` which can compute the shortest solution (if it exists) to a given Fuji-san puzzle and output the PPN text to record/visualize the solution.

.. code:: r

    puzzle2 <- matrix(c(4,4,4,5,2,0,2,4,0,3,1,1,
                        1,2,5,3,3,5,3,2,5,1,0,0), nrow=2, byrow=TRUE)
    s2 <- solve_fujisan(coins=puzzle2)
    game <- read_ppn(textConnection(s2$ppn))[[1]]

    dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
                          invert_colors.suited=TRUE, border_color="black", border_lex=2)
    piecepack_suits <- list(suit_text="\U0001f31e,\U0001f31c,\U0001f451,\u269c,\uaa5c", # 🌞,🌜,👑,⚜,꩜
                        suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
                        suit_cex="0.6,0.7,0.75,0.9,0.9")
    traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
    cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=0.375, 
                       dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
                       edge_color.coin="tan", edge_color.tile="tan")
    cfg <- pp_cfg(c(piecepack_suits, dark_colorscheme, traditional_ranks, cfg3d))

    animate_game(game, op_scale=1, op_angle=90, trans=op_transform, cfg=cfg, file="fujisan.gif")

.. image:: https://www.trevorldavis.com/piecepackr/images/knitr/fujisan.gif
    :alt: Animation of a Fuji-san game
    :align: center
