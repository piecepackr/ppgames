ppgames 0.6.0
=============

New features
------------

* New starting board generators for specific games:

  - ``df_chinese_checkers()``
  - ``df_evade()``
  - ``df_iceberg()``
  - ``df_grasshopper()``
  - ``df_jul_gono()``
  - ``df_pass_the_food()``
  - ``df_tower_of_babel()``

* ``save_ruleset()`` / ``save_pamphlet()`` now supports ruleset generation for:

    - "pass the food"

Bug fixes and minor improvements
--------------------------------

* Checkers set-up is now "mirrored" (pieces on bottom-left instead of bottom-right)
  to match traditional checkers set-up.

Breaking Changes
----------------

* Interpretation of the ``game`` / ``book`` argument in ``save_ruleset``, ``save_pamphlet``, ``save_rulebook`` now
  follows similar rules as the ``GameType`` argument in PPN,
  in particular now use arguments like ``"American Checkers"`` instead of ``"american-checkers"``.

ppgames 0.5.1
=============

New features
------------

* New starting board generators for specific games:

  - ``df_alquerque()``
  - ``df_awithlaknannai_mosona()``
  - ``df_japan()``
  - ``df_tula()``

* Portable Piecepack Notation enhancements:

  - Can now identify pieces within a stack of pieces with square brackets e.g. ``*b4[2:3]`` (#18)
  - Can now "drop" pieces beneath other pieces with backslash e.g. ``S\b2`` (#53)
  - Can now "move" pieces beneath other pieces with underscore or combining double breve below e.g. ``b2_b4`` (#53)
  - Can now restrict where in the piece order a piece will be after a drop/move with percent sign 
    to indicate the piece it should go before (or after) in the internal piece ordering e.g.
    ``b2@b3%e4[2]`` (#53)
  - There are also new shortcut tokens ``@%``, ``\%``, ``-%``, ``_%`` for the common case
    where you want to move/drop above/beneath a specific piece e.g. ``b4@%b5[2]`` 
    is equivalent to ``S@&b5[2]%b5[2]`` which is equivalent to ``S@b5%b5[2]`` (#53)
  - Can now identify pieces with a "non-greedy" search ``?`` (#55) or a "greedy" search ``/``.
  - Can now partially update specification of a piece with ``~`` (#57)
  - ``;`` can now be used to indicate the insertion of the minimal move number `` . `` which
    has been interpreted to mean the previous move number with an extra ``.`` tacked to the end.
  - Can now have identical ``MoveNumbers`` but will emit a warning if identical ``MoveNumbers`` are found.
  - Can now directly set piece attributes for suit, rank, angle, and configuration (#35)
  - Can now specify the following additional game systems (#59):

    + standard six-sided dice (in six colors)
    + (French) Tarot playing cards (plus 3 types of Jokers),
      this is a superset of the standard deck of playing cards
    + (double-6) dominoes ('standard' set plus sets in six colors)
    + checkers (for 1" and 2" cell sizes, in six colors)
    + chess (for 1" and 2" cell sizes, in six colors)
    + go (with stones in six colors)
    + meeples (in six colors)
    + piecepack dice cards

  - Can now define and use macros (#60)

Bug fixes and minor improvements
--------------------------------

* ``plot_move()`` and ``animate_game()`` improvements:

  - The ``annotate`` option can now also be set to ``"cartesian"``.
  - Both ``plot_move()`` and ``animate_game()`` will now automatically adjust dimensions and coordinates 
    if (parts of) pieces would otherwise be drawn too close (or past) zero on either the
    x or y axes.
  - If ``file == NULL`` then ``animate_game()`` will set ``devAskNewPage(TRUE)``
    and then will plot each move of the game in a new graphics device (if ``new_device=TRUE``) or
    the current graphics device asking user to press enter before drawing the next move (#41).
  - If ``file`` ends in ``.html`` then ``animate_game()`` will use ``animation::saveHTML()``.
  - If ``file`` ends in an file format that isn't ``.gif`` or ``.html`` (such as ``.mp4``)
    then ``animate_game()`` will use ``animation::saveMovie()``.
  - Pixel dimensions of ``animate_game()`` are now always adjusted to be an even number
    (a requirement of ``.mp4`` animations).
  - Can now manually set ``width``, ``height``, and ``ppi`` arguments.
  - New argument ``.f`` that allows different graphic functions to be used
    (in particular ``piecepackr::piece3d()`` and ``piecepackr::piece()`` (#54).

* Tiles are now spread out more in ``df_desfases()`` and its "scale factor" attribute has been set to 3. 

* ``save_ruleset() / save_pamphlet()`` can now set PDF metadata (Title, Author, Subject, Keywords, Creator) (#69)

* ``save_ruleset`` supports ruleset generation for:

    - alquerque

Breaking Changes
----------------

* ``res`` argument in ``plot_move()`` renamed to ``ppi`` (to avoid possible clash with ``res`` argument in ``piece3d`` or ``piece``).
* ``△`` (U+25b3) no longer part of PPN.  
  Instead use ``▲`` (U+25b2) or ``/\`` plus a color "suit" (RKGBYW) to indicate Icehouse pyramids.

ppgames 0.4.1
=============

* ``animate_game`` now has ``n_transitions`` argument that uses ``tweenr``
  to try to interpolate transition frames (#32).
* New starting board generators for specific games:

  - ``df_breakthrough()``
  - ``df_crossings()``
  - ``df_froggy_bottom()``
  - ``df_ley_lines()``
  - ``df_lines_of_action()``
  - ``df_piecepackman()``
  - ``df_turkish_draughts()``

* Portable Piecepack Notation enhancements:

  - Can now (re)move a stack of pieces e.g. ``2b2-d4``, ``*3d5``, ``b4:2d5``.
  - Default parser now uses both ``GameType`` and ``SetUp`` to create starting setup (#49).
  - Can now specify alternate movetext parsers with ``MovetextParser`` (#50).
  - Can now specify game ``System`` in ``GameType``/``SetUp`` fields.
  - Can now specify ``SetUp: None`` (or ``GameType: None``).
  - Can now specify ``ScalingFactor`` in the default movetext parser or as
    an ``attribute`` of the starting data frame (#52).
  - ``PieceId`` can now refer to piece at beginning of the move with ``^``.
  - ``Location`` can now be ``&PieceId``.
  - Can now rotate pieces with ``@>`` move token.
  - Can now "swap" pieces with ``#`` move token.

Bug fixes and minor improvements
--------------------------------

* ``cat_piece()`` now has basic support for the following ``game_systems``:

  - ``bit`` and ``board`` components from ``checkers1`` and ``checkers2``
  - ``tile``  component from ``dominoes``, ``dominoes_black``, ``dominoes_blue``
    ``dominoes_green``, ``dominoes_red``, ``dominoes_white``, ``dominoes_yellow``.
    Only supports double-6 domino ranks (and not the remaining double-12 domino ranks).
  - ``die`` component from ``dice``
  - ``pyramid`` component from ``icehouse_pieces``
  - ``tile`` component from ``subpack``

* ``cat_piece()`` now also has basic support for (piecepack) matchsticks.

Breaking changes
----------------

* In PPN simplified piece notation ``1``, ``2``, ``3`` corresponds to icehouse pieces rank 1, 2, 3 in contrast to piecepack rank 2, 3, and 4 i.e. zero-pip icehouse pyramids have been removed.

ppgames 0.3.1
=============

New features
------------

* ``df_alice_chess()``, ``df_chaturaji()``, ``df_fide_chess()``, ``df_four_seasons_chess()``, 
  ``df_shogi()``, ``df_ultima()``, ``df_xiangqi()``  now have a new argument 
  ``has_subpack`` which if ``TRUE`` will provide alternative piecepack "stackpack" diagrams.
* New starting board generators for specific games:

  - ``df_alien_city()`` (#8)
  - ``df_desfases()``
  - ``df_ice_floe()``
  - ``df_plans_of_action()``
  - ``df_relativity()``
  - ``df_san_andreas()``
  - ``df_salta()``
  - ``df_the_in_crowd()``
  - ``df_triactor()`` (#36)
  - ``df_wormholes()``

* Aliases added for some existing starting board generators:

  - ``df_checkers()`` and ``df_english_draughts()`` for ``df_american_checkers()``
  - ``df_chess()`` for ``df_international_chess()``

* Portable Piecepack Notation enhancements:

  - Simplified piece notation now supports playing cards expansion (#30),
    icehouse pieces (#34), (stackpack) subpack,
    dual piecepacks expansion, and hexpack.

* New function ``save_pamphlet()`` which is an alternative to ``save_ruleset()``
  that saves the ruleset as a trifold pamphlet.

Breaking changes
----------------

* Arguments for starting board generators have been changed.  
  In particular functions no longer takes piecepackr configuration list objects as arguments but 
  instead uses arguments like ``has_matchsticks`` to determine how to customize output.
* Instead of an ``output_dir`` argument both ``save_ruleset`` and ``save_rulebook`` now use
  an ``output`` file argument.
* ``df_fide_chess()`` renamed ``df_international_chess()``, corresponding ruleset was renamed 
  international-chess.

Bug fixes and minor improvements
--------------------------------

* Fixes bug in starting Ultima diagram generated by ``df_ultima()``.
* Fixes bug in color of coin faces in Unicode plaintext diagrams 
  if placed above previously colored piece.
* ``cat_piece()`` and ``cat_move()`` now has limited support for a "cfg" column 
  "playing_cards_expansion" and "dual_piecepacks_expansion".
* Minor improvements to ``save_ruleset()``/``save_rulebook()`` output.  
  In particular in addition to the default "letter" paper size can now specify "A4" output.
* Improved rulesets for "american checkers".

ppgames 0.2.1
=============

New features
------------

* Added Unicode plaintext piecepack diagram generating function ``cat_piece()``.
  Thanks James Vipond for some Unicode character suggestions.
* Added prototype "Portable Piecepack Notation utilities" ``read_ppn()``, ``animate_game()``,
  ``plot_move()``, and ``cat_move()`` (#11).
* Starting board generators for specific games:

    - ``df_cell_management()``
    - ``df_everest()``
    - ``df_fujisan()`` (#24)
    - ``df_twelve_mens_morris()``

* Fuji-san solver ``solve_fujisan()`` (#22).

Breaking changes
----------------

* ``df_ultima_chess()`` renamed ``df_ultima()``, ``df_baroque_chess()`` added as an alias.
  Corresponding ruleset was renamed "ultima".

ppgames 0.1.0
=============

* Initial release of R package version.
* Flexible rectangular starting board generators:

    - ``df_rect_board_tiles()`` and ``grid.board_rect_tiles()``
    - ``grid.board_rect_cells()``and ``grid.board_rect_points()``

* Starting board generators for specific games:

    - ``df_alice_chess()``
    - ``df_american_checkers()``
    - ``df_backgammon()``
    - ``df_chaturaji()``
    - ``df_cribbage_board()`` and ``textGrob_cribbage_board()`` for textual annotation (#7)
    - ``df_fide_chess()``
    - ``df_four_field_kono()``
    - ``df_four_seasons_chess()``
    - ``df_nine_mens_morris()``
    - ``df_shogi()``
    - ``df_tablut()`` (#10)
    - ``df_ultima_chess()``
    - ``df_xiangqi()``

* Preliminary work on an GameKit class ``game_kit``
* Prototype ruleset/rulebook generators ``save_ruleset`` and ``save_rulebook``
* ``save_ruleset`` supports ruleset generation for:

    - alice chess
    - american checkers
    - backgammon
    - cribbage
    - chaturaji
    - fide chess
    - four field kono
    - four seasons chess
    - nine mens morris
    - tablut
    - twelve mens morris
    - ultima chess
    - xiangqi
