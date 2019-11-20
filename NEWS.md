ppgames 0.3.0
=============

New features
------------

* ``df_fide_chess`` and ``df_alice_chess`` now have a new argument ``has_subpack`` which if ``TRUE``
  will provide suitable piecepack "stackpack" diagrams.
* Starting board generators for specific games:
  - ``df_alien_city`` (#8)
  - ``df_triactor`` (#36)
* Portable Piecepack Notation enhancements:
  - Simplified piece notation now supports playing cards expansion (#30),
    icehouse pieces (#34), (stackpack) subpack,
    dual piecepacks expansion, and hexpack.

Breaking changes
----------------

* Arguments for starting board generators have been changed.  
  In particular functions no longer takes piecepackr configuration list objects as arguments but 
  instead uses arguments like ``has_matchsticks`` to determine how to customize output.

ppgames 0.2.1
=============

New features
------------

* Added Unicode plaintext piecepack diagram generating function ``cat_piece``.
  Thanks James Vipond for some Unicode character suggestions.
* Added prototype "Portable Piecepack Notation utilities" ``read_ppn``, ``animate_game``,
  ``plot_move``, and ``cat_move`` (#11).
* Starting board generators for specific games:
    - ``df_cell_management``
    - ``df_everest``
    - ``df_fujisan`` (#24)
    - ``df_twelve_mens_morris``
* Fuji-san solver ``solve_fujisan`` (#22).

Breaking changes
----------------

* ``df_ultima_chess`` renamed ``df_ultima``, ``df_baroque_chess`` added as an alias.

ppgames 0.1.0
=============

* Initial release of R package version.
* Flexible rectangular starting board generators:
    - ``df_rect_board_tiles`` and ``grid.board_rect_tiles``
    - ``grid.board_rect_cells``and ``grid.board_rect_points``
* Starting board generators for specific games:
    - ``df_alice_chess``
    - ``df_american_checkers``
    - ``df_backgammon``
    - ``df_chaturaji``
    - ``df_cribbage_board`` and ``textGrob_cribbage_board`` for textual annotation (#7)
    - ``df_fide_chess``
    - ``df_four_field_kono``
    - ``df_four_seasons_chess``
    - ``df_nine_mens_morris``
    - ``df_shogi``
    - ``df_tablut`` (#10)
    - ``df_ultima_chess``
    - ``df_xiangqi``
* Preliminary work on an GameKit class ``game_kit``
* Prototype ruleset/rulebook generators ``save_ruleset`` and ``save_rulebook``
