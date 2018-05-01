Shogi
=====

.. |date| date::

:Author: Trevor L Davis
:Players: 2
:Length: 30 minutes to 2 hours
:Required equipment: two piecepacks
:Version: 0.4
:Date: |date|
:License: CC-BY-SA 4.0

*Shogi* (aka *Japanese Chess* or the *Game of Generals*) is a popular chess variant notable for its "drop rule" which allows captured pieces to be returned back onto the board by the capturing players and an intricate promotion scheme for its pieces.

.. contents::

Equipment
---------

The board(s)
~~~~~~~~~~~~

You'll need 16 tile backs to make a square board (you'll be playing on the lines).  Some players will like to take an extra twenty tile faces to make a border around it so it is harder for the pieces to fall off the edge.  One can also take extra tiles and build two smaller boards off the side of the main board to serve as "piece stands" to hold captured pieces.

The pieces (standard piecepack)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are playing with two standard piecepacks then each player should take:

* one piecepack pawn 
* one '3' coin face
* one '2' coin face
* two piecepack dice (showing the '5' face)
* two '5' coin faces
* two 'a' coin faces
* two '4' coin faces
* nine coin backs

Arrange them as follows:

.. figure:: diagrams/shogi_traditional.pdf
   :alt: Initial Shogi layout

   Initial Shogi layout using two standard piecepacks.


The pieces ("chess" piecepack)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are playing with a pair of `"mirrored" color scheme, six-suited
chess-ranked piecepacks <https://github.com/trevorld/piecepack#chess>`_
(henceforth refered to as a "chess" piecepack) then each player should
take:

* one black piecepack pawn 
* one black 'rook' coin face
* one black 'bishop' coin face
* two black dice (showing the 'king' faces)
* two black 'king` coin faces
* two black 'knight' coin faces
* two black 'queen' coin faces
* nine black coin backs
* one red coin analogue for every black coin taken (these will be subbed in later to indicate that a piece has been promoted)

Arrange them as follows:

.. figure:: diagrams/shogi_chess.pdf
   :alt: Initial Shogi layout

   Initial Shogi layout using a pair of "mirrored" color scheme, six-suited chess-ranked piecepacks.

Indicating Ownership
--------------------

Except for the Kings (represented by piecepack pawns) every other shogi piece
can be captured and used by the other player and hence ownership of
a (non-King) piece is indicated by orienting the piece towards the opposite side of the board when placed on the board.  The direction of the piecepack dice is the direction above the flat top of the "5".  Every shogi piece being represented by a piecepack coin (not the "Kings" or "Golds") can be promoted to a "promoted" version if it moves into or out of the last three ranks of the board. If playing with standard piecepacks show that a piece is promoted by orienting it 90 degrees to the right - if each player sits towards their left corner of the board then each piece whether promoted or not will still be oriented in the general direction of their opponent.  If playing with the "chess" piecepack then simply sub in the red version of the coin upon promotion (making sure to preserve orientation towards the opposite side of the board).  Since every coin/saucer in the 'chess' piecepack has a red side and a black side please note that "red" in these instructions means the part facing up is red and "black" means the part facing up is black: this ensures that when playing with the "chess" piecepack that all promoted pieces are red and all unpromoted pieces are black (as is the case with a traditional shogi chess set).

The pieces
----------

.. list-table:: Shogi Pieces
   :header-rows: 1
   :widths: 23 08 15 28 28

   * - Name
     - Abbr.
     - `Betza <https://en.wikipedia.org/wiki/Betza%27s_funny_notation>`_
     - Standard piecepack
     - "Chess" piecepack
   * - King (challenged)
     - K
     - K = FW
     - |sk1|
     - |ck1|
   * - King (challenger)
     - K
     - K = FW
     - |sk2|
     - |ck2|
   * - Rook
     - R
     - R = WW
     - |sru|
     - |cru|
   * - Promoted Rook
     - +R
     - FR = FWW
     - |srp|
     - |crp|
   * - Bishop
     - B
     - B = FF
     - |sbu|
     - |cbu|
   * - Promoted Bishop
     - +B
     - WB = WFF
     - |sbp|
     - |cbp|
   * - Gold (general)
     - G
     - WfF
     - |sgu|
     - |cgu|
   * - Silver (general)
     - S
     - FfW
     - |ssu|
     - |csu|
   * - Promoted Silver
     - +S
     - WfF
     - |ssp|
     - |csp|
   * - Knight
     - N
     - ffN
     - |snu|
     - |cnu|
   * - Promoted Knight
     - +N
     - WfF
     - |snp|
     - |cnp|
   * - Lance
     - L
     - fR = fWW
     - |slu|
     - |clu|
   * - Promoted Lance
     - +L
     - WfF
     - |slp|
     - |clp|
   * - Pawn
     - P
     - fW
     - |spu|
     - |cpu|
   * - Promoted Pawn
     - +P
     - WfF
     - |spp|
     - |cpp|

.. |sk1| image:: images/pdf/orthodox1/pawn_face_s3_t0.pdf
       :height: 0.50in
.. |ck1| image:: images/pdf/chess5/pawn_face_s6_t0.pdf
       :height: 0.50in
.. |sk2| image:: images/pdf/orthodox1/pawn_face_s4_t0.pdf
       :height: 0.50in
.. |ck2| image:: images/pdf/chess5/pawn_face_s5_t0.pdf
       :height: 0.50in
.. |sru| image:: images/pdf/orthodox1/coin_face_r4_t0.pdf
       :height: 0.50in
.. |cru| image:: images/pdf/chess6/coin_face_r4_t0.pdf
       :height: 0.50in
.. |srp| image:: images/pdf/orthodox1/coin_face_r4_t270.pdf
       :height: 0.50in
.. |crp| image:: images/pdf/chess5/coin_face_r4_t0.pdf
       :height: 0.50in
.. |sbu| image:: images/pdf/orthodox1/coin_face_r3_t0.pdf
       :height: 0.50in
.. |cbu| image:: images/pdf/chess6/coin_face_r3_t0.pdf
       :height: 0.50in
.. |sbp| image:: images/pdf/orthodox1/coin_face_r3_t270.pdf
       :height: 0.50in
.. |cbp| image:: images/pdf/chess5/coin_face_r3_t0.pdf
       :height: 0.50in
.. |sgu1| image:: images/pdf/orthodox1/ppdie_face_s1_r6_t0.pdf
       :height: 0.50in
.. |sgu2| image:: images/pdf/orthodox1/ppdie_face_s2_r6_t0.pdf
       :height: 0.50in
.. |sgu3| image:: images/pdf/orthodox1/ppdie_face_s3_r6_t0.pdf
       :height: 0.50in
.. |sgu4| image:: images/pdf/orthodox1/ppdie_face_s4_r6_t0.pdf
       :height: 0.50in
.. |sgu| replace:: |sgu1| |sgu2| |sgu3| |sgu4| 
       :height: 0.50in
.. |cgu1| image:: images/pdf/chess5/ppdie_face_s1_r6_t0.pdf
       :height: 0.50in
.. |cgu2| image:: images/pdf/chess5/ppdie_face_s2_r6_t0.pdf
       :height: 0.50in
.. |cgu3| image:: images/pdf/chess5/ppdie_face_s3_r6_t0.pdf
       :height: 0.50in
.. |cgu4| image:: images/pdf/chess5/ppdie_face_s4_r6_t0.pdf
       :height: 0.50in
.. |cgu5| image:: images/pdf/chess5/ppdie_face_s5_r6_t0.pdf
       :height: 0.50in
.. |cgu6| image:: images/pdf/chess5/ppdie_face_s6_r6_t0.pdf
       :height: 0.50in
.. |cgu| replace:: |cgu1| |cgu2| |cgu3| |cgu4| |cgu5| |cgu6|
.. |ssu2| image:: images/pdf/orthodox1/coin_face_r6_t0.pdf
       :height: 0.50in
.. |ssu| replace:: |ssu2|
.. |csu| image:: images/pdf/chess6/coin_face_r6_t0.pdf
       :height: 0.50in
.. |ssp2| image:: images/pdf/orthodox1/coin_face_r6_t270.pdf
       :height: 0.50in
.. |ssp| replace:: |ssp2|
.. |csp| image:: images/pdf/chess5/coin_face_r6_t0.pdf
       :height: 0.50in
.. |snu| image:: images/pdf/orthodox1/coin_face_r2_t0.pdf
       :height: 0.50in
.. |cnu| image:: images/pdf/chess6/coin_face_r2_t0.pdf
       :height: 0.50in
.. |snp| image:: images/pdf/orthodox1/coin_face_r2_t270.pdf
       :height: 0.50in
.. |cnp| image:: images/pdf/chess5/coin_face_r2_t0.pdf
       :height: 0.50in
.. |slu| image:: images/pdf/orthodox1/coin_face_r5_t0.pdf
       :height: 0.50in
.. |clu| image:: images/pdf/chess6/coin_face_r5_t0.pdf
       :height: 0.50in
.. |slp| image:: images/pdf/orthodox1/coin_face_r5_t270.pdf
       :height: 0.50in
.. |clp| image:: images/pdf/chess5/coin_face_r5_t0.pdf
       :height: 0.50in
.. |spu1| image:: images/pdf/orthodox1/coin_back_s1_t0.pdf
       :height: 0.50in
.. |spu2| image:: images/pdf/orthodox1/coin_back_s2_t0.pdf
       :height: 0.50in
.. |spu3| image:: images/pdf/orthodox1/coin_back_s3_t0.pdf
       :height: 0.50in
.. |spu4| image:: images/pdf/orthodox1/coin_back_s4_t0.pdf
       :height: 0.50in
.. |spu| replace:: |spu1| |spu2| |spu3| |spu4|
.. |cpu1| image:: images/pdf/chess5/coin_back_s1_t0.pdf
       :height: 0.50in
.. |cpu2| image:: images/pdf/chess5/coin_back_s2_t0.pdf
       :height: 0.50in
.. |cpu3| image:: images/pdf/chess5/coin_back_s3_t0.pdf
       :height: 0.50in
.. |cpu4| image:: images/pdf/chess5/coin_back_s4_t0.pdf
       :height: 0.50in
.. |cpu5| image:: images/pdf/chess5/coin_back_s5_t0.pdf
       :height: 0.50in
.. |cpu6| image:: images/pdf/chess5/coin_back_s6_t0.pdf
       :height: 0.50in
.. |cpu| replace:: |cpu1| |cpu2| |cpu3| |cpu4| |cpu5| |cpu6|
.. |spp1| image:: images/pdf/orthodox1/coin_back_s1_t270.pdf
       :height: 0.50in
.. |spp2| image:: images/pdf/orthodox1/coin_back_s2_t270.pdf
       :height: 0.50in
.. |spp3| image:: images/pdf/orthodox1/coin_back_s3_t270.pdf
       :height: 0.50in
.. |spp4| image:: images/pdf/orthodox1/coin_back_s4_t270.pdf
       :height: 0.50in
.. |spp| replace:: |spp1| |spp2| |spp3| |spp4|
.. |cpp1| image:: images/pdf/chess6/coin_back_s1_t0.pdf
       :height: 0.50in
.. |cpp2| image:: images/pdf/chess6/coin_back_s2_t0.pdf
       :height: 0.50in
.. |cpp3| image:: images/pdf/chess6/coin_back_s3_t0.pdf
       :height: 0.50in
.. |cpp4| image:: images/pdf/chess6/coin_back_s4_t0.pdf
       :height: 0.50in
.. |cpp5| image:: images/pdf/chess6/coin_back_s5_t0.pdf
       :height: 0.50in
.. |cpp6| image:: images/pdf/chess6/coin_back_s6_t0.pdf
       :height: 0.50in
.. |cpp| replace:: |cpp1| |cpp2| |cpp3| |cpp4| |cpp5| |cpp6|

As a mnenomic aid we use piecepack pawns as "Kings" and the piecepack ranks were straightforwardly mapped to FIDE chess ranks (i.e. n,a,2,3,4,5 mapped to ♟,♞,♝,♜,♛,♚) and then as much as feasible those FIDE chess pieces were mapped to shogi chess pieces that move "similarly".  

King 
~~~~

The King is the biggest and most important piece. Since it can't be captured and used by the other player it is the only piece whose directionality isn't needed to determine ownership.  Moves like a FIDE King and a player loses if their King is checkmated by the opposing player.

Rook
~~~~

The fourth piecepack rank is a '3', the fourth FIDE chess rank is a Rook (♜).  The Shogi Rook moves just like a FIDE chess Rook.


Promoted Rook
~~~~~~~~~~~~~

A Promoted Rook can either move like a FIDE chess Rook OR a FIDE chess King.  Also known as a "Dragon".  

Bishop
~~~~~~

The third piecepack rank is a '2', the third FIDE chess rank is a Bishop (♝).  The Shogi Bishop moves just like a FIDE chess Bishop.


Promoted Bishop
~~~~~~~~~~~~~~~

A Promoted Bishop can either move like a FIDE chess Bishop OR a FIDE chess King.  Also known as a "Horse".  

Gold
~~~~

Golds are the only (non-King) piece that does not have a promotion option.  Golds are the only (non-King) piece represented by a piecepack component other than a coin.  The sixth piecepack rank is a '5' and the sixth FIDE chess rank is a King (♚).  Golds have a subset of a King's moves (they cannot move diagonally backwards).  Direction of the die face is the direction above the flat top of the '5'.  Also known as a "Gold General".  

Silver
~~~~~~

The sixth piecepack rank is a '5' and the sixth FIDE chess rank is a King (♚).  Silvers have a subset of a King's moves (they cannot move orthogonally backwards or sideways).  Also known as a "Silver general"

Promoted Silver
~~~~~~~~~~~~~~~

The Promoted Silver moves like a Gold.

Knight
~~~~~~

The second piecepack rank is an 'a' and the second FIDE chess rank is a Knight (♞).  Shogi Knights have a subset of a FIDE Knight's moves: they can only make the forward 'narrow' jumps.  Imagine a knight charging "forward".

Promoted Knight
~~~~~~~~~~~~~~~

A Promoted Knight moves like a Gold.

Lance
~~~~~

The fifth piecepack rank is a '4' and the fifth FIDE chess rank is a Queen (♛).  The Lance has a subset of a FIDE Queen's moves: they can only move orthogonally forward.  Imagine the top of a '4' or ♛ as a pitchfork held by a peasant charging "forward".

Promoted Lance
~~~~~~~~~~~~~~

A Promoted Lance moves like a Gold.

Pawn
~~~~

Unlike a FIDE chess pawn the Shogi pawn both moves AND captures by moving orthogonally one step forward.

Promoted Pawn
~~~~~~~~~~~~~

A Promoted Pawn moves like a Gold.  Also known as a "Tokin".

Gameplay Instructions
---------------------

Wikipedia has a free set of `Shogi instructions <https://en.wikipedia.org/wiki/Shogi>`_.

License
-------

©2018 Trevor L Davis. Some Rights Reserved.  This work is licensed under a `CC BY-SA 4.0 license <https://creativecommons.org/licenses/by-sa/4.0/>`_.
