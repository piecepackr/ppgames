Dual Piecepacks Poker
=====================

:Author: Trevor L Davis
:Players: 2-20 
:Length: Variable
:Required equipment: One piecepack plus one expansion
:Version: 1.1
:Date: |date|
:License: CC-BY-SA 4.0

.. |date| date::

*Dual Piecepacks Poker* is poker with some `"dual" piecepacks <http://www.ludism.org/ppwiki/DualPiecepacks>`_ twists.  There are three different types of flushes, nulls and aces are both high/low, and a no-pairs hand is better than a one-pair hand.

.. contents::

Equipment
---------

* You'll need one piecepack plus one expansion (ideally `"dual" piecepacks <http://www.ludism.org/ppwiki/DualPiecepacks>`_).  If you don't have dual piecepacks treat each piecepack deck as a separate "group" and "link" one suit from each group to exactly one suit in the other group.  For example if one has a piecepack-suited piecepack and a 2-colored french-suited piecepack (aka the "Playing Cards Expansion") treat the piecepack suits as part of a "piecepack-suit" group, the french suits as part of a "french-suit" group, and use the following four "links": Suns-Hearts, Moons-Spades, Crowns-Clubs, Arms-Diamonds [#colormnenomic]_.  Alternatively the FAQ_ has hints on how to play this game with a `Rainbow Deck`_, a set of `Piecepack Dice Cards`_ (v2.0), or (probably too unpleasantly abstract) a deck of standard playing cards.
* It will be easier to shuffle your tiles if they are made from (square-)playing-card stock.  If you have two dual piecepacks you can have one player shuffle one while another player is dealing the other.  
* Like Backgammon, Poker is much more intellectually challenging and fun if you gamble (even if only for very low stakes) and this is traditionally done with special "poker chip" tokens; you can easily use your Piecepack coins, dice, and pawns as poker chips but you may enjoy using other tokens such as the traditional set of poker chips.

.. [#colormnenomic] Mnenomic hint: this scheme matchs the suit colors from the standard `four-color deck <https://en.wikipedia.org/wiki/Four-color_deck>`_ of playing cards (Red Hearts, Black Spades, Green Clubs, and Blue Diamonds) with the suit colors of a standard piecepack (Red Suns, Black Moons, Green Crowns, and Blue Arms).

.. [#dualdecklink] Matched print-and-play piecepack decks with "dark" and "light" french suits are available in the `"dual" <https://github.com/trevorld/piecepack#dual>`_ and  the `"reversi" <https://github.com/trevorld/piecepack#reversi>`_ demos of the `Piecepack Graphics R Package <https://github.com/trevorld/piecepack>`_.  There are also commercial and print-and-play "`Rainbow Deck`_" playing cards that also have "dark" and "light" french-suits that could be used to easily play this game: use the 2,3,4,5,J,K ranks and treat the J as an "ace" and the K as a "null".

.. [#hi_low_note] Since we have no straights the fact that nulls and aces are both high and low only matter in `High-low split <https://en.wikipedia.org/wiki/High-low_split>`_ poker variants like Hi-Lo Omaha Hold'em otherwise you'd always play them high (or low if playing a `Lowball <https://en.wikipedia.org/wiki/Lowball_(poker)>`_ variant) .

Terminology
-----------

For typographical convenience we'll assume you have a "black" french-suited piecepack (♠,♥,♦,♣) and a "white" french-suited piecepack (♤,♡,♢,♧) [#dualdecklink]_.  As an example (♡N) represents "the null of white-hearts" tile and it has a null **rank**, a white-heart **suit**, a heart **link**, and a white **group**.  The order of ranks is N,A,2,3,4,5,A,N.  As one can see nulls and aces are special ranks that can both be high or low.  A hand is formed by five tiles and the worth of the hand is based on how rare it is.  If suits on all five tiles belong to the same **group** then that hand is a **group-flush**, if all five tiles belong to the same **link** then that hand is a **link-flush**, and if all five tiles belong to the same **suit** that hand is a **suit-flush**.  Like normal poker it is possible to have no-pairs, one-pair, two-pairs, three-of-a-kind, four-of-a-kind, five-of-a-kind, and full-house but unlike normal poker one can have flush variants of these combos and no-pairs hands are rarer and hence more valuable than one-pair hands.  "Straights" are not considered to be a separate hand but instead are treated as a "no-pairs" hand but note that like normal poker a (no-flush) "straight" is still better than a (no-flush) three-of-a-kind, a (no-flush) two-pairs, and a (no-flush) one-pair hand and a "royal flush" is still the best hand possible.  From highest to lowest the possible dual piecepacks poker hands are:

.. list-table:: Ranking of hands
   :header-rows: 1
   :stub-columns: 1
   :widths: 30 30 20 20

   * - Hand
     - Example of hand
     - # of hands
     - % of hands
   * - Suit-flush (No-pairs)
     - (♠2,♠3,♠4,♠5,♠N) 
     - 48
     - \<0.1%                
   * - Group-flush Four-of-a-kind
     - (♤3,♡3,♢3,♧3,♢A)
     - 240
     - \<0.1%                
   * - (No-flush) Five-of-a-kind    
     - (♤3,♣3,♢3,♧3,♦3)
     - 336
     - \<0.1%                
   * - Link-flush Two-pair
     - (♧A,♣3,♧3,♣4,♧4)
     - 480
     - \<0.1%                
   * - Link-flush No-pairs
     - (♧A,♣N,♧3,♣4,♧5)   
     - 768 
     - <0.1%                
   * - Group-flush Full-house
     - (♤3,♡3,♢3,♧5,♢5)
     - 1,440   
     - <0.1%                
   * - Link-flush One-pair          
     - (♧A,♣3,♧3,♣N,♧5)     
     - 1,920           
     - 0.1%                 
   * - Group-flush Three-of-a-kind  
     - (♤3,♡3,♢3,♧4,♢5)    
     - 7,680          
     - 0.4%                 
   * - Group-flush No-pairs 
     - (♤N,♡A,♢3,♧4,♢5) 
     - 12,240    
     - 0.7%                 
   * - (No-flush) Four-of-a-kind    
     - (♤3,♣3,♢3,♧3,♦N)    
     - 16,560         
     - 1.0% 
   * - Group-flush Two-pairs         
     - (♤3,♡3,♢A,♧5,♢5)    
     - 17,280         
     - 1.0%                 
   * - (No-flush) Full-house        
     - (♤3,♣3,♢3,♧N,♦N)    
     - 45,600         
     - 2.7%                 
   * - Group-flush One-pair         
     - (♤3,♡3,♢A,♧4,♢5)    
     - 63,360         
     - 3.7%                 
   * - (No-flush) No-pairs          
     - (♤N,♣A,♢3,♧4,♦5)    
     - 183,552        
     - 10.7%                
   * - (No-flush) Three-of-a-kind   
     - (♤3,♣3,♢3,♧A,♦N)    
     - 207,360        
     - 12.1%                
   * - (No-flush) Two-pairs          
     - (♤3,♣3,♢5,♧5,♦N)    
     - 358,560        
     - 20.9%                
   * - (No-flush) One-pair          
     - (♤4,♣4,♢A,♧5,♦N)    
     - 794,880        
     - 46.4%                

How to play
-----------

Play according to the rules of any normal `poker variant <https://en.wikipedia.org/wiki/List_of_poker_variants>`_ but instead of normal playing cards use the dual piecepacks tiles as "cards" and use the table above to determine which hand is best.  A recommended poker variant to try [#hi_low_note]_ would be No Limit `Hi-Lo Omaha Hold'em <https://en.wikipedia.org/wiki/Omaha_hold_%27em#Omaha_hi-low_split-8_or_better>`_ with no qualification to play for low.  No Limit `Texas Hold'em <https://en.wikipedia.org/wiki/Texas_hold_%27em>`_ would also be a good simpler poker variant to start with and would allow for up to 20 players to play (although 2-10 players would be better) but in this variant one will always treat the nulls and aces as high.  

FAQ
---

Why no straight?
    Turns out that if you treat a "straight" as a separate hand then a straight and no-pairs were equally likely and the most sensible tie breaker in case of tie is which hand had the highest cards i.e. treat the straight as if the straight itself didn't matter.  Hence I decided to just let straights not matter at all and simply dropped the straight as a separate hand type and to treat them as a no-pairs hand.  One can note however that the best hand possible ends up still being the best "Straight" Suit-flush (aka a "Royal Flush") and that like in normal poker a (no-flush) "straight" is still better than a (no-flush) three-of-a-kind, a (no-flush) two-pairs, and a (no-flush) one-pair hand.   Also I'm not convinced this game needs any more types of hands 😛.
    
Why are both the null and ace both high and low?
    Why not?  Using the two special piecepack ranks makes it more "piecepacky" and even without straights having ranks that are both high and low has an impact when switching to Lowball and High/Low poker variants.

Can this be played with a `Rainbow Deck`_?
    Yes.  For example one could take the 8 french-suited suits and use the 2,3,4,5,J,K ranks and treat the J as an "ace" and the K as a "null". 

Can this be played with a deck of `Piecepack Dice Cards`_ (v2.0)
    Yes.  For example one could take all the "red" and "black" cards.  There will be "red" and "black" **groups**, four **links** represented by suit symbol, and eight **suits** for each combination of color and suit symbol.

What about a normal deck of playing cards?
    Technically, but unless you mark up some cards it will probably be a little too unpleasantly abstract to track 8 distinct suits and only 6 distinct ranks.  One such (theoretical) scheme would be to count the playing card ranks 2,3,4,5,A,Q as the "white" **group** 2,3,4,5,a,n piecepack ranks and the playing card ranks 6,7,8,9,J,K as the "black" **group** 2,3,4,5,a,n piecepack ranks.

License
-------

©2018 Trevor L Davis. Some Rights Reserved.  This work is licensed under a `CC BY-SA 4.0 <https://creativecommons.org/licenses/by-sa/4.0/>`_ license.

Combinatorical appendix
-----------------------

(n C k) is shorthand notation for "n choose k"; SF, LF, GF, and NF are abbreviations for suit-flush, link-flush, group-flush, and no-flush respectively; and 0P, 1P, 2P, 3K, 4K, 5K, and FH are abbreviations for no-pairs, one-pair, two-pairs, three-of-a-kind, four-of-a-kind, five-of-a-kind, and full-house respectively; and #(*) is shorthand notation for the number of * hands.

.. list-table:: Hand combinatorics
   :header-rows: 1
   :stub-columns: 1
   :widths: 19 13 68

   * - Hand                         
     - # hands  
     - Combinatoric notes                                                                                           
   * - SF 0P
     - 48                        
     - (8 C 1)(6 C 5)                                                                                               
   * - GF 4K
     - 240                       
     - (2 C 1)(6 C 1)(20 C 1)                                                                                       
   * - NF 5K
     - 336                       
     - (6 C 1)(8 C 5)                                                                                               
   * - LF 2P
     - 480                       
     - (4 C 1)(6 C 2)(8 C 1)                                                                                        
   * - LF 0P
     - 768                       
     - (4 C 1)(12 C 5) - #(LF 2P) - #(LF 1P)                                            
   * - GF FH
     - 1,440                     
     - (2 C 1)(6 C 1)(4 C 3)(5 C 1)(4 C 2)                                                                          
   * - LF 1P
     - 1,920                     
     - (4 C 1)(6 C 4)(4 C 1)(2 C 2)(2 C 1)^3                                                                        
   * - GF 3K
     - 7,680                     
     - (2 C 1)(6 C 1)(4 C 3)(20 C 2) - #(GF FH)                                                    
   * - GF 0P
     - 12,240                    
     - (2 C 1)(6 C 5)(4 C 1)^5 - #(SF 0P)                                                                      
   * - NF 4K
     - 16,560                    
     - (6 C 1)(8 C 4)(40 C 1) - #(GF 4K)                                                       
   * - GF 2P
     - 17,280                    
     - (2 C 1)(6 C 2)(4 C 2)^2 (16 C 1)                                                                             
   * - NF FH
     - 45,600                    
     - (6 C 1)(8 C 3)(5 C 1)(8 C 2) - #(GF FH)                                                     
   * - GF 0P
     - 63,360                   
     - (2 C 1)(6 C 1)(4 C 2)(20 C 3) - #(GF 2P) - #(GF FH)                          
   * - NF 0P
     - 183,552                   
     - (6 C 5)(8 C 1)^5 - #(GF 0P) - #(LF 0P) - #(SF 0P)                            
   * - NF 3K
     - 207,360                   
     - (6 C 1)(8 C 3)(40 C 2) - #(NF FH) - #(GF FH) - #(GF 3K) 
   * - NF 2P
     - 358,560                   
     - (6 C 2)(8 C 2)^2(32 C 1) - #(GF 2P) - (LF 2P)                                   
   * - NF 1P
     - 794,880                   
     - (6 C 4)(4 C 1)(8 C 2)(8 C 1)^3 - #(GF 1P) - #(LF 1P)                            
   * - **Total**
     - 1,712,304                 
     - (48 C 5)                                                                                                     

.. _Rainbow Deck: https://boardgamegeek.com/boardgame/59655/rainbow-deck

.. _Piecepack Dice Cards: http://www.ludism.org/ppwiki/PiecepackDiceCardsTwo
