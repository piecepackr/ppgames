# https://en.wikipedia.org/wiki/Transformation_of_text#Reversed_text

r45 <- list()
r90 <- list()
r135 <- list()
r180 <- list()
r225 <- list()
r270 <- list()
r315 <- list()

### Spaces, Letters, Numbers
## Null
r45[[" "]] <- " "
r90[[" "]] <- " "
r135[[" "]] <- " "
r180[[" "]] <- " "
r225[[" "]] <- " "
r270[[" "]] <- " "
r315[[" "]] <- " "
r45[["\u3000"]] <- "\u3000" # Ideographic (Square) Space
r90[["\u3000"]] <- "\u3000"
r135[["\u3000"]] <- "\u3000"
r180[["\u3000"]] <- "\u3000"
r225[["\u3000"]] <- "\u3000"
r270[["\u3000"]] <- "\u3000"
r315[["\u3000"]] <- "\u3000"
# nolint start
# r180[["\u2202"]] <- "e"
# r180[["\u2202"]] <- "\u03ed"
# nolint end
r180[["\u2202"]] <- "\u03f1"
r180[["n"]] <- "u"
r270[["n"]] <- "\u1d59"
r180[["N"]] <- "N"
r180[["0"]] <- "0"
 r90[["\U000FCB50"]] <- "\U000FCB74" # Game Bit PUA
r180[["\U000FCB50"]] <- "\U000FCB68"
r270[["\U000FCB50"]] <- "\U000FCB5C"
 r90[["\U000FCC50"]] <- "\U000FCC74" # Game Bit PUA
r180[["\U000FCC50"]] <- "\U000FCC68"
r270[["\U000FCC50"]] <- "\U000FCC5C"

## Ace
r90[["A"]] <- "\u2222"
r180[["A"]] <- "\u2200"
r180[["a"]] <- "\u0250"
r90[["1"]] <- "\u21bd"
# r90[["1"]] <- "\u295e" # nolint
r180[["1"]] <- "\u21c2"
# r180[["1"]] <- "\u295d" # nolint
r270[["1"]] <- "\u21c0"
# r270[["1"]] <- "\u295b" # nolint
r90[["\u21bf"]] <- "\u21bd"
r180[["\u21bf"]] <- "\u21c2"
r270[["\u21bf"]] <- "\u21c0"
r90[["\u2960"]] <- "\u295e"
r180[["\u2960"]] <- "\u295d"
r270[["\u2960"]] <- "\u295b"
r180[["\u0ed1"]] <- "\u0ed2" ## Spirals (Lao Digit One)
r180[["\u0ed2"]] <- "\u0ed1" ## Spirals (Lao Digit One)
 r90[["\U000FCB51"]] <- "\U000FCB75" # Game Bit PUA
r180[["\U000FCB51"]] <- "\U000FCB69"
r270[["\U000FCB51"]] <- "\U000FCB5D"
 r90[["\U000FCC51"]] <- "\U000FCC75" # Game Bit PUA
r180[["\U000FCC51"]] <- "\U000FCC69"
r270[["\U000FCC51"]] <- "\U000FCC5D"

r180[["2"]] <- "\u218a"
 r90[["\U000FCB52"]] <- "\U000FCB76" # Game Bit PUA
r180[["\U000FCB52"]] <- "\U000FCB6A"
r270[["\U000FCB52"]] <- "\U000FCB5E"
 r90[["\U000FCC52"]] <- "\U000FCC76" # Game Bit PUA
r180[["\U000FCC52"]] <- "\U000FCC6A"
r270[["\U000FCC52"]] <- "\U000FCC5E"

r90[["3"]] <- "m"
r180[["3"]] <- "\u218b"
r270[["3"]] <- "\u03c9"
 r90[["\U000FCB53"]] <- "\U000FCB77" # Game Bit PUA
r180[["\U000FCB53"]] <- "\U000FCB6B"
r270[["\U000FCB53"]] <- "\U000FCB5F"
 r90[["\U000FCC53"]] <- "\U000FCC77" # Game Bit PUA
r180[["\U000FCC53"]] <- "\U000FCC6B"
r270[["\U000FCC53"]] <- "\U000FCC5F"

r180[["4"]] <- "\u152d"
# nolint start
# r180[["4"]] <- "\u3123"
# r180[["4"]] <- "\u07c8"
# r180[["5"]] <- "\u03da"
# nolint end
 r90[["\U000FCB54"]] <- "\U000FCB78" # Game Bit PUA
r180[["\U000FCB54"]] <- "\U000FCB6C"
r270[["\U000FCB54"]] <- "\U000FCB60"
 r90[["\U000FCC54"]] <- "\U000FCC78" # Game Bit PUA
r180[["\U000FCC54"]] <- "\U000FCC6C"
r270[["\U000FCC54"]] <- "\U000FCC60"

r180[["5"]] <- "\u2185\u0332"
 r90[["\U000FCB55"]] <- "\U000FCB79" # Game Bit PUA
r180[["\U000FCB55"]] <- "\U000FCB6D"
r270[["\U000FCB55"]] <- "\U000FCB61"
 r90[["\U000FCC55"]] <- "\U000FCC79" # Game Bit PUA
r180[["\U000FCC55"]] <- "\U000FCC6D"
r270[["\U000FCC55"]] <- "\U000FCC61"

r180[["6"]] <- "9"
 r90[["\U000FCB56"]] <- "\U000FCB7A" # Game Bit PUA
r180[["\U000FCB56"]] <- "\U000FCB6E"
r270[["\U000FCB56"]] <- "\U000FCB62"
 r90[["\U000FCC56"]] <- "\U000FCC7A" # Game Bit PUA
r180[["\U000FCC56"]] <- "\U000FCC6E"
r270[["\U000FCC56"]] <- "\U000FCC62"

 r90[["\U000FCB57"]] <- "\U000FCB7B" # Game Bit PUA
r180[["\U000FCB57"]] <- "\U000FCB6F"
r270[["\U000FCB57"]] <- "\U000FCB63"
 r90[["\U000FCC57"]] <- "\U000FCC7B" # Game Bit PUA
r180[["\U000FCC57"]] <- "\U000FCC6F"
r270[["\U000FCC57"]] <- "\U000FCC63"

 r90[["\U000FCB58"]] <- "\U000FCB7C" # Game Bit PUA
r180[["\U000FCB58"]] <- "\U000FCB70"
r270[["\U000FCB58"]] <- "\U000FCB64"
 r90[["\U000FCC58"]] <- "\U000FCC7C" # Game Bit PUA
r180[["\U000FCC58"]] <- "\U000FCC70"
r270[["\U000FCC58"]] <- "\U000FCC64"

r180[["9"]] <- "6"
 r90[["\U000FCB59"]] <- "\U000FCB7D" # Game Bit PUA
r180[["\U000FCB59"]] <- "\U000FCB71"
r270[["\U000FCB59"]] <- "\U000FCB65"
 r90[["\U000FCC59"]] <- "\U000FCC7D" # Game Bit PUA
r180[["\U000FCC59"]] <- "\U000FCC71"
r270[["\U000FCC59"]] <- "\U000FCC65"

 r90[["\U000FCB5A"]] <- "\U000FCB7E" # Game Bit PUA
r180[["\U000FCB5A"]] <- "\U000FCB72"
r270[["\U000FCB5A"]] <- "\U000FCB66"
 r90[["\U000FCC5A"]] <- "\U000FCC7E" # Game Bit PUA
r180[["\U000FCC5A"]] <- "\U000FCC72"
r270[["\U000FCC5A"]] <- "\U000FCC66"

 r90[["\U000FCB5B"]] <- "\U000FCB7F" # Game Bit PUA
r180[["\U000FCB5B"]] <- "\U000FCB73"
r270[["\U000FCB5B"]] <- "\U000FCB67"
 r90[["\U000FCC5B"]] <- "\U000FCC7F" # Game Bit PUA
r180[["\U000FCC5B"]] <- "\U000FCC73"
r270[["\U000FCC5B"]] <- "\U000FCC67"

### Piecepack
## Suns
r45[["\u2600"]] <- "\u2600"
r45[["\u2609"]] <- "\u2609"
r45[["\u263c"]] <- "\u263c"
r90[["\u2600"]] <- "\u2600"
r90[["\u2609"]] <- "\u2609"
r90[["\u263c"]] <- "\u263c"
r135[["\u2600"]] <- "\u2600"
r135[["\u2609"]] <- "\u2609"
r135[["\u263c"]] <- "\u263c"
r180[["\u2600"]] <- "\u2600"
r180[["\u2609"]] <- "\u2609"
r180[["\u263c"]] <- "\u263c"
r225[["\u2600"]] <- "\u2600"
r225[["\u2609"]] <- "\u2609"
r225[["\u263c"]] <- "\u263c"
r270[["\u2600"]] <- "\u2600"
r270[["\u2609"]] <- "\u2609"
r270[["\u263c"]] <- "\u263c"
r315[["\u2600"]] <- "\u2600"
r315[["\u2609"]] <- "\u2609"
r315[["\u263c"]] <- "\u263c"
 r90[["\U000FCB00"]] <- "\U000FCB0C" # Game Bit PUA (Black)
r180[["\U000FCB00"]] <- "\U000FCB08"
r270[["\U000FCB00"]] <- "\U000FCB04"
 r90[["\U000FCC00"]] <- "\U000FCC0C" # Game Bit PUA (Black)
r180[["\U000FCC00"]] <- "\U000FCC08"
r270[["\U000FCC00"]] <- "\U000FCC04"
 r90[["\U000FCE00"]] <- "\U000FCE0C" # Game Bit PUA (Black)
r180[["\U000FCE00"]] <- "\U000FCE08"
r270[["\U000FCE00"]] <- "\U000FCE04"

 r90[["\U000FCB10"]] <- "\U000FCB1C" # Game Bit PUA (White)
r180[["\U000FCB10"]] <- "\U000FCB18"
r270[["\U000FCB10"]] <- "\U000FCB14"
 r90[["\U000FCC10"]] <- "\U000FCC1C" # Game Bit PUA (White)
r180[["\U000FCC10"]] <- "\U000FCC18"
r270[["\U000FCC10"]] <- "\U000FCC14"
 r90[["\U000FCE10"]] <- "\U000FCE1C" # Game Bit PUA (White)
r180[["\U000FCE10"]] <- "\U000FCE18"
r270[["\U000FCE10"]] <- "\U000FCE14"

## Moons
r90[["\u25d0"]] <- "\u25d3"
r180[["\u25d0"]] <- "\u25d1"
r270[["\u25d0"]] <- "\u25d2"
r90[["\u25d8"]] <- "\u25d8"
r180[["\u25d8"]] <- "\u25d8"
r270[["\u25d8"]] <- "\u25d8"
r180[["\u263e"]] <- "\u263d"
r180[["\u263d"]] <- "\u263e"
 r90[["\U000FCB01"]] <- "\U000FCB0D" # Game Bit PUA (Black)
r180[["\U000FCB01"]] <- "\U000FCB09"
r270[["\U000FCB01"]] <- "\U000FCB05"
 r90[["\U000FCC01"]] <- "\U000FCC0D" # Game Bit PUA (Black)
r180[["\U000FCC01"]] <- "\U000FCC09"
r270[["\U000FCC01"]] <- "\U000FCC05"
 r90[["\U000FCE01"]] <- "\U000FCE0D" # Game Bit PUA (Black)
r180[["\U000FCE01"]] <- "\U000FCE09"
r270[["\U000FCE01"]] <- "\U000FCE05"

 r90[["\U000FCB11"]] <- "\U000FCB1D" # Game Bit PUA (White)
r180[["\U000FCB11"]] <- "\U000FCB19"
r270[["\U000FCB11"]] <- "\U000FCB15"
 r90[["\U000FCC11"]] <- "\U000FCC1D" # Game Bit PUA (White)
r180[["\U000FCC11"]] <- "\U000FCC19"
r270[["\U000FCC11"]] <- "\U000FCC15"
 r90[["\U000FCE11"]] <- "\U000FCE1D" # Game Bit PUA (White)
r180[["\U000FCE11"]] <- "\U000FCE19"
r270[["\U000FCE11"]] <- "\U000FCE15"

## Crowns
r180[["\u2641"]] <- "\u2640" # Earth
r180[["\u2640"]] <- "\u2641" # Venus
r180[["\u0238"]] <- "\u0239" # Small Letter db Digraph
 r90[["\U000FCB02"]] <- "\U000FCB0E" # Game Bit PUA (Black)
r180[["\U000FCB02"]] <- "\U000FCB0A"
r270[["\U000FCB02"]] <- "\U000FCB06"
 r90[["\U000FCC02"]] <- "\U000FCC0E" # Game Bit PUA (Black)
r180[["\U000FCC02"]] <- "\U000FCC0A"
r270[["\U000FCC02"]] <- "\U000FCC06"
 r90[["\U000FCE02"]] <- "\U000FCE0E" # Game Bit PUA (Black)
r180[["\U000FCE02"]] <- "\U000FCE0A"
r270[["\U000FCE02"]] <- "\U000FCE06"

 r90[["\U000FCB12"]] <- "\U000FCB1E" # Game Bit PUA (White)
r180[["\U000FCB12"]] <- "\U000FCB1A"
r270[["\U000FCB12"]] <- "\U000FCB16"
 r90[["\U000FCC12"]] <- "\U000FCC1E" # Game Bit PUA (White)
r180[["\U000FCC12"]] <- "\U000FCC1A"
r270[["\U000FCC12"]] <- "\U000FCC16"
 r90[["\U000FCE12"]] <- "\U000FCE1E" # Game Bit PUA (White)
r180[["\U000FCE12"]] <- "\U000FCE1A"
r270[["\U000FCE12"]] <- "\U000FCE16"

## Arms
r180[["\u2020"]] <- "\u2e38" ## Dagger
r180[["\u2e38"]] <- "\u2020" ## Turned Dagger
r180[["\u2021"]] <- "\u2021" ## Double Dagger
 r90[["\U000FCB03"]] <- "\U000FCB0F" # Game Bit PUA (Black)
r180[["\U000FCB03"]] <- "\U000FCB0B"
r270[["\U000FCB03"]] <- "\U000FCB07"
 r90[["\U000FCC03"]] <- "\U000FCC0F" # Game Bit PUA (Black)
r180[["\U000FCC03"]] <- "\U000FCC0B"
r270[["\U000FCC03"]] <- "\U000FCC07"
 r90[["\U000FCE03"]] <- "\U000FCE0F" # Game Bit PUA (Black)
r180[["\U000FCE03"]] <- "\U000FCE0B"
r270[["\U000FCE03"]] <- "\U000FCE07"

 r90[["\U000FCB13"]] <- "\U000FCB1F" # Game Bit PUA (White)
r180[["\U000FCB13"]] <- "\U000FCB1B"
r270[["\U000FCB13"]] <- "\U000FCB17"
 r90[["\U000FCC13"]] <- "\U000FCC1F" # Game Bit PUA (White)
r180[["\U000FCC13"]] <- "\U000FCC1B"
r270[["\U000FCC13"]] <- "\U000FCC17"
 r90[["\U000FCE13"]] <- "\U000FCE1F" # Game Bit PUA (White)
r180[["\U000FCE13"]] <- "\U000FCE1B"
r270[["\U000FCE13"]] <- "\U000FCE17"

## Misc
r180[["\u260a"]] <- "\u260b" # Ascending/Descending nodes
r180[["\u260b"]] <- "\u260a" # Ascending/Descending nodes

### French Suits
## Hearts
# Black Heart
r90[["\u2665"]] <- "\u2765" # Rotated Black Heart
r90[["\u2764"]] <- "\u2765"
r180[["\u2665"]] <- "\u03c9\u0302"
 r90[["\U000FCB20"]] <- "\U000FCB2C" # Game Bit PUA (Black)
r180[["\U000FCB20"]] <- "\U000FCB28"
r270[["\U000FCB20"]] <- "\U000FCB24"
 r90[["\U000FCC20"]] <- "\U000FCC2C" # Game Bit PUA (Black)
r180[["\U000FCC20"]] <- "\U000FCC28"
r270[["\U000FCC20"]] <- "\U000FCC24"
 r90[["\U000FCE20"]] <- "\U000FCE2C" # Game Bit PUA (Black)
r180[["\U000FCE20"]] <- "\U000FCE28"
r270[["\U000FCE20"]] <- "\U000FCE24"
# White Heart
r90[["\u2661"]] <- "\u2765"
r180[["\u2661"]] <- "\u03c9\u0302"
 r90[["\U000FCB30"]] <- "\U000FCB3C" # Game Bit PUA (White)
r180[["\U000FCB30"]] <- "\U000FCB38"
r270[["\U000FCB30"]] <- "\U000FCB34"
 r90[["\U000FCC30"]] <- "\U000FCC3C" # Game Bit PUA (White)
r180[["\U000FCC30"]] <- "\U000FCC38"
r270[["\U000FCC30"]] <- "\U000FCC34"
 r90[["\U000FCE30"]] <- "\U000FCE3C" # Game Bit PUA (White)
r180[["\U000FCE30"]] <- "\U000FCE38"
r270[["\U000FCE30"]] <- "\U000FCE34"
## Spades
# Black
r180[["\u2660"]] <- "\u2764\u030d"
 r90[["\U000FCB21"]] <- "\U000FCB2D" # Game Bit PUA (Black)
r180[["\U000FCB21"]] <- "\U000FCB29"
r270[["\U000FCB21"]] <- "\U000FCB25"
 r90[["\U000FCC21"]] <- "\U000FCC2D" # Game Bit PUA (Black)
r180[["\U000FCC21"]] <- "\U000FCC29"
r270[["\U000FCC21"]] <- "\U000FCC25"
 r90[["\U000FCE21"]] <- "\U000FCE2D" # Game Bit PUA (Black)
r180[["\U000FCE21"]] <- "\U000FCE29"
r270[["\U000FCE21"]] <- "\U000FCE25"
# White
r180[["\u2664"]] <- "\u2661\u030d"
 r90[["\U000FCB31"]] <- "\U000FCB3D" # Game Bit PUA (White)
r180[["\U000FCB31"]] <- "\U000FCB39"
r270[["\U000FCB31"]] <- "\U000FCB35"
 r90[["\U000FCC31"]] <- "\U000FCC3D" # Game Bit PUA (White)
r180[["\U000FCC31"]] <- "\U000FCC39"
r270[["\U000FCC31"]] <- "\U000FCC35"
 r90[["\U000FCE31"]] <- "\U000FCE3D" # Game Bit PUA (White)
r180[["\U000FCE31"]] <- "\U000FCE39"
r270[["\U000FCE31"]] <- "\U000FCE35"
## Clubs
# Black
r180[["\u2663"]] <- "\u2235\u0304"
 r90[["\U000FCB22"]] <- "\U000FCB2E" # Game Bit PUA (White)
r180[["\U000FCB22"]] <- "\U000FCB2A"
r270[["\U000FCB22"]] <- "\U000FCB26"
 r90[["\U000FCC22"]] <- "\U000FCC2E" # Game Bit PUA (White)
r180[["\U000FCC22"]] <- "\U000FCC2A"
r270[["\U000FCC22"]] <- "\U000FCC26"
 r90[["\U000FCE22"]] <- "\U000FCE2E" # Game Bit PUA (White)
r180[["\U000FCE22"]] <- "\U000FCE2A"
r270[["\U000FCE22"]] <- "\U000FCE26"
# White
r180[["\u2667"]] <- "\u2235\u0304"
 r90[["\U000FCB32"]] <- "\U000FCB3E" # Game Bit PUA (White)
r180[["\U000FCB32"]] <- "\U000FCB3A"
r270[["\U000FCB32"]] <- "\U000FCB36"
 r90[["\U000FCC32"]] <- "\U000FCC3E" # Game Bit PUA (White)
r180[["\U000FCC32"]] <- "\U000FCC3A"
r270[["\U000FCC32"]] <- "\U000FCC36"
 r90[["\U000FCE32"]] <- "\U000FCE3E" # Game Bit PUA (White)
r180[["\U000FCE32"]] <- "\U000FCE3A"
r270[["\U000FCE32"]] <- "\U000FCE36"
## Diamonds
# Black
r90[["\u2666"]] <- "\u25c6"
r180[["\u2666"]] <- "\u2666"
r270[["\u2666"]] <- "\u25c6"
 r90[["\U000FCB23"]] <- "\U000FCB2F" # Game Bit PUA (White)
r180[["\U000FCB23"]] <- "\U000FCB2B"
r270[["\U000FCB23"]] <- "\U000FCB27"
 r90[["\U000FCC23"]] <- "\U000FCC2F" # Game Bit PUA (White)
r180[["\U000FCC23"]] <- "\U000FCC2B"
r270[["\U000FCC23"]] <- "\U000FCC27"
 r90[["\U000FCE23"]] <- "\U000FCE2F" # Game Bit PUA (White)
r180[["\U000FCE23"]] <- "\U000FCE2B"
r270[["\U000FCE23"]] <- "\U000FCE27"
# White
r90[["\u2662"]] <- "\u25c7"
r180[["\u2662"]] <- "\u2662"
r270[["\u2662"]] <- "\u25c7"
 r90[["\U000FCB33"]] <- "\U000FCB3F" # Game Bit PUA (White)
r180[["\U000FCB33"]] <- "\U000FCB3B"
r270[["\U000FCB33"]] <- "\U000FCB37"
 r90[["\U000FCC33"]] <- "\U000FCC3F" # Game Bit PUA (White)
r180[["\U000FCC33"]] <- "\U000FCC3B"
r270[["\U000FCC33"]] <- "\U000FCC37"
 r90[["\U000FCE33"]] <- "\U000FCE3F" # Game Bit PUA (White)
r180[["\U000FCE33"]] <- "\U000FCE3B"
r270[["\U000FCE33"]] <- "\U000FCE37"

### Chess symbols
# https://www.unicode.org/charts/PDF/U1FA00.pdf
# Note Unicode rotates clockwise whereas we rotate counter-clockwise
# Could add "neutral chess"
r45[["\u2658"]] <- "\U1fa45" # N
r45[["\u265e"]] <- "\U1fa46" # n

r90[["\u2654"]] <- "\U1fa33" # K
r90[["\u2655"]] <- "\U1fa34" # Q
r90[["\u2656"]] <- "\U1fa35" # R
r90[["\u2657"]] <- "\U1fa36" # B
r90[["\u2658"]] <- "\U1fa37" # N
r90[["\u2659"]] <- "\U1fa38" # P
r90[["\u265a"]] <- "\U1fa39" # k
r90[["\u265b"]] <- "\U1fa3a" # q
r90[["\u265c"]] <- "\U1fa3b" # r
r90[["\u265d"]] <- "\U1fa3c" # b
r90[["\u265e"]] <- "\U1fa3d" # n
r90[["\u265f"]] <- "\U1fa3e" # p

r135[["\u2658"]] <- "\U1fa30" # N
r135[["\u265e"]] <- "\U1fa31" # n

r180[["\u2654"]] <- "\U1fa1e" # K
r180[["\u2655"]] <- "\U1fa1f" # Q
r180[["\u2656"]] <- "\U1fa20" # R
r180[["\u2657"]] <- "\U1fa21" # B
r180[["\u2658"]] <- "\U1fa22" # N
r180[["\u2659"]] <- "\U1fa23" # P
r180[["\u265a"]] <- "\U1fa24" # k
r180[["\u265b"]] <- "\U1fa25" # q
r180[["\u265c"]] <- "\U1fa26" # r
r180[["\u265d"]] <- "\U1fa27" # b
r180[["\u265e"]] <- "\U1fa28" # n
r180[["\u265f"]] <- "\U1fa29" # p

r225[["\u2658"]] <- "\U1fa1b" # N
r225[["\u265e"]] <- "\U1fa1c" # n

r270[["\u2654"]] <- "\U1fa09" # K
r270[["\u2655"]] <- "\U1fa0a" # Q
r270[["\u2656"]] <- "\U1fa0b" # R
r270[["\u2657"]] <- "\U1fa0c" # B
r270[["\u2658"]] <- "\U1fa0d" # N
r270[["\u2659"]] <- "\U1fa0e" # P
r270[["\u265a"]] <- "\U1fa0f" # k
r270[["\u265b"]] <- "\U1fa10" # q
r270[["\u265c"]] <- "\U1fa11" # r
r270[["\u265d"]] <- "\U1fa12" # b
r270[["\u265e"]] <- "\U1fa13" # n
r270[["\u265f"]] <- "\U1fa14" # p

r225[["\u2658"]] <- "\U1fa06" # N
r225[["\u265e"]] <- "\U1fa07" # n

### Checkers
# White man
r45[["\u26c0"]] <- "\u26c0"
r90[["\u26c0"]] <- "\u26c0"
r135[["\u26c0"]] <- "\u26c0"
r180[["\u26c0"]] <- "\u26c0"
r225[["\u26c0"]] <- "\u26c0"
r270[["\u26c0"]] <- "\u26c0"
r315[["\u26c0"]] <- "\u26c0"
# White king
r45[["\u26c1"]] <- "\u26c1"
r90[["\u26c1"]] <- "\u26c1"
r135[["\u26c1"]] <- "\u26c1"
r180[["\u26c1"]] <- "\u26c1"
r225[["\u26c1"]] <- "\u26c1"
r270[["\u26c1"]] <- "\u26c1"
r315[["\u26c1"]] <- "\u26c1"
# Black man
r45[["\u26c2"]] <- "\u26c2"
r90[["\u26c2"]] <- "\u26c2"
r135[["\u26c2"]] <- "\u26c2"
r180[["\u26c2"]] <- "\u26c2"
r225[["\u26c2"]] <- "\u26c2"
r270[["\u26c2"]] <- "\u26c2"
r315[["\u26c2"]] <- "\u26c2"
# Black king
r45[["\u26c3"]] <- "\u26c3"
r90[["\u26c3"]] <- "\u26c3"
r135[["\u26c3"]] <- "\u26c3"
r180[["\u26c3"]] <- "\u26c3"
r225[["\u26c3"]] <- "\u26c3"
r270[["\u26c3"]] <- "\u26c3"
r315[["\u26c3"]] <- "\u26c3"

# Game Bit PUA Domino Pips
 r90[["\U000FCA00"]] <- "\U000FCA13" # Zero
 r90[["\U000FCA01"]] <- "\U000FCA14" # One
 r90[["\U000FCA02"]] <- "\U000FCA15" # Two
 r90[["\U000FCA03"]] <- "\U000FCA16" # Three
 r90[["\U000FCA04"]] <- "\U000FCA17" # Four
 r90[["\U000FCA05"]] <- "\U000FCA18" # Five
 r90[["\U000FCA06"]] <- "\U000FCA19" # Six
 r90[["\U000FCA07"]] <- "\U000FCA1A" # Seven
 r90[["\U000FCA08"]] <- "\U000FCA1B" # Eight
 r90[["\U000FCA09"]] <- "\U000FCA1C" # Nine
 r90[["\U000FCA0A"]] <- "\U000FCA1D" # Ten
 r90[["\U000FCA0B"]] <- "\U000FCA1E" # Eleven
 r90[["\U000FCA0C"]] <- "\U000FCA1F" # Twelve
 r90[["\U000FCA0D"]] <- "\U000FCA20" # Thirteen
 r90[["\U000FCA0E"]] <- "\U000FCA21" # Fourteen
 r90[["\U000FCA0F"]] <- "\U000FCA22" # Fifteen
 r90[["\U000FCA10"]] <- "\U000FCA23" # Sixteen
 r90[["\U000FCA11"]] <- "\U000FCA24" # Seventeen
 r90[["\U000FCA12"]] <- "\U000FCA25" # Eighteen
r180[["\U000FCA00"]] <- "\U000FCA26" # Zero
r180[["\U000FCA01"]] <- "\U000FCA27" # One
r180[["\U000FCA02"]] <- "\U000FCA28" # Two
r180[["\U000FCA03"]] <- "\U000FCA29" # Three
r180[["\U000FCA04"]] <- "\U000FCA2A" # Four
r180[["\U000FCA05"]] <- "\U000FCA2B" # Five
r180[["\U000FCA06"]] <- "\U000FCA2C" # Six
r180[["\U000FCA07"]] <- "\U000FCA2D" # Seven
r180[["\U000FCA08"]] <- "\U000FCA2E" # Eight
r180[["\U000FCA09"]] <- "\U000FCA2F" # Nine
r180[["\U000FCA0A"]] <- "\U000FCA30" # Ten
r180[["\U000FCA0B"]] <- "\U000FCA31" # Eleven
r180[["\U000FCA0C"]] <- "\U000FCA32" # Twelve
r180[["\U000FCA0D"]] <- "\U000FCA33" # Thirteen
r180[["\U000FCA0E"]] <- "\U000FCA34" # Fourteen
r180[["\U000FCA0F"]] <- "\U000FCA35" # Fifteen
r180[["\U000FCA10"]] <- "\U000FCA36" # Sixteen
r180[["\U000FCA11"]] <- "\U000FCA37" # Seventeen
r180[["\U000FCA12"]] <- "\U000FCA38" # Eighteen
r270[["\U000FCA00"]] <- "\U000FCA39" # Zero
r270[["\U000FCA01"]] <- "\U000FCA3A" # One
r270[["\U000FCA02"]] <- "\U000FCA3B" # Two
r270[["\U000FCA03"]] <- "\U000FCA3C" # Three
r270[["\U000FCA04"]] <- "\U000FCA3D" # Four
r270[["\U000FCA05"]] <- "\U000FCA3E" # Five
r270[["\U000FCA06"]] <- "\U000FCA3F" # Six
r270[["\U000FCA07"]] <- "\U000FCA40" # Seven
r270[["\U000FCA08"]] <- "\U000FCA41" # Eight
r270[["\U000FCA09"]] <- "\U000FCA42" # Nine
r270[["\U000FCA0A"]] <- "\U000FCA43" # Ten
r270[["\U000FCA0B"]] <- "\U000FCA44" # Eleven
r270[["\U000FCA0C"]] <- "\U000FCA45" # Twelve
r270[["\U000FCA0D"]] <- "\U000FCA46" # Thirteen
r270[["\U000FCA0E"]] <- "\U000FCA47" # Fourteen
r270[["\U000FCA0F"]] <- "\U000FCA48" # Fifteen
r270[["\U000FCA10"]] <- "\U000FCA49" # Sixteen
r270[["\U000FCA11"]] <- "\U000FCA4A" # Seventeen
r270[["\U000FCA12"]] <- "\U000FCA4B" # Eighteen

# Middle Dot
r45[["\u00b7"]] <- "\u00b7"
r90[["\u00b7"]] <- "\u00b7"
r135[["\u00b7"]] <- "\u00b7"
r180[["\u00b7"]] <- "\u00b7"
r225[["\u00b7"]] <- "\u00b7"
r270[["\u00b7"]] <- "\u00b7"
r315[["\u00b7"]] <- "\u00b7"
# Katakana Middle Dot
r45[["\u30fb"]] <- "\u30fb"
r90[["\u30fb"]] <- "\u30fb"
r135[["\u30fb"]] <- "\u30fb"
r180[["\u30fb"]] <- "\u30fb"
r225[["\u30fb"]] <- "\u30fb"
r270[["\u30fb"]] <- "\u30fb"
r315[["\u30fb"]] <- "\u30fb"
# Braille Dots-34
r45[["\u280c"]] <- "\u205a"
r90[["\u280c"]] <- "\u2821"
r180[["\u280c"]] <- "\u280c"
r225[["\u280c"]] <- "\u205a"
r270[["\u280c"]] <- "\u2821"
# Braille Dots-43
r90[["\u2821"]] <- "\u280c"
r135[["\u2821"]] <- "\u205a"
r180[["\u2821"]] <- "\u2821"
r270[["\u2821"]] <- "\u280c"
r315[["\u2821"]] <- "\u205a"
# Right Diagonal Ellipsis
r45[["\u22f0"]] <- "\u22ee"
r90[["\u22f0"]] <- "\u22f1"
r135[["\u22f0"]] <- "\u22ef"
r180[["\u22f0"]] <- "\u22f0"
r225[["\u22f0"]] <- "\u22ee"
r270[["\u22f0"]] <- "\u22f1"
r315[["\u22f0"]] <- "\u22ef"
# Left Diagonal Ellipsis
r45[["\u22f1"]] <- "\u22ef"
r90[["\u22f1"]] <- "\u22f0"
r135[["\u22f1"]] <- "\u22ee"
r180[["\u22f1"]] <- "\u22f1"
r225[["\u22f1"]] <- "\u22ef"
r270[["\u22f1"]] <- "\u22f0"
r315[["\u22f1"]] <- "\u22ee"
# Proportion
r90[["\u2237"]] <- "\u2237"
r180[["\u2237"]] <- "\u2237"
r270[["\u2237"]] <- "\u2237"
# Squared Four Dot Punctuation
r90[["\u2e2c"]] <- "\u2e2c"
r180[["\u2e2c"]] <- "\u2e2c"
r270[["\u2e2c"]] <- "\u2e2c"
# Braille Dots-1346
r90[["\u282d"]] <- "\u282d"
r180[["\u282d"]] <- "\u282d"
r270[["\u282d"]] <- "\u282d"
# Five Dot Punctuation
r45[["\u2059"]] <- "\u2e2d"
r90[["\u2059"]] <- "\u2059"
r135[["\u2059"]] <- "\u2e2d"
r180[["\u2059"]] <- "\u2059"
r225[["\u2059"]] <- "\u2e2d"
r270[["\u2059"]] <- "\u2059"
r315[["\u2059"]] <- "\u2e2d"
# Braille Dots-123456
r90[["\u283f"]] <- "\u2026\u20db"
r180[["\u283f"]] <- "\u283f"
r270[["\u283f"]] <- "\u2026\u20db"
# Ellipsis plus Combining Three Dots Above
r90[["\u2026\u20db"]] <- "\u283f"
r180[["\u2026\u20db"]] <- "\u2026\u20db"
r270[["\u2026\u20db"]] <- "\u283f"

pip7v <- "\u2059\u302e\U0001d16d"
pip7h <- "\u30fb\u20db\u20e8"
r90[[pip7v]] <- pip7h
r180[[pip7v]] <- pip7v
r270[[pip7v]] <- pip7h

pip8 <- "\u2812\u20db\u20e8"
r90[[pip8]] <- pip8
r180[[pip8]] <- pip8
r270[[pip8]] <- pip8

pip9 <- "\u22ef\u20db\u20e8"
r90[[pip9]] <- pip9
r180[[pip9]] <- pip9
r270[[pip9]] <- pip9

### Arrows
# Simple Arrows
r45[["\u2191"]] <- "\u2196"
r90[["\u2191"]] <- "\u2190"
r135[["\u2191"]] <- "\u2199"
r180[["\u2191"]] <- "\u2193"
r225[["\u2191"]] <- "\u2198"
r270[["\u2191"]] <- "\u2192"
r315[["\u2191"]] <- "\u2197"
# Double Arrows
r45[["\u21d1"]] <- "\u21d6"
r90[["\u21d1"]] <- "\u21d0"
r135[["\u21d1"]] <- "\u21d9"
r180[["\u21d1"]] <- "\u21d3"
r225[["\u21d1"]] <- "\u21d8"
r270[["\u21d1"]] <- "\u21d2"
r315[["\u21d1"]] <- "\u21d7"
# Black Arrows
r45[["\u2b06"]] <- "\u2b09"
r90[["\u2b06"]] <- "\u2b05"
r135[["\u2b06"]] <- "\u2b0b"
r180[["\u2b06"]] <- "\u2b07"
r225[["\u2b06"]] <- "\u2b0a"
# r270[["\u2b06"]] <- "\u27a1" # nolint
r270[["\u2b06"]] <- "\u2b95" # nolint
r315[["\u2b06"]] <- "\u2b08"
# White Arrows
r45[["\u21e7"]] <- "\u2b01"
r90[["\u21e7"]] <- "\u21e6"
r135[["\u21e7"]] <- "\u2b03"
r180[["\u21e7"]] <- "\u21e9"
r225[["\u21e7"]] <- "\u2b02"
r270[["\u21e7"]] <- "\u21e8"
r315[["\u21e7"]] <- "\u2b00"
# Very Heavy Barb Arrows
r45[["\U1f881"]] <- "\U1f884"
r90[["\U1f881"]] <- "\U1f880"
r135[["\U1f881"]] <- "\U1f887"
r180[["\U1f881"]] <- "\U1f883"
r225[["\U1f881"]] <- "\U1f886"
r270[["\U1f881"]] <- "\U1f882"
r315[["\U1f881"]] <- "\U1f885"

### Misc
# Multiplication Sign
r45[["\u00d7"]] <- "\u002b"
r90[["\u00d7"]] <- "\u00d7"
r135[["\u00d7"]] <- "\u002b"
r180[["\u00d7"]] <- "\u00d7"
r225[["\u00d7"]] <- "\u002b"
r270[["\u00d7"]] <- "\u00d7"
r315[["\u00d7"]] <- "\u002b"
# Smash Product
r45[["\u2a33"]] <- "\u0023" # \ufe5f \u22d5
r90[["\u2a33"]] <- "\u2a33"
r135[["\u2a33"]] <- "\u0023" # \ufe5f \u22d5
r180[["\u2a33"]] <- "\u2a33"
r225[["\u2a33"]] <- "\u0023" # \ufe5f \u22d5
r270[["\u2a33"]] <- "\u2a33"
r315[["\u2a33"]] <- "\u0023" # \ufe5f \u22d5
# Triple Horizontal Bar with Triple Vertical Stroke
r90[["\u2a69"]] <- "\u2a69"
r180[["\u2a69"]] <- "\u2a69"
r270[["\u2a69"]] <- "\u2a69"

### Geometric Shapes
# https://en.wikipedia.org/wiki/Geometric_Shapes
# Black Diamond
r45[["\u25c6"]] <- "\u25aa" # \u25a0
r90[["\u25c6"]] <- "\u25c6"
r135[["\u25c6"]] <- "\u25aa" # \u25a0
r180[["\u25c6"]] <- "\u25c6"
r225[["\u25c6"]] <- "\u25aa" # \u25a0
r270[["\u25c6"]] <- "\u25c6"
r315[["\u25c6"]] <- "\u25aa" # \u25a0
# White Diamond
r45[["\u25c7"]] <- "\u25a1"
r90[["\u25c7"]] <- "\u25c7"
r135[["\u25c7"]] <- "\u25a1"
r180[["\u25c7"]] <- "\u25c7"
r225[["\u25c7"]] <- "\u25a1"
r270[["\u25c7"]] <- "\u25c7"
r315[["\u25c7"]] <- "\u25a1"
# Black Square
r45[["\u25aa"]] <- "\u25c6"
r90[["\u25aa"]] <- "\u25aa"
r135[["\u25aa"]] <- "\u25c6"
r180[["\u25aa"]] <- "\u25aa"
r225[["\u25aa"]] <- "\u25c6"
r270[["\u25aa"]] <- "\u25aa"
r315[["\u25aa"]] <- "\u25c6"
r45[["\u25a0"]] <- "\u25c6"
r90[["\u25a0"]] <- "\u25a0"
r135[["\u25a0"]] <- "\u25c6"
r180[["\u25a0"]] <- "\u25a0"
r225[["\u25a0"]] <- "\u25c6"
r270[["\u25a0"]] <- "\u25a0"
r315[["\u25a0"]] <- "\u25c6"
# White Square
r45[["\u25a1"]] <- "\u25c7"
r90[["\u25a1"]] <- "\u25a1"
r135[["\u25a1"]] <- "\u25c7"
r180[["\u25a1"]] <- "\u25a1"
r225[["\u25a1"]] <- "\u25c7"
r270[["\u25a1"]] <- "\u25a1"
r315[["\u25a1"]] <- "\u25c7"
# Black Triangle
r45[["\u25b2"]] <- "\u25e4"
r90[["\u25b2"]] <- "\u25c0"
r135[["\u25b2"]] <- "\u25e3"
r180[["\u25b2"]] <- "\u25bc"
r225[["\u25b2"]] <- "\u25e2"
r270[["\u25b2"]] <- "\u25b6"
r315[["\u25b2"]] <- "\u25e5"
# White Triangle
r45[["\u25b3"]] <- "\u25f8"
r90[["\u25b3"]] <- "\u25c1"
r135[["\u25b3"]] <- "\u25fa"
r180[["\u25b3"]] <- "\u25bd"
r225[["\u25b3"]] <- "\u25ff"
r270[["\u25b3"]] <- "\u25b7"
r315[["\u25b3"]] <- "\u25f9"
# Various Symmetric Circles
for (circle in c("\u25cb", "\u25cc", "\u25ce", "\u25cf", "\u20dd")) {
    r45[[circle]] <- circle
    r90[[circle]] <- circle
    r135[[circle]] <- circle
    r180[[circle]] <- circle
    r225[[circle]] <- circle
    r270[[circle]] <- circle
    r315[[circle]] <- circle
}

# Enclosing Coin
 r90[["\U000FCE50"]] <- "\U000FCE5C" # Game Bit PUA
r180[["\U000FCE50"]] <- "\U000FCE58"
r270[["\U000FCE50"]] <- "\U000FCE54"

# Enclosing Pawn
 r45[["\u20df"]] <- "\u20de"
 r90[["\u20df"]] <- "\u20df"
r135[["\u20df"]] <- "\u20de"
r180[["\u20df"]] <- "\u20df"
r225[["\u20df"]] <- "\u20de"
r270[["\u20df"]] <- "\u20df"
r315[["\u20df"]] <- "\u20de"

 r90[["\U000FCDE0"]] <- "\U000FCDEC" # Game Bit PUA
r180[["\U000FCDE0"]] <- "\U000FCDE8"
r270[["\U000FCDE0"]] <- "\U000FCDE4"

# Enclosing Die
 r45[["\u20de"]] <- "\u20df"
 r90[["\u20de"]] <- "\u20de"
r135[["\u20de"]] <- "\u20df"
r180[["\u20de"]] <- "\u20de"
r225[["\u20de"]] <- "\u20df"
r270[["\u20de"]] <- "\u20de"
r315[["\u20de"]] <- "\u20df"

die_subs <- list()
die_subs[["\u00b7\u20de"]] <- "\u2680" # Die Face-1
die_subs[["\u280c\u20de"]] <- "\u2681" # Die Face-2
die_subs[["\u22f0\u20de"]] <- "\u2682" # Die Face-3
die_subs[["\u2237\u20de"]] <- "\u2683" # Die Face-4
die_subs[["\u2059\u20de"]] <- "\u2684" # Die Face-5
die_subs[["\u283f\u20de"]] <- "\u2685" # Die Face-6

top_subs <- list()
top_subs[["\u2191"]] <- "\u00d7"
top_subs[["\u21d1"]] <- "\u2a33"
top_subs[["\u2b06"]] <- "\u25a0"
top_subs[["\U1f881"]] <- "\u25a0"
top_subs[["\u21e7"]] <- "\u25a0"
top_subs[["\u25b2"]] <- "\u25a0"
top_subs[["\u25b3"]] <- "\u25a1"

## Box info
# [top, right, bottom, left] 0-none 1-light 2-heavy 3-matted-heavy
char2bi <- list()
char2bi[["\u2500"]] <- c(0, 1, 0, 1)
char2bi[["\u2501"]] <- c(0, 2, 0, 2)
char2bi[["\u2502"]] <- c(1, 0, 1, 0)
char2bi[["\u2503"]] <- c(2, 0, 2, 0)
char2bi[["\u250c"]] <- c(0, 1, 1, 0)
char2bi[["\u250d"]] <- c(0, 2, 1, 0)
char2bi[["\u250e"]] <- c(0, 1, 2, 0)
char2bi[["\u250f"]] <- c(0, 2, 2, 0)
char2bi[["\u2510"]] <- c(0, 0, 1, 1)
char2bi[["\u2511"]] <- c(0, 0, 1, 2)
char2bi[["\u2512"]] <- c(0, 0, 2, 1)
char2bi[["\u2513"]] <- c(0, 0, 2, 2)
char2bi[["\u2514"]] <- c(1, 1, 0, 0)
char2bi[["\u2515"]] <- c(1, 2, 0, 0)
char2bi[["\u2516"]] <- c(2, 1, 0, 0)
char2bi[["\u2517"]] <- c(2, 2, 0, 0)
char2bi[["\u2518"]] <- c(1, 0, 0, 1)
char2bi[["\u2519"]] <- c(1, 0, 0, 2)
char2bi[["\u251a"]] <- c(2, 0, 0, 1)
char2bi[["\u251b"]] <- c(2, 0, 0, 2)
char2bi[["\u251c"]] <- c(1, 1, 1, 0)
char2bi[["\u251d"]] <- c(1, 2, 1, 0)
char2bi[["\u251e"]] <- c(2, 1, 1, 0)
char2bi[["\u251f"]] <- c(1, 1, 2, 0)
char2bi[["\u2520"]] <- c(2, 1, 2, 0)
char2bi[["\u2521"]] <- c(2, 2, 1, 0)
char2bi[["\u2522"]] <- c(1, 2, 2, 0)
char2bi[["\u2523"]] <- c(2, 2, 2, 0)
char2bi[["\u2524"]] <- c(1, 0, 1, 1)
char2bi[["\u2525"]] <- c(1, 0, 1, 2)
char2bi[["\u2526"]] <- c(2, 0, 1, 1)
char2bi[["\u2527"]] <- c(1, 0, 2, 1)
char2bi[["\u2528"]] <- c(2, 0, 2, 1)
char2bi[["\u2529"]] <- c(2, 0, 1, 2)
char2bi[["\u252a"]] <- c(1, 0, 2, 2)
char2bi[["\u252b"]] <- c(2, 0, 2, 2)
char2bi[["\u252c"]] <- c(0, 1, 1, 1)
char2bi[["\u252d"]] <- c(0, 1, 1, 2)
char2bi[["\u252e"]] <- c(0, 2, 1, 1)
char2bi[["\u252f"]] <- c(0, 2, 1, 2)
char2bi[["\u2530"]] <- c(0, 1, 2, 1)
char2bi[["\u2531"]] <- c(0, 1, 2, 2)
char2bi[["\u2532"]] <- c(0, 2, 2, 1)
char2bi[["\u2533"]] <- c(0, 2, 2, 2)
char2bi[["\u2534"]] <- c(1, 1, 0, 1)
char2bi[["\u2535"]] <- c(1, 1, 0, 2)
char2bi[["\u2536"]] <- c(1, 2, 0, 1)
char2bi[["\u2537"]] <- c(1, 2, 0, 2)
char2bi[["\u2538"]] <- c(2, 1, 0, 1)
char2bi[["\u2539"]] <- c(2, 1, 0, 2)
char2bi[["\u253a"]] <- c(2, 2, 0, 1)
char2bi[["\u253b"]] <- c(2, 2, 0, 2)
char2bi[["\u253c"]] <- c(1, 1, 1, 1)
char2bi[["\u253d"]] <- c(1, 1, 1, 2)
char2bi[["\u253e"]] <- c(1, 2, 1, 1)
char2bi[["\u253f"]] <- c(1, 2, 1, 2)
char2bi[["\u2540"]] <- c(2, 1, 1, 1)
char2bi[["\u2541"]] <- c(1, 1, 2, 1)
char2bi[["\u2542"]] <- c(2, 1, 2, 1)
char2bi[["\u2543"]] <- c(2, 1, 1, 2)
char2bi[["\u2544"]] <- c(2, 2, 1, 1)
char2bi[["\u2545"]] <- c(1, 1, 2, 2)
char2bi[["\u2546"]] <- c(1, 2, 2, 1)
char2bi[["\u2547"]] <- c(2, 2, 1, 2)
char2bi[["\u2548"]] <- c(1, 2, 2, 2)
char2bi[["\u2549"]] <- c(2, 1, 2, 2)
char2bi[["\u254a"]] <- c(2, 2, 2, 1)
char2bi[["\u254b"]] <- c(2, 2, 2, 2)
char2bi[["\U000fdd8c"]] <- c(1, 0, 1, 3)
char2bi[["\U000fdd8d"]] <- c(3, 1, 0, 1)
char2bi[["\U000fdd8e"]] <- c(1, 3, 1, 0)
char2bi[["\U000fdd8f"]] <- c(0, 1, 3, 1)
char2bi[["\U000fdd90"]] <- c(1, 1, 1, 3)
char2bi[["\U000fdd91"]] <- c(3, 1, 1, 1)
char2bi[["\U000fdd92"]] <- c(1, 3, 1, 1)
char2bi[["\U000fdd93"]] <- c(1, 1, 3, 1)
char2bi[["\U000fdd94"]] <- c(1, 2, 1, 3)
char2bi[["\U000fdd95"]] <- c(3, 1, 2, 1)
char2bi[["\U000fdd96"]] <- c(1, 3, 1, 2)
char2bi[["\U000fdd97"]] <- c(2, 1, 3, 1)
char2bi[["\U000fdd98"]] <- c(1, 3, 1, 3)
char2bi[["\U000fdd99"]] <- c(3, 1, 3, 1)

# [top, right, bottom, left] 0-none 1-light 2-heavy 3-matted-heavy
## Box chars
box2char <- list()
box2char[["0101"]] <- "\u2500"
box2char[["0202"]] <- "\u2501"
box2char[["1010"]] <- "\u2502"
box2char[["2020"]] <- "\u2503"
box2char[["0110"]] <- "\u250c"
box2char[["0210"]] <- "\u250d"
box2char[["0120"]] <- "\u250e"
box2char[["0220"]] <- "\u250f"
box2char[["0011"]] <- "\u2510"
box2char[["0012"]] <- "\u2511"
box2char[["0021"]] <- "\u2512"
box2char[["0022"]] <- "\u2513"
box2char[["1100"]] <- "\u2514"
box2char[["1200"]] <- "\u2515"
box2char[["2100"]] <- "\u2516"
box2char[["2200"]] <- "\u2517"
box2char[["1001"]] <- "\u2518"
box2char[["1002"]] <- "\u2519"
box2char[["2001"]] <- "\u251a"
box2char[["2002"]] <- "\u251b"
box2char[["1110"]] <- "\u251c"
box2char[["1210"]] <- "\u251d"
box2char[["2110"]] <- "\u251e"
box2char[["1120"]] <- "\u251f"
box2char[["2120"]] <- "\u2520"
box2char[["2210"]] <- "\u2521"
box2char[["1220"]] <- "\u2522"
box2char[["2220"]] <- "\u2523"
box2char[["1011"]] <- "\u2524"
box2char[["1012"]] <- "\u2525"
box2char[["2011"]] <- "\u2526"
box2char[["1021"]] <- "\u2527"
box2char[["2021"]] <- "\u2528"
box2char[["2012"]] <- "\u2529"
box2char[["1022"]] <- "\u252a"
box2char[["2022"]] <- "\u252b"
box2char[["0111"]] <- "\u252c"
box2char[["0112"]] <- "\u252d"
box2char[["0211"]] <- "\u252e"
box2char[["0212"]] <- "\u252f"
box2char[["0121"]] <- "\u2530"
box2char[["0122"]] <- "\u2531"
box2char[["0221"]] <- "\u2532"
box2char[["0222"]] <- "\u2533"
box2char[["1101"]] <- "\u2534"
box2char[["1102"]] <- "\u2535"
box2char[["1201"]] <- "\u2536"
box2char[["1202"]] <- "\u2537"
box2char[["2101"]] <- "\u2538"
box2char[["2102"]] <- "\u2539"
box2char[["2201"]] <- "\u253a"
box2char[["2202"]] <- "\u253b"
box2char[["1111"]] <- "\u253c"
box2char[["1112"]] <- "\u253d"
box2char[["1211"]] <- "\u253e"
box2char[["1212"]] <- "\u253f"
box2char[["2111"]] <- "\u2540"
box2char[["1121"]] <- "\u2541"
box2char[["2121"]] <- "\u2542"
box2char[["2112"]] <- "\u2543"
box2char[["2211"]] <- "\u2544"
box2char[["1122"]] <- "\u2545"
box2char[["1221"]] <- "\u2546"
box2char[["2212"]] <- "\u2547"
box2char[["1222"]] <- "\u2548"
box2char[["2122"]] <- "\u2549"
box2char[["2221"]] <- "\u254a"
box2char[["2222"]] <- "\u254b"
box2char[["1013"]] <- "\U000fdd8c"
box2char[["3101"]] <- "\U000fdd8d"
box2char[["1310"]] <- "\U000fdd8e"
box2char[["0131"]] <- "\U000fdd8f"
box2char[["1113"]] <- "\U000fdd90"
box2char[["3111"]] <- "\U000fdd91"
box2char[["1311"]] <- "\U000fdd92"
box2char[["1131"]] <- "\U000fdd93"
box2char[["1213"]] <- "\U000fdd94"
box2char[["3121"]] <- "\U000fdd95"
box2char[["1312"]] <- "\U000fdd96"
box2char[["2131"]] <- "\U000fdd97"
box2char[["1313"]] <- "\U000fdd98"
box2char[["3131"]] <- "\U000fdd99"

# nolint start
# ppgames_spreadsheet <- "https://docs.google.com/spreadsheets/d/1dd9HuSLgiYOJyP-7wyV3ZjAbnoDJgQR2BWWkD1JbXTc"
# googlesheets4::sheets_deauth()
# df <- googlesheets4::read_sheet(ppgames_spreadsheet)

# Fields
# players : numeric(+)
# length : numeric(2)
# equipment : character(1)
# designer : character(1)
# version : character(1)
# version_date : character(1)
# license : character(1)

# boardgamegeek : character(1)
# chessvariants : character(1)
# ludism : character(1)
# wikipedia : character(1)

# nolint end
unicode_dice <- c("\u2680", "\u2681", "\u2682", "\u2683", "\u2684", "\u2685")
# excludes card back
unicode_cards <- c(intToUtf8(utf8ToInt("\U0001f0a1") + 0:13, multiple = TRUE), # spades
                   intToUtf8(utf8ToInt("\U0001f0b1") + 0:13, multiple = TRUE), # hearts
                   intToUtf8(utf8ToInt("\U0001f0c1") + 0:13, multiple = TRUE), # diamonds
                   intToUtf8(utf8ToInt("\U0001f0d1") + 0:13, multiple = TRUE), # clubs
                   "\U0001f0bf", "\U0001f0cf", "\U0001f0df", # jokers
                   intToUtf8(utf8ToInt("\U0001f0e0") + 0:21, multiple = TRUE)) # trumps
card2rank <- list()
for (r in 1:14) {
    card2rank[[unicode_cards[r]]] <- r
    card2rank[[unicode_cards[r+14]]] <- r
    card2rank[[unicode_cards[r+28]]] <- r
    card2rank[[unicode_cards[r+42]]] <- r
}
card2rank[[unicode_cards[57]]] <- 15
card2rank[[unicode_cards[58]]] <- 15
card2rank[[unicode_cards[59]]] <- 15
card2rank[[unicode_cards[60]]] <- 22
for (r in 1:21) {
    card2rank[[unicode_cards[r+60]]] <- r
}
card2suit <- list()
for (r in 1:14) {
    card2suit[[unicode_cards[r]]] <- 2
    card2suit[[unicode_cards[r+14]]] <- 1
    card2suit[[unicode_cards[r+28]]] <- 4
    card2suit[[unicode_cards[r+42]]] <- 3
}
card2suit[[unicode_cards[57]]] <- 4
card2suit[[unicode_cards[58]]] <- 2
card2suit[[unicode_cards[59]]] <- 1
card2suit[[unicode_cards[60]]] <- 5
for (r in 1:21) {
    card2suit[[unicode_cards[r+60]]] <- 5
}

unicode_dominoes <- intToUtf8(utf8ToInt("\U0001f030") + 0:99, multiple = TRUE)
ranks <- c(NA_integer_, rep(0L, 7), # 0H
           0L, rep(1L, 6), # 1H
           0:1, rep(2L, 5), # 2H
           0:2, rep(3L, 4), # 3H
           0:3, rep(4L, 3), # 4H
           0:4, rep(5L, 2), # 5H
           0:5, 6L) # 6H
ranks <- c(ranks, ranks)
suits <- c(NA_integer_, 0:6, # 0H
           rep(1L, 2), 2:6, # 1H
           rep(2L, 3), 3:6,
           rep(3L, 4), 4:6,
           rep(4L, 5), 5:6,
           rep(5L, 6), 6L,
           rep(6L, 7))
suits <- c(suits, suits)
angles <- c(90, rep(90, 7),  # 0H
            rep(270, 1), rep(90, 6), # 1H
            rep(270, 2), rep(90, 5), # 2H
            rep(270, 3), rep(90, 4), # 3H
            rep(270, 4), rep(90, 3), # 4H
            rep(270, 5), rep(90, 2), # 5H
            rep(270, 6), rep(90, 1)) # 6H
angles <- c(angles, angles - 90)
tile2rank <- list()
tile2suit <- list()
tile2angle <- list()
for (i in seq_along(unicode_dominoes)) {
    d <- unicode_dominoes[i]
    tile2rank[[d]] <- ranks[i]
    tile2suit[[d]] <- suits[i]
    tile2angle[[d]] <- angles[i]
}
# chess
unicode_chess_black <- c("\u265f", "\u265e", "\u265d", "\u265c", "\u265b", "\u265a")
unicode_chess_white <- c("\u2659", "\u2658", "\u2657", "\u2656", "\u2655", "\u2654")

# built-in macros
macros <- list(H = "\u2665", S = "\u2660", C = "\u2663", D = "\u2666",
               WH = "\u2664", WS = "\u2661", WD = "\u2662", WC = "\u2667",
               RJ = unicode_cards[57], BJ = unicode_cards[58],
               WJ = unicode_cards[59], TF = unicode_cards[60],
               p = unicode_chess_black[1], n = unicode_chess_black[2], b = unicode_chess_black[3],
               r = unicode_chess_black[4], q = unicode_chess_black[5], k = unicode_chess_black[6],
               P = unicode_chess_white[1], N = unicode_chess_white[2], B = unicode_chess_white[3],
               R = unicode_chess_white[4], Q = unicode_chess_white[5], K = unicode_chess_white[6])
card_macros <- paste0(rep(c("S", "H", "D", "C"), each = 14),
                      rep(c("A", 2:9, "T", "J", "C", "Q", "K"), 4))
trump_macros <- paste0("T", 1:21)
#### Add macros for jokers and trumps
for (i in seq_along(card_macros)) {
    c <- card_macros[i]
    macros[[c]] <- unicode_cards[i]
}
for (i in seq_along(trump_macros)) {
    c <- trump_macros[i]
    macros[[c]] <- unicode_cards[i+60]
}

domino_macros <- paste0(rep(0:6, each=7), "-", rep(0:6, 7))
for (i in seq_along(domino_macros)) {
    t <- domino_macros[i]
    macros[[t]] <- unicode_dominoes[51 + i]
}

color_suits <- c("R", "K", "G", "B", "Y", "W")

save(r45, r90, r135, r180, r225, r270, r315,
     die_subs, top_subs,
     box2char, char2bi,
     unicode_dice,
     unicode_cards, card2rank, card2suit,
     unicode_dominoes, tile2rank, tile2suit, tile2angle,
     unicode_chess_black,
     unicode_chess_white,
     color_suits, macros,
     file="R/sysdata.rda", version=2)
