# https://en.wikipedia.org/wiki/Transformation_of_text#Reversed_text

r90 <- list()
r180 <- list()
r270 <- list()

## Spaces, Letters, Numbers

r90[[" "]] <- " "
r90[["A"]] <- "\u2222"
r90[["1"]] <- "\u21bd"
r90[["\u21bf"]] <- "\u21bd"
# r90[["1"]] <- "\u295e" # nolint
r90[["\u2960"]] <- "\u295e"

r180[[" "]] <- " "
# nolint start
# r180[["\u2202"]] <- "e"
# r180[["\u2202"]] <- "\u03ed"
# nolint end
r180[["\u2202"]] <- "\u03f1"
r180[["n"]] <- "u"
r180[["N"]] <- "N"
r180[["0"]] <- "0"
r180[["a"]] <- "\u0250"
r180[["A"]] <- "\u2200"
r180[["1"]] <- "\u21c2"
r180[["\u21bf"]] <- "\u21c2"
# r180[["1"]] <- "\u295d" # nolint
r180[["\u2960"]] <- "\u295d"
r180[["2"]] <- "\u218a"
r180[["3"]] <- "\u218b"
r180[["4"]] <- "\u152d"
# nolint start
# r180[["4"]] <- "\u3123"
# r180[["4"]] <- "\u07c8"
# r180[["5"]] <- "\u03da"
# nolint end
r180[["5"]] <- "\u2185\u0332"
r180[["6"]] <- "9"

r270[[" "]] <- " "
r270[["1"]] <- "\u21c0"
r270[["\u21bf"]] <- "\u21c0"
# r270[["1"]] <- "\u295b" # nolint
r270[["\u2960"]] <- "\u295b"
r270[["3"]] <- "\u03c9"

## Piecepack/French Suits

r90[["\u2600"]] <- "\u2600" # Suns
r90[["\u2609"]] <- "\u2609"
r90[["\u263c"]] <- "\u263c"
r90[["\u25d0"]] <- "\u25d3" # Moons
r90[["\u25d8"]] <- "\u25d8"
r90[["\u2665"]] <- "\u2765" # Rotated Black Heart
r90[["\u2764"]] <- "\u2765"

r180[["\u2600"]] <- "\u2600" # Suns
r180[["\u2609"]] <- "\u2609"
r180[["\u263c"]] <- "\u263c"
r180[["\u263e"]] <- "\u263d"
r180[["\u263d"]] <- "\u263e" ## Moons
r180[["\u25d0"]] <- "\u25d1"
r180[["\u25d8"]] <- "\u25d8"
r180[["\u2641"]] <- "\u2640" ## Crowns (Earth)
r180[["\u2640"]] <- "\u2641" #         (Venus)
r180[["\u0238"]] <- "\u0239" #         Small Letter db Digraph
r180[["\u2020"]] <- "\u2e38" ## Swords (Dagger)
r180[["\u2e38"]] <- "\u2020" ##        Turned Dagger
r180[["\u2021"]] <- "\u2021" ##        Double Dagger
r180[["\u2666"]] <- "\u2666" # Diamond Suits (Black)
r180[["\u2662"]] <- "\u2662" #               (White)
r180[["\u260a"]] <- "\u260b" # Ascending/Descending nodes
r180[["\u260b"]] <- "\u260a" # Ascending/Descending nodes
r180[["\u0ed1"]] <- "\u0ed2" ## Spirals (Lao Digit One)

r270[["\u2600"]] <- "\u2600" # Suns
r270[["\u2609"]] <- "\u2609"
r270[["\u263c"]] <- "\u263c"
r270[["\u25d0"]] <- "\u25d2" # Moons
r270[["\u25d8"]] <- "\u25d8"

## Chess symbols

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

## Box info
# [top, right, bottom, left] 0-none 1-light 2-dark
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

# [top, right, bottom, left] 0-none 1-light 2-dark
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

license_names <- list(`CC-BY-SA-4` = "Creative Commons Attribution-ShareAlike 4.0 International License")
# license_names <- list(`CC-BY-SA-4` = "CC BY-SA 4.0")
license_urls <- list(`CC-BY-SA-4` = "https://creativecommons.org/licenses/by-sa/4.0")


save(r90, r180, r270, box2char, char2bi,
     license_names, license_urls,
     file="R/sysdata.rda", version=2)
