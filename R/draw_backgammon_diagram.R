# Black #000000
# Orange #E69F00
# Sky Blue #56B4E9
# Bluish Green #009E73
# Yellow #F0E442
# Blue #0072B2
# Vermillion #D55E00
# Reddish Purple #F079A7
library("piecepackr")
try(dev.off())
cfg <- list(suit_symbols="🌞,🌜,👑,⚜,꩜",
         suit_symbols_scale="0.6,0.7,0.75,0.9,0.9",
        # suit_symbols_scale.tile_face="1.0,1.1,1.1,1.4,0.9",
        suit_symbols_font="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
        suit_colors="#D55E00,#000000,#F0E442,#0072B2,#009E73",
        background_colors="#000000,#D55E00,#0072B2,#F0E442,white" 
        # ps_r.tile_face=sqrt(0.25^2 + 0.25^2), ps_theta.tile_face=-45,
        # gridline_colors.tile_face="#D55E00,#000000,#F0E442,#0072B2,#009E73"
)
draw_backgammon_board <- function(cfg) {
    y_top <- 4
    y_bot <- 1
    for(i_r in 1:6) {
        draw_component("tile_face", cfg, 3 + i_r %% 2, i_r, x=inch(26-2*i_r), y=inch(1))
        draw_component("tile_face", cfg, 3 + (i_r + 1) %% 2, i_r, x=inch(13-2*i_r), y=inch(y_top))
        draw_component("tile_face", cfg, 1 + (i_r + 1) %% 2, i_r, x=inch(26-2*i_r), y=inch(y_top), angle=180)
        draw_component("tile_face", cfg, 1 + (i_r + 0) %% 2, i_r, x=inch(13-2*i_r), y=inch(1), angle=180)
    }
    x_1 <- 25-2+1
    x_6 <- 25-12+1
    x_12 <- 1
    x_08 <- 5*2-1
    df <- tibble::tribble( ~component_side, ~x, ~y, ~i_s,
                          "coin_back", x_6 +0.5, y_bot+0.5, 4,
                          "coin_back", x_6 +0.5, y_bot-0.5, 4,
                          "coin_back", x_6 -0.5, y_bot+0.5, 4,
                          "coin_back", x_6 -0.5, y_bot-0.5, 4,
                          "coin_back", x_12+0.5, y_top+0.5, 3,
                          "coin_back", x_12+0.5, y_top-0.5, 3,
                          "coin_back", x_12-0.5, y_top+0.5, 3,
                          "coin_back", x_12-0.5, y_top-0.5, 3,
                          "coin_back", x_12-0.0, y_top-0.0, 3,
                          "coin_back", x_08+0.5, y_bot-0.5, 4,
                          "coin_back", x_08-0.5, y_bot+0.5, 4,
                          "coin_back", x_08+0.5, y_bot+0.5, 3)
    draw_components(df, cfg=cfg, units="inches")
    df <- tibble::tribble( ~component_side, ~x, ~y, ~i_s,
                          "coin_back", x_6 +0.5, y_top+0.5, 1,
                          "coin_back", x_6 +0.5, y_top-0.5, 1,
                          "coin_back", x_6 -0.5, y_top+0.5, 1,
                          "coin_back", x_6 -0.5, y_top-0.5, 1,
                          "coin_back", x_12+0.5, y_bot+0.5, 2,
                          "coin_back", x_12+0.5, y_bot-0.5, 2,
                          "coin_back", x_12-0.5, y_bot+0.5, 2,
                          "coin_back", x_12-0.5, y_bot-0.5, 2,
                          "coin_back", x_12-0.0, y_bot-0.0, 2,
                          "coin_back", x_08+0.5, y_top-0.5, 1,
                          "coin_back", x_08-0.5, y_top+0.5, 1,
                          "coin_back", x_08-0.5, y_top-0.5, 2)
    draw_components(df, cfg=cfg, units="inches", angle=180)
    draw_component("pawn_face", cfg, 4, x=inch(x_1-0.5), y=inch(y_top+0.5))
    draw_component("pawn_face", cfg, 3, x=inch(x_1+0.5), y=inch(y_top-0.5))
    draw_component("pawn_face", cfg, 2, x=inch(x_1-0.5), y=inch(y_bot+0.5), angle=180)
    draw_component("pawn_face", cfg, 1, x=inch(x_1+0.5), y=inch(y_bot-0.5), angle=180)
    draw_component("die_face", cfg, 4, 1, x=inch(x_6), y=inch(y_bot))
    draw_component("die_face", cfg, 1, 1, x=inch(x_6), y=inch(y_top), angle=180)
    draw_component("die_face", cfg, 3, 2, x=inch(6.5-0.5), y=inch(y_bot+1.5))
    draw_component("die_face", cfg, 2, 2, x=inch(6.5+0.5), y=inch(y_bot+1.5), angle=180)
}
# dev.new(width=25, height=5)
# inkscape -z -e images/backgammon.png -w 1200 -h 240 images/backgammon.svg 
svg("images/backgammon.svg", width = 25, height=5)
draw_backgammon_board(cfg)
dev.off()
