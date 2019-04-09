library("piecepackr")
library("tibble")
try(dev.off())
cfg <- list(suit_symbols="🌞,🌜,👑,⚜,꩜",
         suit_symbols_scale="0.6,0.7,0.75,0.9,0.9",
        suit_symbols_font="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
        suit_colors="darkred,black,darkgreen,darkblue,black",
        invert_colors.suited = TRUE
)

draw_tablut_board <- function(cfg) {
    df <- tibble(component_side="tile_back", 
                         x=rep(seq(2,8,by=2), 4),
                         y=rep(seq(2,8,by=2), each=4))
    draw_components(df, cfg=cfg, units="inches")
    df <- tibble(component_side="coin_face",
                 i_r=rep(3:6, 4),
                 x=c(5,4,5,6,5,6,5,4,2,1,1,1,8,9,9,9),
                 y=c(2,1,1,1,8,9,9,9,5,4,5,6,5,6,5,4),
                 angle=c(rep(0,4),rep(180,4), rep(-90, 4), rep(90, 4)))
    draw_components(df, cfg=cfg, units="inches")
    df <- tibble(component_side="coin_back",
                 i_s=rep(1:4, each=2),
                 x=c(5,5,6,7,5,5,4,3),
                 y=c(6,7,5,5,4,3,5,5),
                 angle=rep(c(0, -90, 180,  90), each=2))
    draw_components(df, cfg=cfg, units="inches")
    df <- tibble(component_side="die_face",
                 i_s=1:4, i_r=1,
                 x=c(4.75,5.25,5.25,4.75),
                 y=c(5.25,5.25,4.75,4.75),
                 angle=c(0,-90,180,90))
    draw_components(df, cfg=cfg, units="inches")
    draw_component("pawn_face", cfg, 3, x=inch(5), y=inch(5))
}

# dev.new(width=10, height=10)
# inkscape -z -e images/tablut.png -w 1024 -h 1024 images/tablut.svg
svg("images/tablut.svg", width=10, height=10)
draw_tablut_board(cfg)
dev.off()
