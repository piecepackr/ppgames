# Black #000000
# Orange #E69F00
# Sky Blue #56B4E9
# Bluish Green #009E73
# Yellow #F0E442
# Blue #0072B2
# Vermillion #D55E00
# Reddish Purple #F079A7
backgammon_cfg <- list(suit_text="🌞,🌜,👑,⚜,꩜",
         suit_scale="0.6,0.7,0.75,0.9,0.9",
        # suit_symbols_scale.tile_face="1.0,1.1,1.1,1.4,0.9",
        suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
        suit_color="#D55E00,#000000,#F0E442,#0072B2,#009E73",
        background_color="#000000,#D55E00,#0072B2,#F0E442,white" 
        # ps_r.tile_face=sqrt(0.25^2 + 0.25^2), ps_theta.tile_face=-45,
        # gridline_colors.tile_face="#D55E00,#000000,#F0E442,#0072B2,#009E73"
)

tablut_cfg <- list(suit_text="🌞,🌜,👑,⚜,꩜",
         suit_scale="0.6,0.7,0.75,0.9,0.9",
        suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
        suit_color="darkred,black,darkgreen,darkblue,black",
        invert_colors.suited = TRUE
)

cfgs <- list(backgammon = backgammon_cfg, tablut = tablut_cfg)
