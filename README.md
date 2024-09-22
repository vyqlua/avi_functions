# avi_functions

Functions for calculating and plotting AVI variables easily


# Current Status: 22 Sep 2024

To import these functions in R, use `source("https://raw.githubusercontent.com/vyqlua/avi_functions/refs/heads/main/ipsatize_fn.R")`.

Created functions for ipsatizing AVI items and calculating composite AVI scores. [`ipsatize_avi(data, item_stem, full_avi = FALSE)`]

All AVI items should start with the same starting string (e.g., "i." for ideal affect AVI items), and the items should contain the names of the emotion (or shortened 4-letter versions of the names. for `sad`, it can be labelled as `sad` or `sadx`). The different AVI composite scores are calculated as follows:

- HAP = enth | exci | elat | euph
- LAP = rela | calm | peac | sere

- HAN = fear | host | nerv | angr
- LAN = dull | slee | slug

- POS = happ | cont | sati
- NEG = unha | sad | lone

Note that functions do not automatically calculate `POS` and `NEG`. Add argument `full_avi = TRUE` if you want to calculate those two variables as well.

Also created functions for plotting composite AVI scores and getting descriptives for AVI scores [`plot_avi(data, group_id, full_avi = FALSE, flip_color = FALSE, ipsatized_only = TRUE)`]. If the plot comes out and the colors are flipped, indicate `flip_culture = TRUE`. If you want to plot the raw scores as well, indicate `ipsatized_only = FALSE`. If you want to plot `POS` and `NEG` as well, indicate `full_avi = TRUE`.


# Future Updates

I intend to update the `plot_avi()` function to be able to plot avoided affect as well.