### avi_functions

Functions for calculating and plotting AVI variables easily. These are some intended future updates (i.e., currently, the functions are unable to...):
- Modify `ipsatize_avi()` function to calculate a composite score combining all 3 arousal levels
- Modify `ipsatize_avi()` function to calculate cronbach alphas
- Modify `plot_avi()` function to be more modifiable (e.g., color scheme, plot avoided affect, etc.)

___

### to use:
To import these functions in R, use `source("https://raw.githubusercontent.com/vyqlua/avi_functions/refs/heads/main/ipsatize_fn.R")`.

1. `ipsatize_avi(data, item_stem, full_avi = FALSE, remove = NULL)`

This is a function for ipsatizing AVI items and calculating composite AVI scores. 

All AVI items should start with the same starting string (e.g., "i." for ideal affect AVI items), and the items should contain the names of the emotion (or shortened 4-letter versions of the names. for `sad`, it can be labelled as `sad` or `sadx`). The different AVI composite scores are calculated as follows:

- HAP = enth | exci | elat | euph
- LAP = rela | calm | peac | sere

- HAN = fear | host | nerv | angr
- LAN = dull | slee | slug

- POS = happ | cont | sati
- NEG = unha | sad | lone

Note that functions do not automatically calculate `POS` and `NEG`. Add argument `full_avi = TRUE` if you want to calculate those two variables as well. If you'd like to remove any items (e.g., `euphoric`) from the computation of the composite score(s), specify `remove = "euphoric"`. The remove argument can also be a vector (e.g., `c("euphoric","elated")`), and the function does a `dplyr::select(contains())` search on the specified string/ vector.

2. `plot_avi(data, group_id, full_avi = FALSE, flip_color = FALSE, ipsatized_only = TRUE)`

This is a function for plotting composite AVI scores and getting descriptives for AVI scores. 

If the plot comes out and the colors are flipped, indicate `flip_color = TRUE`. If you want to plot the raw scores as well, indicate `ipsatized_only = FALSE`. If you want to plot `POS` and `NEG` as well, indicate `full_avi = TRUE`.

</span>

___ 

### log of changes/ updates:

*22 Sep 2024*: Uploaded first version of functions

*23 Sep 2024*: Modified `ipsatize_avi()` function to allow for removing specific items in composite score (since `euphoric` is often removed to improve internal reliability of HAP composite scores)
