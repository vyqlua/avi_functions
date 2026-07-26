This helper script contains three functions for ipsatizing + calculating the AVI, calculating the alpha of AVI composites, and plotting AVI scores.

</span>

### to use:

To import these functions in R, use `source("https://raw.githubusercontent.com/vyqlua/avi_functions/refs/heads/main/ipsatize_fn.R")`.

</span>

#### `ipsatize_avi(data, item_stem, full_avi, remove, maximizing_pos)`

**Description:** This is a function for ipsatizing AVI items and calculating composite AVI scores. 
All AVI items should start with the same starting string (e.g., "i." for ideal affect AVI items), and the items should contain the names of the emotion (or shortened 4-letter versions of the names. for `sad`, it can be labelled as `sad` or `sadx`). The different AVI composite scores are calculated as follows:

HAP = enth | exci | elat | euph ; 

LAP = rela | calm | peac | sere ;

HAN = fear | host | nerv | angr ;

LAN = dull | slee | slug ;

POS = happ | cont | sati ;

NEG = unha | sad | lone .

**Arguments:**

`data` : dataframe. note the requirements regarding how variables should be named for the AVI calculation. if you'd like to be extra cautious, you can filter out a subset of data so that the datafile only contains the participant ID and the AVI variables.

`item_stem` : what is the item stem for your AVI items? (e.g., "i." for ideal affect AVI items). they will need to START with the item stem. 

`full_avi` : defaults to `FALSE`. set to `TRUE` if you'd like to calculate `POS` and `NEG` composites. 

`remove` : defaults to `NULL`. if you'd like to remove any items (e.g., *euphoric*) from the computation of the composite score(s), specify them as a string (e.g., `"euphoric"`), or a vector (e.g., `c("euphoric","elated")`). the function does a `dplyr::select(contains())` search on the specified string/ vector. 

`maximizing_pos` : defaults to `FALSE`. set to `TRUE` to calculate maximizing positivity scores (ideal HAP, POS, and NEG minus ideal HAN, NEG, and LAN).

</span>

#### `alphas_avi(data, item_stem, full_avi, remove, maximizing_pos, group_id)`

**Description:** This is a function for calculating the alpha values of the AVI variables. Similar to the first function, all AVI items should start with the same item_stem, and the items should contain the names of the emotion.

**Arguments:**

`data` : dataframe. note the requirements regarding how variables should be named for the AVI calculation. if you'd like to be extra cautious, you can filter out a subset of data so that the datafile only contains the participant ID and the AVI variables.

`item_stem` : what is the item stem for your AVI items? (e.g., "i." for ideal affect AVI items). they will need to START with the item stem. 

`full_avi` : defaults to `FALSE`. set to `TRUE` if you'd like to calculate `POS` and `NEG` alphas. 

`remove` : defaults to `NULL`. if you'd like to remove any items (e.g., *euphoric*) from the computation of the alphas, specify them as a string (e.g., `"euphoric"`), or a vector (e.g., `c("euphoric","elated")`). the function does a `dplyr::select(contains())` search on the specified string/ vector. 

`maximizing_pos` : defaults to `FALSE`. set to `TRUE` to calculate maximizing positivity alpha (ideal HAP, POS, and NEG minus ideal HAN, NEG, and LAN).

`group_id` : defaults to `FALSE`. optional argument to calculate avi alphas for specific subgroups (e.g., culture subgroups). specify a string indicating the name of the grouping variable.

</span>

#### `plot_avi(data, group_id, full_avi, specify_colors, ipsatized_only)`

**Description:** This is a function for plotting composite AVI scores and getting descriptives for AVI scores. It plots real and ideal affect (and will plot only either if your dataframe does not have either). If you used the above function to ipsatize and calculate your AVI scores, the dataframe would likely be well set up for this function.

**Arguments:**

`data` : dataframe. note the requirements regarding how variables should be named for the AVI calculation. if you'd like to be extra cautious, you can filter out a subset of data so that the datafile only contains the participant ID and the AVI composite variables.

`group_id` : defaults to `FALSE`. optional argument to plot 2 groups (e.g., cultural groups) in the same plot. specify a string indicating the name of the grouping variable.

`full_avi` : defaults to `FALSE`. set to `TRUE` if you'd like to plot `POS` and `NEG` AVI variables.

`specify_colors` : defaults to `FALSE`, and will plot single groups in grey and two groups in blue (#52B2CF) and orange (#F49070). if you specified a `group_id`, you can list how you'd like the colors to map using a vector (e.g., "group1" = "red", "group2" = "blue"). currently, the function does not support specifying colors for single groups.

`ipsatized_only` :  defaults to `TRUE`. only ipsatized scores are plotted. if you'd also like to plot the raw scores, set this argument to `FALSE`.


</span>

___ 

### future updates/ to dos:

- Modify `plot_avi()` function to be more modifiable (e.g., plot avoided affect, etc.)
- Modify `plot_avi()` function plot predicted instead of raw values
- Generally make the functions neater

___

### log of changes/ updates:

*22 Sep 2024*: Uploaded first version of functions

*23 Sep 2024*: Modified `ipsatize_avi()` function to allow for removing specific items in composite score (since `euphoric` is often removed to improve internal reliability of HAP composite scores)

*25 Sep 2025*: Updated function to be able to calculate maximizing positivity.

~~*27 Jan 2026*: Made the codebook that's printed mapping successfully computed AVI variables to the items used in the computation much much prettier and less annoying to read when printed.~~

*10 Mar 2026*: Saved codebook to environment instead of printing it out.

*11 Mar 2026*: Changed tail of ipsatized variables to _i instead of _ip

*24 May 2026*: Changed selecting logic from contain("_i") to ends_with("_i")

*26 Jul 2026*: Added alpha function. Also added some safeguards for re-running the ipsatize avi function.

*26 Jul 2026*: Updated plot_avi function to modify group colors and plot single groups. Also updated readme for clarity.
