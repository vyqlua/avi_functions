#  ----------------------------------------
#  | AVI Ipsatization & Plotting Function |
#  ----------------------------------------

# How to use:

  # ipsatize_avi(data, item_stem, full_avi = FALSE)
    #  1. At the top of your script, specify `source("https://raw.githubusercontent.com/vyqlua/avi_functions/refs/heads/main/ipsatize_fn.R")`. 
    #     Note that the `ipsatize_fn.R` file (i.e., this file) needs to be the correct working directory
    #  2. Ensure all AVI subscale items have a similar item stem (e.g,. "r.sadx", "r.quiet", etc.)
    #  3. Use function. Your script should look something like this:
    #     `data <- data %>% ipsatize_avi("r.")`

  # plot_avi(data, group_id, full_avi = FALSE, flip_color = FALSE, ipsatized_only = TRUE)
    #  1. At the top of your script, specify `source("https://raw.githubusercontent.com/vyqlua/avi_functions/refs/heads/main/ipsatize_fn.R")`. 
    #     Note that the `ipsatize_fn.R` file (i.e., this file) needs to be the correct working directory
    #  2. Ensure dataframe is NOT already in long format.
    #  3. Use function. Your script should look something like this:
    #     `data <- data %>% plot_avi(group_id = "Culture")`
    #  4. If the plot comes out and the colors are flipped, indicate `flip_culture = TRUE`.
    #  5. If you want to plot the raw scores as well, indicate `ipsatized_only = FALSE`.
    #  6. If you want to plot `POS` and `NEG` as well, indicate `full_avi = TRUE`.

# --------------------------------------------

ipsatize_avi <- function(data, item_stem, full_avi = FALSE, remove = NULL, maximizing_pos = FALSE) {
  
  library(dplyr)
  library(stringr)
  
    temp_data <- data %>% 
      rowwise() %>%
      dplyr::mutate(
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "Mean") := mean(c_across(starts_with(paste(item_stem))), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "Sd") := sd(c_across(starts_with(paste(item_stem))), na.rm = TRUE)) %>%
    ungroup()
  
    temp_data <- temp_data %>%
      rowwise() %>%
      dplyr::mutate(
        across(
          starts_with(item_stem), 
          ~ (. - get(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "Mean"))) / get(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "Sd")),
          .names = "{col}_ip"
        )
      ) %>%
      ungroup()
    
    message("Items ipsatized successfully. Variables created:")
    newvars <- setdiff(names(temp_data), names(data))
    print(newvars)
    
    if (!is.null(remove)) {
      temp_data2 <- temp_data %>% dplyr::select(-contains(remove))
    } else {
      temp_data2 <- temp_data
      }
    
    temp_data2 <- temp_data2 %>% 
      rowwise() %>%
      dplyr::mutate(
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAP") := 
          mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                          matches("enth|exci|elat|euph", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAP") := 
          mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                          matches("rela|calm|peac|sere", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAN") := 
          mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                          matches("fear|host|nerv", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAN") := 
          mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                          matches("dull|slee|slug", ignore.case = TRUE)), na.rm = TRUE),
        
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAP_ip") := 
          mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                          matches("enth|exci|elat|euph", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAP_ip") := 
          mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                          matches("rela|calm|peac|sere", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAN_ip") := 
          mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                          matches("fear|host|nerv", ignore.case = TRUE)), na.rm = TRUE),
        !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAN_ip") := 
          mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                          matches("dull|slee|slug", ignore.case = TRUE)), na.rm = TRUE)
        ) %>% ungroup()
 
    
    if (full_avi == TRUE) {
      temp_data2 <- temp_data2 %>% 
        rowwise() %>%
        dplyr::mutate(
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "POS") := 
            mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                            matches("happ|cont|sati", ignore.case = TRUE) & !contains("unha")), na.rm = TRUE),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "NEG") := 
            mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                            matches("unha|sad|lone", ignore.case = TRUE)), na.rm = TRUE),
          
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "POS_ip") := 
            mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                            matches("happ|cont|sati", ignore.case = TRUE) & !contains("unha")), na.rm = TRUE),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "NEG_ip") := 
            mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                            matches("unha|sad|lone", ignore.case = TRUE)), na.rm = TRUE)
          
        ) %>% ungroup()
    }
    
    if (maximizing_pos == TRUE) {
      temp_data2 <- temp_data2 %>% 
        rowwise() %>%
        dplyr::mutate(
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAPPOSLAP") := 
            mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                            matches("enth|exci|elat|euph|rela|calm|peac|sere|happ|cont|sati", ignore.case = TRUE) & !contains("unha")), na.rm = TRUE),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAPPOSLAP_ip") := 
            mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                            matches("enth|exci|elat|euph|rela|calm|peac|sere|happ|cont|sati", ignore.case = TRUE) & !contains("unha")), na.rm = TRUE),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HANNEGLAN") := 
            mean(c_across(starts_with(paste(item_stem)) & !ends_with("_ip") & 
                            matches("fear|host|nerv|unha|sad|lone|dull|slee|slug", ignore.case = TRUE)), na.rm = TRUE),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HANNEGLAN_ip") := 
            mean(c_across(starts_with(paste(item_stem)) & ends_with("_ip") & 
                            matches("fear|host|nerv|unha|sad|lone|dull|slee|slug", ignore.case = TRUE)), na.rm = TRUE))
      temp_data2 <- temp_data2 %>% 
        rowwise() %>%
        dplyr::mutate(
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "MaxPos") := 
            !!sym(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAPPOSLAP")) - 
            !!sym(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HANNEGLAN")),
          !!paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "MaxPos_ip") := 
            !!sym(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAPPOSLAP_ip")) - 
            !!sym(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HANNEGLAN_ip")))
      
      temp_data3 <- temp_data2 %>% dplyr::select(contains("HAPPOSLAP"), contains("HANNEGLAN"))
      temp_data2 <- temp_data2 %>% dplyr::select(-contains("HAPPOSLAP"), -contains("HANNEGLAN"))
    }
          
    message("AVI Variables Computed: ")
    newvars2 <- setdiff(names(temp_data2), names(temp_data))
    print(newvars2)
    
    avi_codebook <- data.frame(
      variables = c(paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAP"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "POS"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAP"),
                    
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAN"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "NEG"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAN"),
                    
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAP_i"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "POS_i"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAP_i"),
                    
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "HAN_i"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "NEG_i"),
                    paste0(substr(item_stem, nchar(item_stem) - 1, nchar(item_stem) - 1), "LAN_i")),
      
      items = c(temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("enth|exci|elat|euph", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("happ|cont|sati", ignore.case = TRUE) & !contains("unha")) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("rela|calm|peac|sere", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("fear|host|nerv", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("unha|sad|lone", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & !ends_with("_ip") & matches("dull|slee|slug", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("enth|exci|elat|euph", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("happ|cont|sati", ignore.case = TRUE) & !contains("unha")) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("rela|calm|peac|sere", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("fear|host|nerv", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("unha|sad|lone", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", "),
                temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & matches("dull|slee|slug", ignore.case = TRUE)) %>% names() %>% paste(collapse = ", ")
                )
      )
    
    if (full_avi == FALSE) {
      avi_codebook <- avi_codebook %>%
        dplyr::filter(!str_detect(variables, "POS") & !str_detect(variables, "NEG"))
    }
    
    if(maximizing_pos == TRUE) {
      avi_codebook <- 
        rbind(avi_codebook,
              data.frame(variables = c(temp_data2 %>% dplyr::select(contains("MaxPos") & !contains("ip")) %>% colnames(),
                                       temp_data2 %>% dplyr::select(contains("MaxPos") & contains("ip")) %>% colnames(),
                                       "HAPPOSLAP (intermediate variable)", "HANNEGLAN (intermediate variable)"),
                         items = c(paste(temp_data3 %>% dplyr::select(contains("HAPPOSLAP") & !contains("ip")) %>% colnames(), "-",
                                         temp_data3 %>% dplyr::select(contains("HANNEGLAN") & !contains("ip")) %>% colnames(),
                                         sep = " "),
                                   paste(temp_data3 %>% dplyr::select(contains("HAPPOSLAP") & contains("ip")) %>% colnames(), "-",
                                         temp_data3 %>% dplyr::select(contains("HANNEGLAN") & contains("ip")) %>% colnames(),
                                         sep = " "),
                                   temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & 
                                                                  matches("enth|exci|elat|euph|rela|calm|peac|sere|happ|cont|sati", ignore.case = TRUE) &
                                                                  !contains("unhappy")) %>% 
                                     names() %>% paste(collapse = ", "),
                                   temp_data2 %>% dplyr::select(starts_with(paste(item_stem)) & ends_with("_ip") & 
                                                                  matches("fear|host|nerv|unha|sad|lone|dull|slee|slug", ignore.case = TRUE)) %>% 
                                     names() %>% paste(collapse = ", ")
                                   )))
    }
  
    print(avi_codebook)
    
    common_columns <- intersect(names(data), names(temp_data2))
    temp_data2 <- merge(data, temp_data2, by = common_columns)
    return(temp_data2)
}

# --------------------------------------------

plot_avi <- function(data, group_id, full_avi = FALSE, flip_color = FALSE, ipsatized_only = TRUE) {
  
  library(ggplot2)
  
  if (!group_id %in% colnames(data)) {
    "Error: Specified `group_id` column not found in the dataset."
  }
  
plot_data <- data %>% dplyr::select(
    group_id, 
    contains("HAP") & !contains("HAPP"), 
    contains("LAP"), 
    contains("POS") & !contains("Aff"), 
    contains("HAN"), 
    contains("LAN"), 
    contains("NEG") & !contains("Aff")) 
  
  plot_data <- plot_data %>% 
    pivot_longer(cols = -group_id,
                 names_to = "AVI",
                 values_to = "value")
  
plot_data <- plot_data %>%
  group_by(!!sym(group_id), AVI) %>%
    summarize(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      se = sd(value, na.rm = TRUE)/sqrt(length(value)))

plot_data <- plot_data %>%
  mutate(
    AVI_order = case_when(
      grepl("HAP", AVI) ~ 1,
      grepl("POS", AVI) ~ 2, 
      grepl("LAP", AVI) ~ 3, 
      grepl("HAN", AVI) ~ 4, 
      grepl("NEG", AVI) ~ 5,  
      grepl("LAN", AVI) ~ 6,  
      TRUE ~ 7               
    ),
    AVI = factor(AVI, levels = unique(AVI[order(AVI_order)])) # Reorder based on 'AVI_order'
  )

if (flip_color == TRUE) {
  colours <- c("#52B2CF", "#F49070")
} else {
  colours <- c("#F49070", "#52B2CF")
}

if (ipsatized_only == FALSE) {
  r_nonip <- 
    ggplot(plot_data %>% 
           dplyr::filter(
             !grepl("_ip", AVI),
             grepl("r", AVI)), 
         aes(x = AVI, y = mean, fill = !!sym(group_id))) +
    scale_fill_manual(values = colours) +
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.1)+
    xlab("AVI") +
    ylab("Non-Ipsatized Scores") +
    ggtitle("Group Differences in Actual Affect") +
    jtools::theme_apa() }

r_ip <- 
  ggplot(plot_data %>% 
         dplyr::filter(
           grepl("_ip", AVI),
           grepl("r", AVI)), 
       aes(x = AVI, y = mean, fill = !!sym(group_id))) +
  scale_fill_manual(values = colours) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.1)+
  xlab("AVI") +
  ylab("Ipsatized Scores") +
  ggtitle("Group Differences in Actual Affect") +
  jtools::theme_apa() 

if (ipsatized_only == FALSE) {
  i_nonip <- 
    ggplot(plot_data %>% 
           dplyr::filter(
             !grepl("_ip", AVI),
             !grepl("r", AVI)), 
         aes(x = AVI, y = mean, fill = !!sym(group_id))) +
    scale_fill_manual(values = colours) +
    geom_bar(stat='identity', position='dodge') +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.1)+
    xlab("AVI") +
    ylab("Non-Ipsatized Scores") +
    ggtitle("Group Differences in Ideal Affect") +
    jtools::theme_apa() }

i_ip <- 
  ggplot(plot_data %>% 
         dplyr::filter(
           grepl("_ip", AVI),
           !grepl("r", AVI)), 
       aes(x = AVI, y = mean, fill = !!sym(group_id))) +
  scale_fill_manual(values = colours) +
  geom_bar(stat='identity', position='dodge') +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(0.9), width = 0.1)+
  xlab("AVI") +
  ylab("Ipsatized Scores") +
  ggtitle("Group Differences in Ideal Affect") +
  jtools::theme_apa() 

if (ipsatized_only == FALSE) {
  print(plot_data %>% dplyr::filter(grepl("r", AVI)) %>% arrange(AVI_order, Culture))
  print(plot_data %>% dplyr::filter(!grepl("r", AVI)) %>% arrange(AVI_order, Culture))
  print(r_nonip)
  print(r_ip)
  print(i_nonip)
  print(i_ip)
} else {
  print(plot_data %>% dplyr::filter(grepl("_ip", AVI), grepl("r", AVI)) %>% arrange(AVI_order, Culture))
  print(plot_data %>% dplyr::filter(grepl("_ip", AVI), !grepl("r", AVI)) %>% arrange(AVI_order, Culture))
  print(r_ip)
  print(i_ip)
}
}
