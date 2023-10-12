library(tidyverse)
library(treemapify)

# set theme ----
theme_set(theme_classic())
theme_update(axis.text  = element_text(size = 16, colour = "black"),
             axis.title = element_text(size = 16, colour = "black"),
             plot.title = element_text(size = 20, colour = "black"),
             strip.text = element_text(size = 16, colour = "black"))

# barplot_count ----
barplot_count = function(df, var, title = NA, high_n = 50, xlab = "Number of patients"){
  df %>% 
    drop_na({{var}}) %>% 
    ggplot(aes(y = fct_rev(fct_infreq({{var}})) %>% fct_relevel("Missing"))) +
    geom_bar(colour = "#2166ac", fill = "#d1e5f0") +
    geom_text(stat='count', aes(label=comma(after_stat(count)),
                                hjust = if_else(after_stat(count) > high_n, 1, 0)),
              size  = 10) +
    scale_y_discrete(labels = label_wrap(15)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    ggtitle(str_wrap(title, 30)) +
    ylab("") +
    xlab(xlab)
  
}

# histogram ----
histogram = function(df, var, title = NA, xlab = NA, ylab = NA){
  df %>% 
    ggplot(aes({{var}})) +
    geom_histogram(binwidth = 10) +
    ggtitle(str_wrap(title, 30)) +
    xlab(xlab) +
    ylab(ylab) +
    scale_x_continuous(expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1), add = c(0, 0)))
}


# treemap ----
mytreemap = function(df, group1, group2){
  
  df_counts = df %>% 
    select({{group1}}, {{group2}}) %>% 
    separate_longer_delim({{group2}}, delim = ",") %>% 
    drop_na() %>% 
    count({{group1}}, {{group2}})
  
  # from Claus Wilke - Fundamentals of Data Visualization, Chapter 11 Visualizing Nested Proportions
  ## manually add colors
  # hues
  hues = c(300, 220, 100, 50) # purple, blue, green, beige
  
  
  # minimum and maximum population density
  minval = min(pull(df_counts, n))
  maxval = max(pull(df_counts, n))
  
  # df = train_grade
  # minval = min(pull(df, n))
  # maxval = max(pull(df, n))
  
  # turn value into color
  df_colour <- df_counts %>%
    mutate(index = as.numeric(factor({{group1}}))) %>%
    group_by(index) %>%
    mutate(
      value = (n-minval)/(maxval-minval),
      fill = scales::gradient_n_pal(
        colorspace::sequential_hcl(
          6,
          h = hues[index[1]],
          c = c(45, 20),
          l = c(30, 80),
          power = .5
        )
      )(1-value)
    )
  
  df_colour %>% 
    ggplot(aes(area = n, subgroup = {{group1}}, fill = fill)) +
    geom_treemap(color = "white", size = 0.5*.pt, alpha = NA) + 
    geom_treemap_subgroup_text(
      colour = "white",
      place = "bottom", alpha = 0.7,
      grow = TRUE
    ) +
    geom_treemap_subgroup_border(color = "white") +
    geom_treemap_text(
      aes(label = {{group2}}, size = 5*n),
      color = "black",
      place = "topleft",
      min.size = 5
    ) +
    scale_size_identity() +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    guides(colour = "none", fill = "none")
}
#ggsave("treemap_ggplot.png", width = 20, height = 10)

#mytreemap(site_survey, wb, train_grade)
