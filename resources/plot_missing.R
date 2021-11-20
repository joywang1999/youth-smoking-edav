library(tidyverse)
library(patchwork)

plot_missing <- function(data, percent = FALSE) {
  
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>% 
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  missing_tidy <- 
    missing_patterns %>% 
    rownames_to_column("id") %>%
    gather(key, value, -c(id,count), factor_key = TRUE) %>% 
    group_by(key) %>% 
    mutate(colcount=sum(value*count),
           total=sum(count)) %>% 
    ungroup() %>% 
    group_by(id) %>% 
    mutate(complete_cases=sum(value)==0) %>% 
    ungroup() %>%
    mutate(key = fct_reorder(key, colcount, .desc = TRUE),
           id = fct_rev(fct_inorder(id)))
  
  g1 <- missing_tidy %>% 
    ggplot(aes(x=key, y=id, fill=value, alpha=complete_cases)) + 
    geom_tile(color="white") + 
    geom_text(aes(label=ifelse(complete_cases, "complete cases", "")), check_overlap = TRUE) + 
    scale_alpha_manual(values = c(0.5, 1)) + 
    scale_fill_manual(values = c("grey", "blueviolet")) + 
    xlab("variable") + 
    ylab("missing pattern") + 
    theme_classic() +
    theme(legend.position="none",
          axis.text.x = element_text(angle=90)) 
  
  g2 <- missing_tidy %>% 
    distinct(id, count, total, complete_cases) %>% 
    { if (percent) mutate(., count=count/total*100) else . } %>%
    ggplot(aes(x=count, y=id, alpha=complete_cases)) + 
    geom_col(fill="cornflowerblue") + 
    scale_alpha_manual(values = c(0.5, 1)) + 
    xlab(ifelse(percent, "% rows", "row count")) + 
    xlim(0, ifelse(percent, 100, NA)) + 
    ylab("") + 
    theme_bw() + 
    # this is to reduce large blank in between plots
    theme(legend.position="none",
          axis.text.y = element_blank())
  
  g3 <- missing_tidy %>% 
    distinct(key, colcount, total) %>% 
    { if (percent) mutate(., colcount=colcount/total*100) else . } %>%
    ggplot(aes(x=key, y=colcount)) + 
    geom_col(fill="cornflowerblue", alpha=0.5) + 
    xlab("") + 
    ylab(ifelse(percent, "% rows\nmissing", "num rows\nmissing")) + 
    ylim(0, ifelse(percent, 100, NA)) + 
    theme_bw() + 
    # this is to reduce large blank in between plots
    theme(legend.position="none",
          axis.text.x = element_blank())
  
  return(g3 + plot_spacer() + g1 + g2  + plot_layout(widths = c(4,1), heights = c(1,4)))
}