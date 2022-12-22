#' Create visual and numeric summaries for feature comparison
#'
comparison <- function(df, feature) {
  # assumption is that df is a data frame with three variables:
  # first column is feature value for full scan
  # second column is feature value for cropped scan
  # third column is ground truth given as scan quality ordered from Good to Yikes
  names(df)[1:3] <- c("full", "cropped", "quality")

  upper = max(c(df$full, df$cropped), na.rm=TRUE)
  upper = upper * 1.1

  scatty = ggplot(df, aes(x=full, y=cropped)) +
    geom_abline(color = "grey50") +
    geom_point(aes(group=1), size=1, color="grey70", data=df[,-3]) +
    geom_point(size=2, alpha = 0.9, aes(color=quality)) +
    scale_color_manual("Scan quality", values=col_quality) +
    xlim(0,upper) +
    ylim(0,upper) +
    xlab("Full Scan") +
    ylab("Cropped Scan") +
    ggtitle(feature) +
    #geom_smooth(method=lm, se = FALSE, aes(group = 1)) +
    #stat_regline_equation(label.y = upper, aes(label = ..eq.label..)) +
    #stat_regline_equation(label.y = upper*0.9, aes(label = ..rr.label..)) +
    facet_grid(~quality) +
    theme(legend.position = "bottom")

  boxy = df %>% pivot_longer(1:2, names_to="Scan") %>%
    ggplot(aes( x = Scan, y = value, fill=Scan)) +
    geom_boxplot() + facet_grid(.~quality) +
    ylab(feature) +
    scale_fill_manual(values=c("yellow", "green"))

  summ = data.frame(min = numeric(),
                    firstQ = numeric(),
                    med = numeric(),
                    mean = numeric(),
                    thirdQ = numeric(),
                    max = numeric())

  summ[1,] = summary(df[,1])
  summ[2,] = summary(df[,2])
  rownames(summ) = c("Standard", "Cropped")
  attr(summ, "title") <-  paste("Summary Info of", feature)

  return(list(scatterplot=scatty, boxplot=boxy, summary=summ))
}