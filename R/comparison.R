# color scheme for good, tiny problems, bad to yikes
col_quality <- brewer.pal(n=8, name="RdBu")[rev(c(1:3, 5, 7))]

# color scheme for standard versus cropped scans
col_scans_light <- brewer.pal(n=9, name="BrBG")[c(3,7)]
col_scans_dark <- brewer.pal(n=9, name="BrBG")[c(2,8)]


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
    ggtitle(feature) +
    scale_fill_manual(values=col_scans_light)

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

  metric <- df %>% mutate(quality = quality %in% c("Good", "Tiny Problems"))

  arrange(metric, full) %>% roc_auc(truth=factor(quality), estimate=full)
  arrange(metric, cropped) %>% roc_auc(truth=factor(quality), estimate=cropped)


  rocy <- roc_curve(metric, truth=factor(quality), Class1=full) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(aes(colour="Full")) +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw() +
    geom_path(aes(colour="Cropped"),
              data = roc_curve(metric, truth=factor(quality), Class1=cropped)) +
    ggtitle(feature) +
    scale_colour_manual("Scan",values=col_scans_dark)

  aucy <- data.frame(matrix(data=0, nrow = 1, ncol = 2, dimnames=list(c(), c("Full_AUC", "Cropped_AUC"))))
    aucy$Full_AUC <- roc_auc(data=metric, truth=factor(quality), Class1=full)$.estimate
    aucy$Cropped_AUC <- roc_auc(data=metric, truth=factor(quality), Class1=cropped)$.estimate

  return(list(scatterplot=scatty, boxplot=boxy, summary=summ, roc_curve=rocy, roc_auc = aucy))
}
