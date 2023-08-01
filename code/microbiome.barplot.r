# Microbiome.Barplot function modified from MicrobeR

Microbiome.Barplot2<-function (FEATURES, METADATA, NTOPLOT, CATEGORY) 
{
    if (missing(NTOPLOT) & nrow(FEATURES) > 19) {
        NTOPLOT = 19
    }
    else if (missing(NTOPLOT)) {
        NTOPLOT = nrow(FEATURES)
    }
    FEATURES <- Make.Percent(FEATURES)
    FEATURES <- FEATURES[order(rowMeans(FEATURES), decreasing = T), 
        ]
    if (NTOPLOT < nrow(FEATURES)) {
        Remainder <- colSums(FEATURES[(NTOPLOT + 1):nrow(FEATURES), 
            ])
        FEATURES <- rbind(FEATURES[1:NTOPLOT, ], Remainder)
    }
    forplot <- TidyConvert.ToTibble(FEATURES, "Taxa") %>% gather(-Taxa, 
        key = "SampleID", value = "Abundance")
    forplot$Taxa <- factor(forplot$Taxa, levels = rev(unique(forplot$Taxa)))
    if (!missing(METADATA) & !missing(CATEGORY)) {
        if (TidyConvert.WhatAmI(METADATA) == "data.frame" | TidyConvert.WhatAmI(METADATA) == 
            "matrix") {
            METADATA <- TidyConvert.ToTibble(METADATA, "SampleID")
        }
        forplot <- left_join(forplot, METADATA, by = "SampleID")
    }
    PLOT <- (ggplot(forplot, aes(x = SampleID, y = Abundance, 
        fill = Taxa)) + geom_bar(stat = "identity",color="black") + theme_classic() + 
        ylab("% Abundance") + xlab("") + theme(axis.text.x = element_text(angle = 45, 
        hjust = 1, size = 10)) + theme(legend.text = element_text(size = 10)))
    if (NTOPLOT <= 19) {
        COLORS <- rev(c("blue4", "olivedrab", "firebrick", "gold", 
            "darkorchid", "steelblue2", "chartreuse1", "aquamarine", 
            "yellow3", "coral", "grey", "purple","coral3","deeppink",
            "chartreuse3","deepskyblue","yellow","pink","green","blue","orange"))
        PLOT <- (PLOT + scale_fill_manual(values = COLORS, name = " "))
    }else if (NTOPLOT > 19 & NTOPLOT <= length(expand_colors)) {
      COLORS <- expand_colors
      PLOT <- (PLOT + scale_fill_manual(values = COLORS, name = " "))
    }
    # }else if (NTOPLOT > 10 & NTOPLOT <= length(categorical_colors)) {
    #   COLORS <- categorical_colors
    #   PLOT <- (PLOT + scale_fill_manual(values = COLORS, name = " "))
    # }
    if (!missing(CATEGORY)) {
        PLOT <- (PLOT + facet_grid(~get(CATEGORY), margins = FALSE, 
            drop = TRUE, scales = "free", space = "free"))
    }
    return(PLOT)
}

expand_colors<-rev(c("blue4", "olivedrab", "firebrick", "gold", 
    "darkorchid", "steelblue2", "chartreuse1", "aquamarine", 
    "yellow3", "coral", "#3a6ce3", "#6dd5db", "#5d8fd5", "#783fcf", "#b89ad8", "grey"))

categorical_colors <- c(
  "purple",
  "coral3",
  "deeppink",
  "chartreuse3",
  "deepskyblue",
  "yellow",
  "pink",
  "green",
  "orange",
  "#b4426b",
  "#d395a5",
  "#de424e",
  "#973a35",
  "#855d55",
  "#e08f71",
  "#ded43e",
  "#63684b",
  "#cde18a",
  "#b4e445",
  "#74a737",
  "#c8d6bd",
  "#66db4a",
  "#6cdbaf",
  "#5b9c86",
  "#3b6761",
  "#bec3dc",
  "#59559b",
  "#e4408f",
  "#d04ad1",
  "#943d93",
  "#b89ad8",
  "#783fcf",
  "#5d8fd5",
  "#6dd5db",
  "#3a6ce3")