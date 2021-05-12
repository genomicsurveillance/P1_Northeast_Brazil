if (!(require("ggplot2"))){
  install.packages("ggplot2")
  library("ggplot2")
}
if (!(require("scales"))){
  install.packages("scales")
  library("scales")
}
if (!(require("wesanderson"))){
  install.packages("wesanderson")
  library("wesanderson")
}

element_textbox <- function(...) {
  el <- element_text(...)
  class(el) <- c("element_textbox", class(el))
  el
}

element_grob.element_textbox <- function(element, ...) {

  text_grob <- NextMethod()
  rect_style<- calc_element("strip.background", theme_bw())
  # rect_style$fill<- "grey22"; rect_style$colour<- "grey22"
  rect_style$fill<- "grey44"; rect_style$colour<- "black"
  rect_grob <- element_grob(rect_style)

  ggplot2:::absoluteGrob(
    grid::gList(
      rect_grob,
      text_grob
    ),
    height = grid::grobHeight(text_grob),
    width = grid::unit(1, "npc")
  )
}

ggplot_respose<- function(data, predictions, pred_range, color_shade, var_facet){
  p<- ggplot(data=predictions, aes(x = time_pso, y = fit)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, colour = "transparent", fill = color_shade) +
      geom_jitter(aes(x=time_pso, y =SARSCOV2), data=data, height = 0, inherit.aes = F, pch = "|", width= 0.2, size = 3, alpha = 0.5)+
      geom_line(col = "black", lineend = 'round') +
      facet_wrap(~ .data[[var_facet]]) +
      xlab("days since symptom onset") +
      ylab("p(positive)")+
      scale_x_continuous(expand = c(0.01,0.01), limits=range(pred_range))+
      scale_y_continuous(expand = c(0.01,0.01), limits=c(0,1))+
      gtheme
  return(p)
}

kit_key_names<-    c("Allplex? 2019-nCoV Assay", "Allplex",
                  "KIT BIOMOL OneStep/COVID-19", "BIOMOL",
                  "Protocolo CDC - Vírus Respiratórios", "CDC N1,2",
                  "Protocolo CDC/EUA: SARS-CoV2 (N1, N2 e N3)", "CDC N1,2,3",
                  "Protocolo Charité: SARS-CoV2 (E)", "Charité (E)",
                  "Protocolo Charité: SARS-CoV2 (E/P1)", "Charité (E/P1)",
                  "Protocolo Charité: SARS-CoV2 (E/RP)", "Charité (E/RP)",
                  "REA DETECCAO 2019 NCOV 500T SINTESE BIO 10006713", "500T IDT",
                  "TaqPath? COVID 19 CE IVD RT PCR Kit","TaqPath",
                  "VERI-Q PCR 316 nCoV-QS","VERI-Q")
kit_key_names<- matrix(kit_key_names, ncol=2, byrow=T)


race_key_names<-c("AMARELA", "Yellow",
                  "BRANCA", "White",
                  "INDíGENA", "Indigenous",
                  "PARDA", "Mixed",
                  "PRETA", "Black",
                  "NA", "Unknown")
race_key_names<- matrix(race_key_names, ncol=2, byrow=T)


colorsHM<- rep(c("darkblue",wes_palette("Zissou1"),"#C11D24","#C11D24","#C11D24","#C11D24"),each=1)

gglinewidth<- 0.85

ggstyle<- theme_linedraw() +
          theme(axis.text.x = element_text(size = 10, color="#555555", face = "plain", family="Helvetica"),
            axis.text.y = element_text(size = 10, color="#555555", face = "plain", family="Helvetica"),
            axis.title.x = element_text(size = 12, color="#555555", face = "italic", family="Helvetica"),
            axis.title.y = element_text(size = 12, color="#555555", face = "italic", family="Helvetica"),
            panel.grid.major = element_line(color="#555555",),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "grey99"),
            legend.background = element_rect(fill = "grey99"),
            legend.key.size = unit(0.5, "cm") ,
            legend.text= element_text(size=11, family="Helvetica"),
            legend.margin=margin(0,1,0,1),
            legend.box.margin=margin(-5,-5,-5,-5),
            plot.title=element_textbox(hjust=0.5,size=12,face= 2,margin=margin(t=5,b=5),color='white')
          )

ggstylex<- #theme_linedraw() +
          theme(axis.text.x = element_text(size = 10, color="#555555", face = "plain", family="Helvetica"),
            axis.text.y = element_text(size = 10, color="#555555", face = "plain", family="Helvetica"),
            axis.title.x = element_text(size = 10, color="#555555", face = "italic", family="Helvetica"),
            axis.title.y = element_text(size = 10, color="#555555", face = "italic", family="Helvetica"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill =NA),
            legend.background = element_rect(fill =NA),
            # legend.key.height = unit(0, 'cm'), #changes area around symbols
            # legend.key.width = unit(0, 'cm'), #changes area around symbols
            legend.text= element_text(size=10, family="Helvetica",color="#555555"),
            axis.line = element_line(color="#555555"),
            plot.title=element_text(size=9,face=2,color="#555555")
          )

#wes_palette("Zissou1")
plot_hm<- function(data, xvar, yvar, colors=wes_palette("Zissou1"), predlabel="???", use_pred_lim=F, pred_lim=c(0,1)){
    p<- ggplot(data=data) + ggstylex +
        geom_tile(aes(x=get(xvar), y=get(yvar), fill=pred, col=pred)) +
        geom_contour(aes(x=get(xvar), y=get(yvar), z=pred), color='white', alpha=0.2)
    if(use_pred_lim) p<- p + scale_fill_gradientn(predlabel, colors=colors, limits=pred_lim) + scale_color_gradientn(predlabel, colors=colors, limits=pred_lim)
    else p<- p + scale_fill_gradientn(colors=colors) + scale_color_gradientn(colors=colors)
    return(p)
}

# plot_hm_date_age<- function(data, colors=wes_palette("Zissou1"), use_pred_lim=F, pred_lim=c(0,1)){
#     p<- ggplot(data=data) + ggstyle+
#         geom_tile(aes(x=date, y=age, fill=pred)) +
#         geom_contour(aes(x=date, y=age, z=pred))
#     if(use_pred_lim) p<- p + scale_fill_gradientn(colors=colors, limits=pred_lim)
#     else p<- p + scale_fill_gradientn(colors=colors)
#     return(p)
# }
