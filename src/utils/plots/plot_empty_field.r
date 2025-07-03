library(ggplot2)

plot_empty_field <- function() {
    h <- 80
    w <- 120
    green <- "#39782e"

    field_lines <- data.frame(
        x_start = w/2,
        y_start = 0,
        x_end   = w/2,
        y_end   = h
    )
    
    p <- ggplot()+
        # limites do campo
        annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = green, color="white", linewidth = 1)+

        # linha de meio de campo
        geom_segment(data=field_lines, aes(x=x_start, y = y_start, xend = x_end, yend = y_end), color = "white", linewidth = 1) + 

        # Arco da área esquerda
        annotate("path", 
                 x = 12 + 10 * cos(seq(0, 2*pi, length.out = 100)), 
                 y = 40 + 10 * sin(seq(0, 2*pi, length.out = 100)), 
                 color = "white", linewidth = 1) +
        # Arco da área esquerda
        annotate("path", 
                 x = 108 + 10 * cos(seq(0, 2*pi, length.out = 100)), 
                 y = 40 + 10 * sin(seq(0, 2*pi, length.out = 100)), 
                 color = "white", linewidth = 1) +


        # Grande área esquerda
        annotate("rect", xmin=0, ymin=18, xmax=18, ymax=62, fill = green, color = "white", linewidth = 1) + 
        # Grande área direita
        annotate("rect", xmin=102, ymin=18, xmax=120, ymax=62,, fill = green, color = "white", linewidth = 1) +

        # Pequena área esquerda
        annotate("rect", xmin=0, ymin=30, xmax=6, ymax=50, fill = green, color = "white", linewidth = 1) + 
        # Pequena área direita
        annotate("rect", xmin=114, ymin=30, xmax=120, ymax=50, fill = green, color = "white", linewidth = 1)+

        # Gol esquerdo
        annotate("rect", xmin=-2, ymin=36, xmax=0, ymax=44, fill = green, color = "white", linewidth = 1) +
        # Gol direito
        annotate("rect", xmin=120, ymin=36, xmax=122, ymax=44, fill = green, color = "white", linewidth = 1) +

        # Marca do penalti esquerdo
        annotate("point", x=12, y = 40, color = "white", size = 2) +
        # Marca do penalti direito
        annotate("point", x=108, y = 40, color = "white", size = 2) +

        # Ponto central
        annotate("point", x=60, y=40, color = "white", size = 2) +
        # Círculo central
        annotate("path", 
                 x = 60 + 10 * cos(seq(0, 2*pi, length.out = 100)), 
                 y = 40 + 10 * sin(seq(0, 2*pi, length.out = 100)), 
                 color = "white", linewidth = 1) +

        # Escanteio inferior esquerdo
        annotate("path", 
                 x = 0 + 2 * cos(seq(0, pi/2, length.out = 50)), 
                 y = 0 + 2 * sin(seq(0, pi/2, length.out = 50)), 
                 color = "white", linewidth = 0.5) +

        # Escanteio inferior direito
        annotate("path", 
                 x = 120 + 2 * cos(seq(pi/2, pi, length.out = 50)), 
                 y = 0 + 2 * sin(seq(pi/2, pi, length.out = 50)), 
                 color = "white", linewidth = 0.5) +

        # Escanteio superior direito
        annotate("path", 
                 x = 120 + 2 * cos(seq(pi, 3/2*pi, length.out = 50)), 
                 y = 80 + 2 * sin(seq(pi, 3/2*pi, length.out = 50)), 
                 color = "white", linewidth = 0.5) +

        # Escanteio superior esquerdo
        annotate("path", 
                 x = 0 + 2 * cos(seq(3/2*pi, 2*pi, length.out = 50)), 
                 y = 80 + 2 * sin(seq(3/2*pi, 2*pi, length.out = 50)), 
                 color = "white", linewidth = 0.5) +

        theme_void() + # Remove eixos e plano de fundo padrão
        coord_fixed(ratio = 1) + # Mantém a proporção do campo
        theme(plot.background = element_rect(fill = "#969696", color = NA)) # Cor de fundo do plot
    
    return(p)
}