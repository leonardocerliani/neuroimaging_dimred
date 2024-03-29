
library(magrittr)
library(shiny)
library(dplyr)
library(tidyr)
library(tibble)
library(reactable)
library(plotly)
library(ggplot2)
library(heatmaply)
library(uwot)
library(Rtsne)
options(digits = 2)

set.seed(124)

N = 30

# A is here for "activity"
A <- function(mu) rnorm(N, mean = mu)

df <- tibble(
  task = c(rep("AAA",N), rep("BBB",N), rep("CCC",N)) %>% as.factor(),
  BA44  = c(A(2), A(2), A(5)),
  BA45 = c(A(2), A(2), A(5)),
  BA46  = c(A(2), A(3), A(5)),
  V1  = c(A(2), A(2), A(2)),
  V4  = c(A(2), A(2), A(2)),
  V5  = c(A(2), A(2), A(2)),
  SI   = c(A(5), A(2), A(2)),
  SPL  = c(A(5), A(2), A(2)),
  IPL  = c(A(5), A(3), A(2))
) %>% 
  group_by(task) %>%    # assign participant numbers
  # mutate(sub = paste0("sub_",row_number())) %>%
  mutate(sub = paste0("sub_",row_number(),"_",task)) %>%
  relocate(sub) %>%
  ungroup()

# image(df %>% select(contains("GM")) %>% as.matrix())


# df_mean is task-by-JU
df_mean <- df %>% 
  group_by(task) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = T)),
    .groups = "drop"
  )


# t_df_mean is JU-by-task
t_df_mean <- df_mean %>% 
  pivot_longer(
    cols = !task, names_to = "JU", values_to = "Zmean"
  ) %>% 
  pivot_wider(
    names_from = task, values_from = Zmean
  )



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # reactableOutput('summary_table'),
  # plotlyOutput("spiderplot"),
  # plotlyOutput("dimred"),
  # plotlyOutput("lasso_heatmap")
  
  sidebarLayout(
    sidebarPanel(

      h5("Mean Z per brain region in each task"),
      tags$p("Choose which regions to use for dimensionality reduction"),
      reactableOutput('summary_table'),
      HTML("<br><br>"),
      
      radioButtons(
        "dimred_method", "Choose dimensionality reduction method",
        c("MDS" = "mds", "UMAP" = "umap", "tSNE" = "tsne"),
        inline = T
      ),
      
      HTML("<br>"), h5("Low-dimensional embedding"),
      tags$p("Click & drag with the mouse to select observations"),
      plotlyOutput("dimred"),
      
      width = 6
    ),
    mainPanel(
      HTML("<br>"),
      h5("Mean Z per brain region in each task"),
      HTML("<br>"),
      plotlyOutput("spiderplot"),
      
      HTML("<br><br><br>"),
      h5("Embedding selection"),
      HTML("<br><br>"),
      plotlyOutput("lasso_heatmap"),
      width = 6
    )
  )
  
)

# Define server logic
server <- function(input, output) {
  
  # print summary table
  output$summary_table <- renderReactable({
    do_table(t_df_mean)
  })
  
  
  # spiderplot
  output$spiderplot <- renderPlotly({
    
    idx_JUs <- getReactableState("summary_table", "selected")
    
    act_ifelse(
      is.null(idx_JUs),
      draw_spiderplot(t_df_mean),
      draw_spiderplot(t_df_mean[idx_JUs,])
    )
  })
  
  
  # low-dimensional embedding
  output$dimred <- renderPlotly({
    
    idx_JUs <- getReactableState("summary_table", "selected")
    JU_names <- df %>% select(!task) %>% colnames()
    
    if (is.null(idx_JUs)) {
      do_dimred(df, input$dimred_method)
    } else {
      df_selection <- df %>% select(c(task,JU_names[idx_JUs]))
      do_dimred(df_selection, input$dimred_method)
    }
    
  })
  
  
  # Lasso on lowdim embedding to show heatmap of selected points
  output$lasso_heatmap <- renderPlotly({
    
    lasso_selected <- event_data("plotly_selected", source = "A")
    # print(lasso_selected)
    
    req(lasso_selected)
    
    
    df_lasso_selected <- df %>%
      filter(sub %in% lasso_selected$customdata) %>% 
      arrange(task) %>%
      # mutate(sub = paste0("sub_",lasso_selected$customdata)) %>%
      column_to_rownames(var = "sub") %>%
      heatmaply(Colv = NA, Rowv = NA, scale="none", colorbar_xanchor = "left") %>% 
      config(displayModeBar = F) %>% 
      layout(font = list(family = "arial narrow"))
    
  })
  
  
}



# ------------------------- Auxiliary functions -------------------------------

# Do dimensionality reduction (MDS, tSNE, UMAP)
do_dimred <- function(df, method) {
  
  # nsub <- length(df$task)
  vals <- df %>% select(!task) %>% as.matrix()
  
  D = dist(vals) %>% suppressWarnings()
  
  switch(method,
         mds = {
           mds <- cmdscale(D, eig = T, k = 2)
           pcs <- list(mds$points[,1], mds$points[,2])
         },
         umap = {
           u <- umap(D)
           pcs <- list(u[,1], u[,2])
         },
         tsne = {
           tsne <- Rtsne(D, dims = 2, perplexity = 10)
           pcs <- list(tsne$Y[,1], tsne$Y[,2])
         }
  )    
  
  df_lowdim <- tibble(
    task = df$task,
    sub = df$sub,
    pc1 = pcs[[1]],
    pc2 = pcs[[2]],
  )
  
  plot_ly(type = 'scatter', mode = 'markers', source = "A") %>%
    add_trace(
      data = df_lowdim,
      x = ~pc1,
      y = ~pc2,
      customdata = ~sub,
      color = ~task,
      text = ~sub,
      hoverinfo = 'text'
      # hovertemplate = paste('sub %{text}')
    ) %>% 
    layout(dragmode = "lasso") %>% 
    config(displayModeBar = F)
}


# Draw spiderplot
draw_spiderplot <- function(t_df_mean) {
  
  p <- plot_ly(
    type = 'scatterpolar', mode = 'lines+markers', fill = 'toself', opacity = 0.5
  ) %>% config(displayModeBar = F)
  
  df_vals <- t_df_mean %>% select(-JU)
  ticks <-  t_df_mean$JU %>% as.character()
  groups <- df_vals %>% colnames()
  
  for (ith_col in 1:length(groups)) {
    
    onecol <- df_vals[,ith_col] %>% pull()
    
    p <- p %>%
      add_trace(
        r = c(onecol, onecol[1]),
        theta = c(ticks, ticks[1]),
        name = groups[ith_col],
        line = list(
          dash = "solid",
          shape = "spline",
          smoothing = 1,
          width = 2
        )
      ) %>% layout(font = list(family = "arial narrow"))
  }
  
  return(p)
}


# Render initial summary table
do_table <- function(t_df_mean) {
  
  BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
  
  reactable(
    t_df_mean, 
    resizable = T,
    selection = "multiple", 
    onClick = "select",
    defaultColDef = colDef(
      style = function(value) {
        vals <- t_df_mean %>% select(where(is.numeric))
        if (!is.numeric(value)) return()
        normalized <- (value - min(vals)) / (max(vals) - min(vals))
        color <- BuYlRd(normalized)
        list(background = color)
      },
      format = colFormat(digits = 2), minWidth = 50
    ),
    style = list(fontFamily = "Arial narrow", fontSize = "13px")
  )    
}



# A small wrapper around if..then..else to make it similar to ifelse
act_ifelse <- function(condition, do_iftrue, do_iffalse) {
  if (condition) {
    do_iftrue
  } else {
    do_iffalse
  }
}


# Run the application 
shinyApp(ui = ui, server = server)















