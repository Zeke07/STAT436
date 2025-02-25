library(tidyverse)
library(ggplot2)
library(shiny)
library(bslib)
library(DT)

# NOTE: pwd -> /Users/zeke/Desktop/Classes/STAT436/homework/hw2
# Dataset: https://www.kaggle.com/datasets/asaniczka/amazon-products-dataset-2023-1-4m-products?select=amazon_categories.csv

# preliminary - load dataset
amazon_categories <- read_csv("https://drive.usercontent.google.com/download?id=1hLBIa3eDT6MKOIlniP7pwVykK2T3SBHL&export=download&authuser=1&confirm=t")
amazon_products <- read_csv("https://drive.usercontent.google.com/download?id=1lE3X88LLSgh3xSZsZ-5YEGx88xUbjjPN&export=download&authuser=1&confirm=t")

# join the two tables to define the product category name 
# mutate, mutate to add product score, sale status, hyperlinks, etc
amazon_full <- amazon_products %>%
  inner_join(amazon_categories, by = c("category_id" = "id")) %>% 
  mutate(listPrice = case_when(listPrice == 0 ~ price,
                               .default = listPrice), 
         sale = listPrice != price, productURL = paste0('<a href="#" onclick="window.open(\'', productURL, '\', \'_blank\');">', productURL, '</a>'),
         reviews_stars_prod = (stars * reviews) + stars + 1)


# data filtering function helper
markdown_data <- function(df, cat, price_range, best_seller, search_keyword, discount=TRUE){
  
  # filter table based on plot settings
  df <- df %>%
    filter(category_name %in% cat, price >= price_range[1], price <= price_range[2], isBestSeller == best_seller)
  
  
  # apply search filter if input is not empty
  if (nzchar(search_keyword)) {
    keyword <- tolower(search_keyword)  # case-insensitive search
    df <- df[apply(df, 1, function(row) any(grepl(keyword, tolower(row), fixed = TRUE))), ]
  }
  
  # filter to only show discounted items
  if (discount==TRUE){
    df <- df %>% filter(sale == discount)
  }
  
  return (df)
  
  
}


# core scatterplot 
markdown_plt_v2 <- function(df){
  
  # the breaks and labels for the point sizes need to 
  # be dynamically updated since the range of possible values in `boughtInLastMonth` will vary
  # on the queried table
  spectrum = unique(df$boughtInLastMonth)
  brk = sort(sample(spectrum, min(length(spectrum), 5), replace=FALSE), decreasing=FALSE)
  lbls = as.character(brk)
  rng = c(1, length(brk), 1) + 2
  
  ggplot(df) +
    
    # two points for each observation, one being current price, other is original price
    geom_point(aes(x = reviews_stars_prod, y = price, color="Current Price", size=boughtInLastMonth), alpha=0.5) +
    geom_point(aes(x = reviews_stars_prod, y = listPrice, color="Original Price", size=boughtInLastMonth), alpha=0.5) +
    
    # segment connecting current and original price to visualize markdown
    geom_segment(aes(x = reviews_stars_prod, xend = reviews_stars_prod, y = price, yend = listPrice, color = "Markdown"), 
                 linetype = "dotted") +
    
    # facet over product categories (if one or more selected)
    facet_wrap(~category_name, ncol=3) + 
    
    # custom legend
    scale_color_manual(
      name = 'Pricing',
      breaks = c('Current Price', 'Original Price', 'Markdown'),
      values = c('Current Price'="red", 'Original Price'='black', "Markdown"="purple")) +
    
    # establish point size based on # items bought in last month
    scale_size_continuous(
      name = "# Bought In Last Month",
      range = c(min(rng), max(rng)),
      breaks = brk,
      labels = lbls) +
    
    # scale axes since the range of possible values is high
    scale_x_log10() +
    theme_bw() + 
    labs(title = "Amazon Product Price to Success Comparison by Category",
         subtitle = "[Hover over red points for more info]", 
         x = "Product Score [# Reviews x Rating + Rating + 1]",
         y = "Price") +
    
    # flip the plot coordinate mappings
    # NOTE: this was for simplicity, since I wasn't sure which orientation was better,
    # and didn't want to manually re-write the code every time
    coord_flip()
  
  
}


# function definition of plot settings object for brevity
plot_settings <- function(){
  popover(
    bsicons::bs_icon("gear", class = "ms-auto"),
    selectInput("categories", "Product Category:",
                amazon_categories$category_name, 
                multiple = TRUE, 
                selected = "Computers"),
    textInput("plot_search", "Search: "),
    sliderInput("prange", "Price (Low-High):",
                min=min(amazon_full$price), max=max(amazon_full$price),
                value=c(min(amazon_full$price), 50),
                ticks=FALSE, step = 5,
                pre = "$", sep = ","),
    checkboxInput("best_seller", "Best Sellers?"),
    checkboxInput("sale", "Discount Only (Recommended!)", value=TRUE),
    title = "Plot Settings",
  )
}


# shiny app [function definitions]

# [implementation]
# UI definition with retractable sidebar panel and navigation tabs -
ui <- page_sidebar(
  
  # app title ----
  title ="Amazon Products Analysis",
  
  sidebar = sidebar(
    uiOutput("product_image") # sidebar for product info/image
    
  ),
  
  
  # main panel for displaying outputs ----
  # Output: A tabset that combines two panels ----
  navset_card_underline(
    id = "active_tab",
    title = "Visualizations",
    
    # panel for main plot 
    nav_panel("Plot", 
              plotOutput("markdown_plot", hover = hoverOpts("plot_hover"), brush=brushOpts("plot_brush")), 
    ),
    
    # panel for data table render
    nav_panel("Table", DTOutput("table")), 
    nav_panel("Getting Started",  
              card(
                card_header("Welcome to my Amazon Products Shiny App! - Zayn"),
                card_body(
                  p("Follow these steps to explore Amazon product insights:"),
                  tags$ul(
                    tags$li("Click on the 'Plot' tab to view the interactive visualization."),
                    tags$li("Use the settings (gear icon) to search, filter by price, category, and other attributes."),
                    tags$li("If you are viewing only discounted items, hover on the [RED] points (not the [BLACK] ones) for pop-up information on the side bar (you can collapse the side bar by clicking the arrow on the top left). The plot is intended for items that are on sale, but if you choose to view all items via the settings (including those not on sale), the non-discounted ones will appear as a single blackish-red point that you can hover over."),
                    tags$li("Brushing (click & drag on the plot) updates the 'Plot' and 'Table' tab dynamically. Click the cursor away when it is a crosshair to de-select and revert to the previous state"),
                    tags$li("Switch to the 'Table' tab to view the filtered product data."),
                    tags$li("Click on product links in the table to visit their Amazon page.")),
                  p("Enjoy exploring the dataset!")))
    ),
    plot_settings()
  ),
  
  
  
  
)

# define server logic for amazon product app ----
server <- function(input, output, session) {
  
  # coalesce brush and filter into reactive func, so that the plot/tables will re-render either on a filter,
  # or a brush event.
  markdown_df <- reactive({
    
    # produce filtered dataframe based on user inputs to plot settings
    df <- markdown_data(amazon_full, input$categories, input$prange, input$best_seller, input$plot_search, input$sale)
    
    # only modify if a brush event has occurred, else just default to the filtered data
    # this works because the brush event, if it exists, will be a subset of the filtered data
    if (!is.null(input$plot_brush)) {
      temp <- brushedPoints(df, input$plot_brush)
      if (nrow(temp) != 0) df <- temp
    }
    
    return (df)
  })
  
  
  # plot render
  output$markdown_plot <- renderPlot(markdown_plt_v2(markdown_df()))
  
  # data table render
  output$table <- renderDT(
    datatable(
      markdown_df(),
      escape = FALSE,  # allow rendering of HTML for hyperlinks
      selection = "single", # a
      filter = "top",
      options = list(pageLength = 100)
    ))
  
  # reactive func for plot hover events
  # filter only the single RED point that the cursor is over
  hover_data <- reactive({
    nearPoints(markdown_df(), input$plot_hover, threshold = 10, maxpoints = 1)
  })
  
  
  # render formatted product information to the UI sidebar
  output$product_image <- renderUI({
    data <- hover_data()
    if (nrow(data) == 0) return(NULL)
    tagList(paste0(substr(data$title, 1, 120), ifelse(nchar(data$title) > 120, "...", "")),
            tags$img(src = data$imgUrl, height = "150px"),
            tags$div(
              HTML(paste(tags$b("Current Price: "), "$", data$price), "<br>"),
              HTML(paste(tags$b("From: "), "$", tags$s(style="color:red", data$listPrice), "<br>")),
              HTML(paste(tags$b("Rating (/5): "), tags$span(style="color:gold", tags$b(data$stars)), "<br>")),
              HTML(paste(tags$b("# of Reviews: "), data$reviews, "<br>")))
    )
    
  })
  
}


# run shiny app
shinyApp(ui, server)



