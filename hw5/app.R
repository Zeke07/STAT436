library(tidyverse)
library(keras3)
library(reticulate)
library(ggplot2)
library(shiny)
library(bslib)

# NOTE: this implementation is loading a pre-processed
# version of the PCA model along with a truncated version of the data (in R's compressed format!)
# data-loading/static variables

# load image df as csv
df <- readRDS("mnist_fashion_compressed.rds")  # or fashion-mnist_train.csv


classes <- c("T-shirt/top", "Trouser", "Pullover", "Dress", "Coat", 
             "Sandal", "Shirt", "Sneaker", "Bag", "Ankle boot")

# assign string labels based on numeric for label column
df <- df %>% 
  mutate(label = classes[label+1])

# omit label when computing PCA (numerics only)
images = df %>% 
  select(-label)
labels = df$label


# pca on image data
pca <- readRDS("fm_pca_model_slim.rds") # prcomp(images, center = TRUE, scale. = TRUE)



# re-attach labels after PCA
images$label = labels

# minimize margins!
par(mar = c(0, 0, 0, 0))

# calculate the proportion of variance explained
explained_variance <- pca$sdev^2 / sum(pca$sdev^2)

# plot df for explained var
variance_df <- data.frame(
  PC = 1:length(explained_variance),
  Variance = explained_variance,
  CumulativeVariance = cumsum(explained_variance)
)


# static optimal number of components to retain (e.g., enough to explain 95% variance)
opt_components <- which(cumsum(explained_variance) >= 0.95)[1]


# static plot settings tab
plot_settings <- function() {
  popover(bsicons::bs_icon("gear", class = "ms-auto"), 
          selectInput("classes", "Clothing Type: ", 
                      choices = classes, 
                      multiple = TRUE,
                      selected = classes),
          actionButton("prev", "<"),
          actionButton("nex", ">"))
}



# SHINY APP - UI
ui <- page_fillable(
  
  card(card_header("Optimal Image Re-construction Using PCA [MNIST Fashion]"),
       plotOutput("cumvar", click = "component_click"),
       plot_settings(), 
       height = 400), 
  layout_column_wrap(
    card(card_header("Original"), 
         plotOutput("original")),
    card(card_header(paste("Projected")),
         plotOutput("compressed")),
    card(card_header("Loss"),
         plotOutput("error"))
  )
  
)




# plot cumulative variance explained as we add more components
explained_variance_plt <- function(selected_pc) {
  
  ggplot(variance_df, aes(x = PC, y = CumulativeVariance)) +
    geom_line() +
    geom_vline(xintercept = opt_components, linetype = "dashed", color = "red") + 
    geom_vline(xintercept = selected_pc, linetype = "dashed", color = "purple") + 
    geom_point() +
    annotate(x = opt_components, y=0.95, label="Optimal", vjust=2, geom="label", color = "red") + 
    annotate(x = selected_pc, y=0.5, label="Selected", geom="label", color = "purple") + 
    labs(title = "Total Variance Explained by Principal Components",
         subtitle = "[CLICK along x-axis to re-project images with selected # of PCs, CLICK gear icon for more settings!]",
         x = "# Of Principal Components",
         y = "Total Variance Explained (%)") +
    theme_minimal()
  
}

# helper for generalized image plotting
img_plt <- function(matrix, colors) {
    image(
      1:28, 1:28,
      t(matrix)[, 28:1],
      col = colors,
      axes = FALSE, 
      main = NULL
    )
}

# SHINY APP - SERVER 
server <- function(input, output){
  
  # denotes the number of principal components to use in image projctio 
  # default value is number components with 95% explainability on the dataset  
  # but this value is modular per the input of the user to the brush plot element
  selected_pc <- reactiveVal(opt_components)
  
  
  # filter image based on the rolling index (user can scroll using arrows)
  # and the selected clothing types
  image_idx <- reactiveVal(1)
  num_imgs <- reactiveVal(nrow(images))
  selected_image <- reactiveVal(as.numeric((images %>% select(-label))[1, ]))
  
  # update index of image to display based on forward/back arrows in settings
  observeEvent(input$prev, {
    
    idx <- (image_idx() - 1) %% num_imgs()
    if (idx == 0) 
      idx = num_imgs()
    
    image_idx(idx)
  })
  
  observeEvent(input$nex, {
    
    idx <- (image_idx()+1) %% num_imgs()
    if (idx == 0) 
      idx = 1
    
    image_idx(idx)
  })
  
  
  # filter dataset based on any input parameters in the settings tab
  observeEvent(list(input$classes, input$prev, input$nex), {
    filtered_imgs <- images %>% 
      filter(label %in% input$classes)
    
    num_imgs(nrow(filtered_imgs))
    
    
    # set current image as selected index within the subset filtered by selected classes 
    selected_image(as.numeric((filtered_imgs %>% 
                                 select(-label))[image_idx(), ]))
    
  })
  
  
  # reactive matrix definitions for both original, projected images, and errors
  original_matrix <- reactive({
    original_image <- selected_image()
    return (matrix(original_image, nrow = 28, byrow = TRUE))
  })
  
  # projected image over the selected principal components
  reconstructed_matrix <- reactive({
    
    original_image <- selected_image()
    projected <- (original_image - pca$center) %*% pca$rotation[, 1:selected_pc()]
    
    # reconstruct image from the projected data
    reconstructed <- projected %*% t(pca$rotation[, 1:selected_pc()]) + pca$center
    
    # return reshaped data vector to be 28x28 image matrix
    return (matrix(reconstructed, nrow = 28, byrow = TRUE))
  })
  
  
  error_matrix <- reactive({
    return (original_matrix() - reconstructed_matrix())
  })
  
  # x-component of user click is the number of components on the x-axis
  observeEvent(input$component_click, {
    # clamp selected values in range of 0 -> max pcs
    pcs <- as.integer(pmax(0, pmin(input$component_click$x, length(explained_variance))))
    selected_pc(pcs)
  })
  
  
  # plot, gets modified based on user click
  output$cumvar <- renderPlot({
    explained_variance_plt(selected_pc())
  })
  
  
  
  # plot definitions for the three images we are comparing (original, compressed, and error between them)
  output$original <- renderPlot({
    img_plt(original_matrix(), gray.colors(256))
  })
  
  output$compressed <- renderPlot({
    img_plt(reconstructed_matrix(), gray.colors(256))
  })
  
  output$error <- renderPlot({
    img_plt(error_matrix(), terrain.colors(256))
  })
  
  
}


# APP LAUNCH
shinyApp(ui, server)