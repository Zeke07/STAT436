library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(gganimate)
library(gifski) 
library(png)     
library(shiny)
library(bslib)
library(DT)



passwords <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-01-14/passwords.csv') 

# normalize crack time into seconds (on logarithmic scale since the range of values is enormous), drop NA values
passwords <- passwords %>% 
  drop_na() %>%
  mutate(LOG_CRACK_TIME_SECS = log(case_when(
    time_unit == "years" ~ 60 * 60 * 24 * 365 * value,
    time_unit == "minutes" ~ 60 * value,
    time_unit == "days" ~ 60 * 60 * 24 * value,
    time_unit == "months" ~ 60 * 60 * 24 * 30.43 * value, # average # days in a month,
    time_unit == "weeks" ~ 60 * 60 * 24 * 7 * value,
    time_unit == "hours" ~ 60 * 60 * value,
    .default = value # unit is alr seconds
  ) + 1)
  )

def_passwords <- c("1234", "qwerty") # basic default selected passwords

# expand the full dataframe for animation
time_lapse <- passwords %>% 
  group_by(password, category, value, time_unit, LOG_CRACK_TIME_SECS) %>% 
  reframe(time=seq(0, LOG_CRACK_TIME_SECS, 1))


# filtered data for time-lapse plot based on password
filtered_tl_df <- function(df, passwords) {
  df %>% 
    filter(password %in% passwords) %>% 
    mutate(selected = password %in% passwords)
  
}

# faceted time-lapse plot
# output as .gif to be rendered by shiny
time_lapse_plt <- function(df) {
  p <- ggplot(df) +
    geom_col(aes(time, password, fill=category),
             width = 1,
             color="black") +
    facet_grid(category ~ ., scale = "free_y") +
    coord_cartesian(expand = FALSE) + 
    scale_fill_brewer(palette="Pastel1") +
    transition_reveal(time) + 
    labs(x = "Log(Online Crack Time + 1) (s)", 
         y = "Password", 
         fill = "Category") +
    theme_dark() + 
    theme(strip.text = element_blank())
  
  outfile <- tempfile(fileext = ".gif")
  plt_gif <- animate(p, 
                     renderer = gifski_renderer(loop = FALSE),
                     height = 800, 
                     width = 900,
                     res = 200, 
                     fps = 60)
  anim_save(outfile, animation = plt_gif)
  
  list(src = outfile, contentType = "image/gif")

}

# --- shiny app implementation

# --UI
ui <- page_navbar(
  title = "Password Strength Dashboard",
  layout_columns(
  card(
    card_header("Password Online Crack-Time Time-Lapse"), # main time-lapse plot
    imageOutput("time_plt_gif")
  ),
  card(
    card_header("Selected Passwords Data Table", 
                   popover(bsicons::bs_icon("gear", class = "ms-auto"), # plot settings
                        selectInput("passwords", "Select Passwords:", 
                                       choices = passwords$password, 
                                       multiple = TRUE,
                                       selected = def_passwords),
                        actionButton("replay", "Render"))),
       dataTableOutput("table")), # data table
  col_widths = c(8, 4) 
  )
)

# --server
# visual elements are updated on clicking of 'Render' button, filtered
# based on selected passwords
server <- function(input, output) {
  
  # make this a reactive input parameter for filtering dataframe
  # this will update on an action event
  selected_passwords <- reactiveVal(NULL)
  
  # dataframe for animated time-lapse plot
  time_lapse_df <- reactive({
    req(selected_passwords())  # ensure a selection is made before proceeding
    filtered_tl_df(time_lapse, selected_passwords()) 
  })
  
  # filter the display table when 'Render' is pressed
  pass_df <- eventReactive(input$replay, {
    passwords %>% 
      filter(password %in% input$passwords) %>% 
      rename(online_crack_time = value) %>% 
      select(strength, everything(), -rank_alt, -font_size) %>% 
      arrange(desc(strength))
  })
  
  # reactive .gif (image render), updates on reactive data element dependency
  gif <- reactive({
    time_lapse_plt(time_lapse_df())
  })
  
  
  # change reactive val (used to filter data), render new plot 
  # on click of 'Render' button
  observeEvent(input$replay, {
    selected_passwords(input$passwords) 
    output$time_plt_gif <- renderImage({
      gif()
    }, deleteFile = FALSE)
  })
  
  # render data table, depends on a dataframe that gets updated with click event
  output$table <- renderDT(datatable(
    pass_df(),
    filter = "top",
    options = list(pageLength = 10)
  ))
}


# deploy shiny app
shinyApp(ui, server)


