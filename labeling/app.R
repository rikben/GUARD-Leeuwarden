library(shiny)
library(dplyr)
library(readr)
library(DT)

year <- "2020"

parcel_csv <- paste0("../downloading/metadata/parcel_metadata",year,".csv")
image_csv  <- paste0("../downloading/metadata/image_metadata",year,".csv")
image_root <- paste0("../downloading/images_",year)

addResourcePath("plot_images", normalizePath(image_root))

label_choices <- c(
  "1 - green" = "green",
  "2 - slightly yellow" = "slightly_yellow",
  "3 - yellow" = "yellow",
  "4 - ploughed" = "ploughed",
  "5 - sparse vegetation (no glyphosate)" = "sparse_vegetation_no_glyphosate",
  "discard / no data" = "no_data"
)

parcel_meta <- read_csv(parcel_csv, show_col_types = FALSE)
image_meta  <- read_csv(image_csv, show_col_types = FALSE)

parcel_meta <- parcel_meta |>
  mutate(
    discarded = case_when(
      is.na(discarded) ~ FALSE,
      discarded %in% c(TRUE, "TRUE", "true", "yes", "Yes", "1") ~ TRUE,
      TRUE ~ FALSE
    )
  )

image_meta <- image_meta |>
  mutate(
    discarded = case_when(
      is.na(discarded) ~ FALSE,
      discarded %in% c(TRUE, "TRUE", "true", "yes", "Yes", "1") ~ TRUE,
      TRUE ~ FALSE
    )
  )

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scroll_top', function(message) {
        window.scrollTo({ top: 0, behavior: 'smooth' });
      });
    "))
  ),
  
  titlePanel("Image labelling tool"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("parcel_id", "Select plot", choices = parcel_meta$parcel_id),
      hr(),
      uiOutput("plot_info"),
      actionButton("discard_plot", "Discard plot"),
      actionButton("save", "Save progress"),
      hr(),
      actionButton("previous_plot", "Previous plot"),
      actionButton("next_plot", "Save and next plot")
    ),
    
    mainPanel(
      uiOutput("image_cards"),
      hr(),
      actionButton("next_plot_bottom", "Save and next plot")
    )
  )
)

server <- function(input, output, session) {
  
  parcels <- reactiveVal(parcel_meta)
  images <- reactiveVal(image_meta)
  
  selected_images <- reactive({
    images() |>
      filter(parcel_id == as.numeric(input$parcel_id)) |>
      arrange(image_date)
  })
  
  selected_parcel <- reactive({
    parcels() |>
      filter(parcel_id == as.numeric(input$parcel_id))
  })
  
  output$plot_info <- renderUI({
    parcel <- selected_parcel()
    imgs <- selected_images()
    
    n_images <- nrow(imgs)
    n_labelled <- sum(!is.na(imgs$class_label) & imgs$class_label != "")
    n_discarded <- sum(imgs$discarded, na.rm = TRUE)
    
    labelled_status <- ifelse(
      n_labelled == n_images,
      "COMPLETE",
      paste0("INCOMPLETE: ", n_labelled, " / ", n_images, " images labelled")
    )
    
    tagList(
      tags$h4("Plot information"),
      tags$p(strong("Parcel ID: "), parcel$parcel_id),
      tags$p(strong("BRP ID: "), parcel$brp_id),
      tags$p(strong("Folder: "), parcel$folder_name),
      tags$p(strong("Glyphosate: "), parcel$glyphosate),
      tags$p(strong("Plot discarded: "), parcel$discarded),
      tags$p(strong("Labelling status: "), labelled_status),
      tags$p(strong("Discarded images: "), paste0(n_discarded, " / ", n_images))
    )
  })
  
  output$image_cards <- renderUI({
    imgs <- selected_images()
    plot_folder <- selected_parcel()$folder_name[1]
    
    tagList(lapply(seq_len(nrow(imgs)), function(i) {
      row <- imgs[i, ]
      
      png_file <- gsub("\\.tif$", ".png", basename(row$file_path))
      img_src <- file.path("plot_images", plot_folder, png_file)
      
      default_label <- ifelse(
        is.na(row$class_label) || row$class_label == "",
        row$suggested_class_label,
        row$class_label
      )
      
      wellPanel(
        fluidRow(
          column(
            4,
            tags$img(
              src = img_src,
              style = "max-width:100%; border:1px solid #ccc;"
            )
          ),
          column(
            8,
            h4(row$image_id),
            p(paste("Date:", row$image_date)),
            p(paste("NDVI mean:", round(row$ndvi_mean, 3))),
            p(paste("NDVI SD:", round(row$ndvi_sd, 3))),
            p(paste("Suggested:", row$suggested_class_label)),
            p(paste("Discarded:", row$discarded)),
            
            selectInput(
              paste0("label_", row$image_id),
              "Label",
              choices = label_choices,
              selected = default_label
            ),
            
            actionButton(
              paste0("discard_", row$image_id),
              "Discard image"
            )
          )
        )
      )
    }))
  })
  
  observe({
    imgs <- images()
    
    for (id in imgs$image_id) {
      local({
        image_id <- id
        
        observeEvent(input[[paste0("discard_", image_id)]], {
          x <- images()
          x$discarded[x$image_id == image_id] <- TRUE
          images(x)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$next_plot, {
    save_current_plot()
    
    parcel_ids <- parcels()$parcel_id
    current_index <- which(parcel_ids == as.numeric(input$parcel_id))
    
    if (current_index < length(parcel_ids)) {
      updateSelectInput(
        session,
        "parcel_id",
        selected = parcel_ids[current_index + 1]
      )
    }
    
    showNotification("Saved. Moved to next plot.", type = "message")
  })
  
  observeEvent(input$next_plot_bottom, {
    save_current_plot()
    
    parcel_ids <- parcels()$parcel_id
    current_index <- which(parcel_ids == as.numeric(input$parcel_id))
    
    if (current_index < length(parcel_ids)) {
      updateSelectInput(
        session,
        "parcel_id",
        selected = parcel_ids[current_index + 1]
      )
    }
    
    showNotification("Saved. Moved to next plot.", type = "message")
    
    session$sendCustomMessage("scroll_top", list())
  })
  
  observeEvent(input$previous_plot, {
    save_current_plot()
    
    parcel_ids <- parcels()$parcel_id
    current_index <- which(parcel_ids == as.numeric(input$parcel_id))
    
    if (current_index > 1) {
      updateSelectInput(
        session,
        "parcel_id",
        selected = parcel_ids[current_index - 1]
      )
    }
    
    showNotification("Saved. Moved to previous plot.", type = "message")
  })
  
  observeEvent(input$discard_plot, {
    x <- parcels()
    x$discarded[x$parcel_id == as.numeric(input$parcel_id)] <- TRUE
    parcels(x)
  })
  
  observeEvent(input$save, {
    save_current_plot()
    showNotification("Progress saved.", type = "message")
  })
  
  save_current_plot <- function() {
    x <- images()
    
    for (id in selected_images()$image_id) {
      input_id <- paste0("label_", id)
      
      if (!is.null(input[[input_id]])) {
        x$class_label[x$image_id == id] <- input[[input_id]]
      }
    }
    
    images(x)
    
    write_csv(parcels(), parcel_csv)
    write_csv(images(), image_csv)
  }
}

shinyApp(ui, server)