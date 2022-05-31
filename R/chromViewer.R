#' Shiny gadget for viewing chromatograms
#' @name chrom_viewer
#' @aliases chrom_viewer
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import shinydashboard
#' @import shinyWidgets
#' @import chromatographR
#' @import DT
#' @import ggplot2
#' @importFrom dplyr filter
#' @param peak_table A \code{peak_table} object.
#' @param chrom_list A list of chromatograms.
#' @return No return value.
#' @examplesIf interactive()
#' data(Sa_warp)
#' data(pk_tab)
#' chrom_viewer(pk_tab)
#' @author Ethan Bass
#' @export chrom_viewer
chrom_viewer <- function(peak_table, chrom_list){
  if (missing(chrom_list) & missing(peak_table))
    stop("Must provide either a peak_table or a chrom_list.")
  if (missing(chrom_list)){
    chrom_list <- try(get(peak_table$args["chrom_list"]))
    if (inherits(chrom_list, "try-error")) stop("Chromatograms not found!")
  }
  data <- tidy_chrom_converter(chrom_list)
  if (!missing(peak_table)){
    peak_table <- summarize_peak_info(peak_table)
    peak_sum <- t(peak_table$pk_meta[c("rt","sd","mean_area","median_area","sd_area"),])
    peak_tab <- t(rbind(peak_table$pk_meta["rt",], peak_table$tab))
  } else{
    peak_sum <- data.frame()}
  chrom_names <- unique(data$chr)
  rts <- filter(data, chr == unique(data$chr)[1] & lambda == data$lambda[1]) %>%
    .[["rt"]] %>% as.numeric

  header <- dashboardHeader(title = "chromViewer")

  sidebar <- dashboardSidebar(
    sidebarMenu(
      # menuItem("Load data", tabName = "data", icon = icon("database")),
      menuItem("trace", tabName = "trace", icon = icon("chart-line")),

      ### dropdown menus
      selectizeInput(
        'select_lambdas', label = "Select wavelength",
        choices = unique(data$lambda),
        options = list(create = TRUE),
        multiple = TRUE),

      selectizeInput("select_chroms", "Select chromatograms",
                     choices = chrom_names,
                     multiple = TRUE),

      uiOutput("lambda_controls"),
      uiOutput("chrom_controls"),
      actionButton("save_spectrum", "Save spectrum"),
      actionButton("save_both", "Save screenshot")
    ))

  body <- dashboardBody(tabItems(
    tabItem(tabName = "trace",
            fluidRow(plotOutput("spectrum", height = 300,
                                dblclick="spectrum_dbl",
                                brush = brushOpts(
                                  id = "spectrum_brush",
                                  resetOnNew = TRUE
                                ))
            ),
            fluidRow(plotOutput("plot1", height = 300,
                                click = "trace_click",
                                dblclick = "trace_dbl",
                                brush = brushOpts(
                                  id = "trace_brush",
                                  resetOnNew = TRUE
                                )),
                     tags$script(HTML("
          $('#plot1').mousedown(function(e) {
              var parentOffset = $(this).offset();
              var relX = e.pageX - parentOffset.left;
              var relY = e.pageY - parentOffset.top;
              Shiny.setInputValue('x1', relX);
              Shiny.setInputValue('y1', relY);
          }).mouseup(function(e) {
              var parentOffset = $(this).offset();
              var relX = e.pageX - parentOffset.left;
              var relY = e.pageY - parentOffset.top;
              Shiny.setInputValue('x2', relX);
              Shiny.setInputValue('y2', relY);
              Shiny.setInputValue('action', Math.random());
          });
      "))
            ),
            fluidRow(
              tabsetPanel(type = "tabs",
                          tabPanel("Summary", DT::dataTableOutput("peak_summary")),
                          tabPanel("Peak Table", DT::dataTableOutput("peak_table"))
              )
            )
    )
  )
  )

  ui <- (dashboardPage(header, sidebar, body, skin = "black"))


  server <- function(input, output, session) {
    # Trace plot --------------------------------------------------------
    ranges <- reactiveValues(x = NULL, y = NULL)
    ret <- reactiveValues(rt = 1)
    # rt <- reactiveVal(1)
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    params <- reactiveValues(lambdas = c("210"), chroms=chrom_names[1])
    elements <- reactiveVal(c())
    chroms <- reactiveVal(c())
    ggtheme <- theme_classic()

    ### main panel
    output$plot1 <- renderPlot({
      # time <- unique(data$rt)[ret$rt]
      # rts <- data$rt[!duplicated(data$rt)]
      p<-data %>% filter(lambda %in% params$lambdas & chr %in% params$chroms) %>%
        ggplot(aes(rt, value, group=interaction(lambda,chr))) +
        geom_line(aes(color=chr, linetype=lambda)) +
        geom_label(data = subset(data, rt==ret$rt), aes(label=rt)) +
        geom_vline(xintercept = ret$rt, linetype="dotted", col="red") +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
        scale_x_discrete(breaks = scales::breaks_pretty(1))
      p+ggtheme
    })

    output$spectrum <- renderPlot({
      time <- rts[ret$rt]
      p<-data %>% filter(rt %in% time & chr %in% params$chroms) %>% ggplot(aes(lambda, value, group=chr)) +
        coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE) +
        geom_line(aes(color=chr)) + scale_x_discrete(breaks = scales::breaks_extended(10))
      p +ggtheme #+ theme(legend.position="top")
    })

    ### peak table
    # tab <- t(peak_table$pk_meta[c("rt","sd","mean_area","median_area","sd_area"),])
    output$peak_summary <- DT::renderDataTable(peak_sum, selection="single",
                                               options = list(
                                                 columnDefs = list(list(searchable = FALSE, targets = 0))
                                               ), filter = 'top')

    output$peak_table <- DT::renderDataTable(peak_tab, selection="single",
                                             options = list(
                                               columnDefs = list(list(searchable = FALSE, targets = 0))
                                             ), filter = 'top')
    ### side bar
    output$lambda_controls <- renderUI({
      checkboxGroupButtons(
        inputId = "lambda_selector",
        label = "Select wavelengths",
        selected = params$lambdas,
        choices = params$lambdas,
        status = "primary",
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon"))
      )
    })

    output$chrom_controls <- renderUI({
      checkboxGroupButtons(
        inputId = "chrom_selector",
        label = "Select chromatograms",
        choices = params$chroms,
        selected = params$chroms,
        status = "primary",
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon"))
      )
    })

    ### Trace events ------------------------------------------------------------
    observeEvent(input$trace_click, {
      if(input$x1 == input$x2 && input$y1 == input$y2){
        ret$rt <- input$trace_click$x
        # print(ret$rt)
      }
    })

    observeEvent(input$peak_table_rows_selected, {
      RT <- peak_table$pk_meta[3,input$peak_table_rows_selected]
      ret$rt <- which.min(abs(RT - rts))
      # input$peak_table_rows_selected <- input$peak_summary_rows_selected
    })

    observeEvent(input$peak_summary_rows_selected, {
      RT <- peak_table$pk_meta[3,input$peak_summary_rows_selected]
      ret$rt <- which.min(abs(RT - rts))
      # input$peak_summary_rows_selected <- input$peak_table_rows_selected
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$trace_dbl, {
      brush <- input$trace_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })

    ### Spectrum events ------------------------------------------------------------

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$spectrum_dbl, {
      brush2 <- input$spectrum_brush
      if (!is.null(brush2)) {
        ranges2$x <- c(brush2$xmin, brush2$xmax)
        ranges2$y <- c(brush2$ymin, brush2$ymax)

      } else {
        ranges2$x <- NULL
        ranges2$y <- NULL
      }
    })

    ### select wavelengths
    observeEvent(input$select_lambdas, {
      req(input$select_lambdas)
      elements(c(elements(), input$select_lambdas[[1]]))
      params$lambdas <- sort(unique(elements()))
    })

    observeEvent(elements(), {
      req(elements())
      updateSelectInput(session, "select_lambdas",
                        selected = character(0),
                        choices = unique(data$lambda)
      )
    })
    # select chromatograms
    observeEvent(input$select_chroms, {
      req(input$select_chroms)
      chroms(c(chroms(), input$select_chroms))
      params$chroms <- unique(chroms())
    })

    observeEvent(chroms(), {
      req(chroms())
      updateSelectInput(session, "select_chroms",
                        selected = character(0),
                        choices = chrom_names
      )
    })

    observeEvent(input$chrom_selector, {
      updateCheckboxGroupButtons(session = session,
                                 inputId = "chrom_selector",
                                 selected <- input$chrom_selector)
      params$chroms <- selected
      chroms(selected)
    })

    observeEvent(input$lambda_selector, {
      updateCheckboxGroupButtons(session = session,
                                 inputId = "lambda_selector",
                                 selected <- input$lambda_selector)
      params$lambdas <- selected
      elements(selected)
    })
  }


  runGadget(ui, server)
}
