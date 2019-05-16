#
#
#

sspm <- function(...)
    suppressPackageStartupMessages(library(...))

sspm("tidyverse")
sspm("DT")
sspm("RColorBrewer")
sspm("shinythemes")

file_info <- file.info("data/brkga.rds")

simulation <- read_rds("data/brkga.rds") %>% mutate(
    Type = str_sub(Instancia, 6,8), 
    SubType = str_sub(Instancia, 10,10), 
    File = as.integer(str_remove(str_sub(Instancia, 12,13), "_")), 
    n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
    m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
    Name = paste(SubType, File, sep = "-"),
    Instancia = str_remove(str_remove(Instancia, pattern = "conv_"), pattern = ".txt"))


brito <- read_rds("data/brito.rds")  %>% mutate(
    Type = str_sub(Instancia, 1,3), 
    SubType = str_sub(Instancia, 5,5), 
    File = as.integer(str_remove(str_sub(Instancia, 7,8), "_")), 
    n = as.integer(str_remove(str_extract(Instancia,pattern = "n\\d+"), pattern = "n")),
    m = as.integer(str_remove(str_extract(Instancia,pattern = "m\\d+"), pattern = "m")),
    Name = paste(SubType, File, sep = "-"))

db <- simulation %>% select(-Target, -LSEr, -BKEr) %>% 
    right_join(brito, by = c("Instancia", "Type", "SubType", "File", "Name", "n", "m")) %>% 
    mutate(LSEr = 100*(round(LS,2) - Target)/Target,
           BKEr = 100*(round(BRKGA,2) - Target)/Target)

new_simulation <- db %>% filter(!is.na(Order))

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage("CAST BRKGA", windowTitle = "CAST BRKGA", theme = shinytheme("cerulean"),
                 tabPanel("Monitor",
                          fluidRow(
                              span(style="text-align:left;font-weight:700;padding:10pt 0 10pt 10pt;color:red", "Last Update:"),
                              span(style="text-align:left;font-weight:700;padding:10pt 10pt 10pt 0pt;color:black", strftime(file_info$mtime, format = "%Y-%m-%d %H:%M:%S %Z"))
                          ),
                          fluidRow(
                              h3(style="text-align:left;font-weight:700;padding:10pt" ,"Monitor")
                          ),
                          plotOutput("monitor"),
                          hr(),
                          fluidRow(
                              h3(style="text-align:left;font-weight:700;padding:10pt" ,"Instance filters")
                          ),
                          
                          fluidRow(
                              column(5, offset = 1,
                                    uiOutput("uiType"),
                                    uiOutput("uiSubType")
                                     ),
                              column(5,
                                     uiOutput("uiN"),
                                     uiOutput("uiM")
                                     )
                          ),
                          #hr(),
                          fluidRow(
                              h3(style="text-align:left;font-weight:700;padding:10pt" ,"Results")
                          ),
                          fluidRow(
                              column(10, offset = 1,
                                     DTOutput("simulation", width = '100%')
                                     )
                              )
                          ),
                 tabPanel("Figures"),
                 tabPanel("Tables")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$uiType <- renderUI(
        tagList(
            selectInput('Type', 'Type', sort(unique(simulation$Type)), selected = "MDG"),
            br()
        )
    )

    output$uiSubType <- renderUI(
        if (!is.null(input$Type)){
            tagList(
                selectInput('SubType', 'Subtype', simulation %>% filter(Type == input$Type) %>%
                                select(SubType) %>%
                                unlist(use.names = FALSE) %>%
                                unique() %>%
                                sort(decreasing = TRUE)
                ),
                br()
            )
        }
    )

    output$uiN <- renderUI({
        if (!is.null(input$SubType)){
            tagList(
                selectInput('N', 'Instance size',
                            simulation %>% filter(SubType == input$SubType) %>%
                                select(n) %>%
                                unlist(use.names = FALSE) %>%
                                unique() %>%
                                sort(decreasing = TRUE)
                ),
                br()
            )   
        }
    })

    output$uiM <- renderUI({
        if (!is.null(input$N)){        
        tagList(
            selectInput('M', 'Tour size',
                        simulation %>% filter(n == input$N) %>%
                            select(m) %>%
                            unlist(use.names = FALSE) %>%
                            unique() %>%
                            sort(decreasing = TRUE)
                        ),
            br()
        )
        }
    })

    output$monitor <- renderPlot({
        base <- db %>% 
            filter(Type == "MDG") %>% 
            mutate(nA = is.na(LSEr),
                   target = abs(round(LSEr, 2) - 0.0) <= .Machine$double.eps,
                   draw = (!target)&(abs(round(LSEr,2) + BRKGAPDM) <= .Machine$double.eps),
                   best = (!target)&(round(LSEr,2) > -BRKGAPDM),
                   lost = (!target)&(round(LSEr,2) < -BRKGAPDM)) %>% 
            gather(Relation, Value, -Order:-BKEr) %>% 
            filter(Value) %>% 
            mutate(Relation = factor(Relation, levels = c("target", "best",   "draw", "lost")))
        base %>% 
            ggplot() + 
            geom_bar(aes(x = as.factor(File), fill = Relation), position = "stack", na.rm = TRUE) + 
            facet_wrap(. ~ SubType, nrow = 3, strip.position = "right") + #xlim(1, 40) +
            scale_y_continuous(name ="Replications", breaks = seq(0,10,by = 2)) +
            scale_x_discrete(name ="Instance", breaks = paste0(seq(1,max(db$File),by = 2))) +
            scale_fill_manual(breaks = c("target", "best",   "draw", "lost"), 
                              values = rev(RColorBrewer::brewer.pal(4, "RdYlGn")), drop = FALSE) +
            theme(legend.position="top") + labs(fill = " ")
        
    })
    
    output$simulation <- renderDT({
        if (!(is.null(input$Type) || 
              is.null(input$SubType) ||
              is.null(input$N) ||
              is.null(input$M))) {
        base <- new_simulation %>% 
            filter(Type == input$Type,
                   SubType == input$SubType,
                   n == as.numeric(input$N),
                   m == as.numeric(input$M)) %>%  
        select(Type, SubType, n, m, File, Order, LSEr, BRKGAPDM, BKEr, N_gen, N_bst, Duration) %>% 
            mutate(Duration = round(Duration,2)) %>% arrange(Type, SubType, n, m, -File, -Order) %>% 
            mutate(LSEr = round(LSEr, 3),
                   BKEr = round(BKEr, 3))

            datatable( base, 
                colnames = c("Type", "Subtype", "n", "m", "File", "Order", "LSEr", "PDMEr", "BKEr", "Generations", "Best", "Duration"),
                extensions = c('Buttons',  'KeyTable', 'Responsive'),
                options = list(
                    keys = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    columnDefs = list(list(className = 'dt-center', targets = c(1,2)),
                                      list(orderable = FALSE, targets = 1:12)),
                    pageLength = 40,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#006600', 'color': '#fff'});",
                        "}")
                )
            ) %>%
                formatRound(columns = c("LSEr", "BRKGAPDM", "BKEr", "Duration"), digits =  2)
        }
        
        }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
