library(shiny)
library(shinycssloaders)
library(shinyBS)
source("utils.R")


ui <- fluidPage(div(style   = "margin: 10px 20px;",
    includeCSS("www/style.css"),

    div(style = "padding: 0 20px",
    div(
        div(style = "width: 200px; display: inline-block;  vertical-align: top;",
               img(src = "logo.jpg"),
               h1("PACKMAN")
              ),
        div(style = "width: 300px; display: inline-block; vertical-align: top;",
           h3("Pouch Pattern Generator"),
        br(),
            textInput("name", "Name of pattern", value = "Layout #001"),
            sliderInput("h",
                        "Height:",
                        min = 5,
                        max = 25,
                        value = 12, ticks = F),
            sliderInput("w1",
                        "Width at top:",
                        min = 5,
                        max = 25,
                        value = 8, ticks = F),
            sliderInput("w2",
                        "Width at bottom:",
                        min = 5,
                        max = 25,
                        value = 10, ticks = F),
            sliderInput("l1",
                        "Length at top:",
                        min = 5,
                        max = 50,
                        value = 18, ticks = F),
            sliderInput("l2",
                        "Length at bottom:",
                        min = 5,
                        max = 50,
                        value = 20, ticks = F),
            sliderInput("x",
                        tagList("Zipper reach %", shinyBS::tipify(icon("info-circle"), title = "How far down the side the zipper goes. Gets a bit wierd at the extremes.", placement = "right")),
                        min = 0,
                        max = 100,
                        post = "%",
                        value = 50, ticks = F),
            div(numericInput("z","Zipper width:",value = 3, min = 1, max = 6, step = .1, width = "100px"), style = "display: inline-block;"),
            div(numericInput("s", "Seam allowance:",value = 1, min = .5, max = 2.5, step = .1, width = "120px"), style = "display: inline-block;"),
            br(),
            downloadButton("png", label = "Save to PNG", width = "100%"),
            br(),
            br(),
            div(tags$a(icon("github"), "View source on github", href = "http://www.github.com", style = "color: #999;"))
        ),

        div(style = "width: 800px; display: inline-block; vertical-align: top;",
           plotOutput("pattern", width = "800px", height = "800px")  %>% withSpinner(color="#cccccc", type = 7)),
       div(style = "width: 500px; display: inline-block; vertical-align: top;",
           br(),
           h4("Instructions"),
           tags$ol(
               tags$li("Print (can be tricky to print to scale) or copy pattern measurements to paper and cut it out."),
               tags$li("Lay out your fabric and mark a cross."),
               tags$li("Chalk the the pattern in each of the four quadrants, starting the the top right corder and then flipping it horizontally and vertically as you go around."),
               tags$li("Cut it out"),
               tags$li("(Bonus) Iron press the top and bottom creases, will look a little neater in the end."),
               tags$li("Split your zipper and sew right-side to right-side on the very top and bottom of the pattern."),
               tags$li("Top stich that zipper to make it look like pro"),
               tags$li("Install zipper slider and turn the pouch inside out"),
               tags$li("(Bonus) Add some flavor: zipper tabs, mesh pockets, handles, labels etc. while fabric is still flat"),
               tags$li("Turn the fabric so that the now closed zipper is centered on top and wrangle it so you can sew both of the sides together, then bind the edges."),
               tags$li("(Bonus) At this point you may or may be able to do a top-stitch to the seam you just did, depends on the shape of the pouch and how nimble your sewing machine is."),
               tags$li("Make sure your zipper is somewhat open at this point"),
               tags$li("One by one, line up each of the four boxed corners together and sew them shut and the finally bind the edges."),
               tags$li("Turn pouch right side out. Done")
           ))
    
))
))
server <- function(input, output) {
    
    pattern <- reactive({
       req(input$s)  
       req(input$z)
       p <- make_pattern(h = input$h, w1 = input$w1, w2 = input$w2, l1 = input$l1, l2 = input$l2, z = input$z, x = input$x / 100, s = input$s)
       p$m <- max(p$A, p$B, p$E) 
       p$plot <- plot_pattern(p, input$name)
       p
    })

    output$pattern <- renderPlot({
        
        p <- pattern()
        
        
        if (p$B - p$alpha_x < p$s) {
            out <- ggplot() +
                annotate("text", x = 0, y = 0, label = "Zipper reach needs to be longer...", size = 6) +
                theme_void() 
            
            return(out)
        }
        
        if (p$E - p$beta_x <= p$s) {
           out <-  ggplot() +
                annotate("text", x = 0, y = 0, label = "Zipper reach needs to be shorter...", size = 6) +
                theme_void() 
           
           return(out)
        }
        
        p$plot
    })
    
    output$png <- downloadHandler(
        filename = function(x = input$name) {
            str_c(str_to_lower(str_replace_all(make.names(x), "\\.", "")), ".png")
        },
        content = function(file) {
            ggsave(filename = file, plot = pattern()$plot, device = png(), units = "cm", height = pattern()$A, width = max(pattern()$B, pattern()$E))
        }
    )
    
}

shinyApp(ui = ui, server = server)
