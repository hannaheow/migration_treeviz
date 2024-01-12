load("migtree.Rdata")

library(ggplot2)
#inputs: 
#cinterest = fips code for the county of interest
#type = aig or mig 
#comparison = withinstate or withinrural or none 

highlight = function(statename, name, type, comparison){
    cinterest = unique(ppu2$origid[ppu2$name == name & ppu2$statename == statename])
    if(comparison == "none"){
        pdat = ppu2[ppu2$origid != cinterest,]
    }else if (comparison == "withinstate"){
        pdat = ppu2[startsWith(ppu2$origid, substr(cinterest, 1, 2)) & ppu2$origid != cinterest,]
        
    } else {
        pdat = ppu2[ppu2$urbcode == ppu2$urbcode[ppu2$origid == cinterest][1] & ppu2$origid != cinterest,]
        
    } 
    
    cdat = ppu2[ppu2$origid == cinterest,]
    
    statename =  cdat$statename[1]
    urbname = ifelse(cdat$urbcode == 1, "large central metro", 
                     ifelse(cdat$urbcode == 2, "large fringe metro", 
                            ifelse(cdat$urbcode == 3, "medium metro", 
                                   ifelse(cdat$urbcode == 4, "small metro", 
                                          ifelse(cdat$urbcode == 5, "micropolitan", "noncore")))))
    
    subname = ifelse(comparison == "withinstate", paste0(statename, " counties."), 
                     ifelse(comparison == "none", "counties across the US", 
                            paste0(urbname, " counties.")))
    
    countyname = cdat$name[1]
    ctype = ifelse(type == "mig", "migration", "adjusted gross income")
    name = paste0("Net ", ctype, " for ", countyname,", ", statename, 
                  "\nfrom 2009 through 2020 \ncompared to other ", subname)
    
    p = ggplot() + 
        geom_path(data = pdat, 
                  aes(x = if(type == "mig") x_adj_mig
                      else x_adj_agi, 
                      y = if(type == "mig") y_adj_mig
                      else y_adj_agi, 
                      group = origid),
                  color = "blue",
                  alpha = 0.2, 
                  position = position_dodge(width = 0.2)) + 
        geom_path(data = cdat, aes(x = if(type == "mig") x_adj_mig
                                   else x_adj_agi, 
                                   y = if(type == "mig") y_adj_mig
                                   else y_adj_agi),
                  color = "dark red", size = 2) +
        scale_color_identity() +
        scale_size_identity() +
        scale_alpha_identity() + 
        ggtitle(name) + 
        theme_bw()+
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
    p
}




library("shiny")
#cinterest, type, comparison
ui <- fluidPage(title = "Migration Patterns", 
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                    # Sidebar panel for inputs ----
                    sidebarPanel(
                      p("Each US county is represented by a line."),
                      p(HTML("For each year that net migration or net adjusted gross income is <b>positive</b>, the line curves <b>right</b>. \nFor each year that net migration or net adjusted gross income is <b>negative</b>, the line curves <b>left</b>.")),
                      p(HTML("The base of the <em>tree</em> represents year 2009 while the tips of the <em>tree branches</em> represent year 2020.")),
                        selectInput("statename", "Choose a state:",
                                    choices = unique(ppu2$statename),
                                    selected = 2022),
                        selectInput("name", "Choose a county:", 
                                    choices = ""), 
                        radioButtons("type", "Examine trends in net migration or net adjusted gross income?",
                                    choices = c("Migration" = "mig", "Adjusted Gross Income" = "aig")),
                        radioButtons("comparison", "Make comparisons....",
                                     choices = c("within state" = "withinstate", "within urbanicity category" = "withinurb", 
                                                 "across all US counties" = "none")),
                         p(""),
                        div(em("Created by Hannah Olson-Williams using ",
                            a("US County-to-County Migration Flow Data published by the IRS", href="https://www.irs.gov/statistics/soi-tax-stats-migration-data"))) 
                        
                    ),
                    mainPanel(
                        plotOutput("migtrends"), 
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        )
                    )
                
)
)



# Define server logic to map various measures
server <- function(input, output,session) {
    
    observeEvent(input$statename, {
        updateSelectInput(
            session,
            "name",
            choices = unique(ppu2$name[ppu2$statename == input$statename])
                
        )
    })
    output$migtrends = renderPlot({
        highlight(input$statename, input$name, input$type, input$comparison)
    })
    
}

# Create Shiny app ----
shinyApp(ui, server) 





