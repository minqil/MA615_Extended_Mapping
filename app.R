library(readr)
library(dplyr)
library(tidyverse)
library(srvyr)
library(knitr)
library(reshape2)
library(magrittr)

# read data
project<-read_csv(file="PublicAssistanceFundedProjectsDetails.csv")
# select hurricanes
original_hurricane <- filter(project, incidentType=="Hurricane")

hurricanes<-original_hurricane %>%
    mutate(declarationYear=substring(declarationDate,1,4),
           obligatedYear=substring(obligatedDate,1,4))
hurricanes %<>% select(disasterNumber, declarationYear, declarationDate:totalObligated, obligatedYear, obligatedDate:id)

# Cost of damage in different damage category
hurricane_damage_cate_sum <- original_hurricane%>%group_by(damageCategoryCode)%>%
    srvyr::summarize(projectAmount=sum(projectAmount)/1000000,federalShareObligated=sum(federalShareObligated)/1000000,totalObligated=sum(totalObligated)/1000000)
final_hurricane <- melt(hurricane_damage_cate_sum,id.vars = c("damageCategoryCode"), variable.name = "cost_cate", value.name= "cost_dollar")

# Mapping for damage times and cost
hurricane_ob <- original_hurricane%>%group_by(region=state,subregion=county,year=substring(declarationDate,1,4))%>%
    srvyr::summarize(totalObligated_sum=sum(totalObligated),count=n())

hurricane_ob$region <- tolower(hurricane_ob$region)
hurricane_ob$subregion <- tolower(hurricane_ob$subregion)
hurricane_ob$totalObligated_sum_million <- hurricane_ob$totalObligated_sum/1000000

map_states <- map_data("county", unique(hurricane_ob$state))
map_states_border <- map_data("state",unique(hurricane_ob$state))




final_hurricane_ob <- merge(hurricane_ob,map_states,by=c("region","subregion"))


# The number of hurricanes
## data processing
hurr_num <- original_hurricane %>%
    select(disasterNumber, declarationDate, county, countyCode, state, stateCode, stateNumberCode)

hurr_num %<>%separate(`declarationDate`, c("Year","Month", "Day"), sep = "-")
hurr_num %<>% select(-c(Month, Day))

## bar chart
hurr_num1<- hurr_num %>% 
    group_by(state, Year) %>%
    summarize(hurricanes=n_distinct(disasterNumber)) %>%
    arrange(desc(hurricanes))

hurr_num1$Year<- as.numeric(hurr_num1$Year)

## map
hurr_num2<- hurr_num %>% 
    group_by(state,stateCode,stateNumberCode,county,countyCode,Year) %>%
    summarize(hurricanes=n_distinct(disasterNumber)) %>%
    arrange(desc(hurricanes))

### load data
MainStates <- map_data("state")
AllCounty <- map_data("county")

hurr_num2$county %<>% tolower()

hurr_map <- left_join(hurr_num2, AllCounty, by = c("county"="subregion"))

hurr_map$Year<-as.numeric(hurr_map$Year)



ui<-navbarPage("Hurricanes",
               
               navbarMenu("Numbers of Hurricanes",
                          
                tabPanel("Bar Chart",
                         br(),
                         titlePanel("Declaration Year"),
                         sidebarPanel(
                             selectInput(
                                 inputId = "from",
                                 label = "From:",
                                 choices = sort(unique(hurr_num1$Year))
                                 ),
                             uiOutput('ui_to')
                            
                             ),
                         mainPanel(
                             h3('The Number of Hurricanes in Each State'),
                             plotOutput('bar')
                             )
                          ),
               
              
                 tabPanel("Map",
                          br(),
                          titlePanel("Declaration Year"),
                          sidebarPanel(
                              selectInput(
                                  inputId = "from1",
                                  label = "From:",
                                  choices = sort(unique(hurr_map$Year))
                              ),
                              uiOutput('ui_to_map')
                          ),
                          mainPanel(
                              h3('The Number of Hurricanes in Each County'),
                              plotOutput('map1')
                          )
                        )
               
               ),
               
               tabPanel("Damage Times and Expenses",
                        br(),
                        sidebarPanel(
                            uiOutput('ui_year')
                        ),
                        mainPanel(
                            h3('Different Expenses in Each Damage Category and Area'),
                            plotOutput('map')
                        ),
                        mainPanel(
                            
                            plotOutput('map_count')
                        )
               ),
               
               tabPanel("Damage Category",
                        br(),
                        sidebarPanel(
                            selectInput(
                                inputId = "damagecategorycode",
                                label = "DamageCategoryCode",
                                choices = sort(unique(final_hurricane$damageCategoryCode))
                                
                            ),
                            uiOutput("cost_cate")
                        ),
                        mainPanel(
                            h3('Different Expenses in Each Damage Category'),
                            plotOutput('p')
                        )
               ),
               
               tabPanel("DataTable",
                        fluidRow(
                            column(4,
                                   selectInput("disasterNumber",
                                               "DisasterNumber:",
                                               c("All",
                                                 unique(as.character(hurricanes$disasterNumber))))
                            ),
                            column(4,
                                   selectInput("declarationYear",
                                               "DeclarationYear:",
                                               c("All",
                                                 unique(as.character(hurricanes$declarationYear))))
                            ),
                            column(4,
                                   selectInput("obligatedYear",
                                               "ObligatedYear:",
                                               c("All",
                                                 unique(as.character(hurricanes$obligatedYear))))
                            ),
                            column(4,
                                   selectInput("damageCategory",
                                               "DamageCategory:",
                                               c("All",
                                                 unique(as.character(hurricanes$damageCategory))))
                            )
                            
                        ),
                        DT::dataTableOutput("table")
               )
               
)



server <- function(input,output){
    
    plot_bar=reactive({
        bar1<-hurr_num1%>%
            filter(Year>=input$from&Year<=input$to)%>%
            group_by(state) %>%
            summarize(hurricanes=sum(hurricanes))
        
        bar2<-ggplot(bar1, aes(x=state, y=hurricanes))+
            geom_bar(stat = "identity", fill = "lightblue")+
            coord_flip()+
            geom_text(aes(label = hurricanes), vjust = 0.5, colour = "black", position = position_dodge(.9), size = 3)+
            theme(panel.background=element_rect(fill='transparent',
                                                color ="gray"), 
                  axis.text.x = element_text( hjust = 0.5, 
                                              vjust = 0.5,color = "black",size=5))+
            ggtitle("The Number of Hurricanes in Each State")+
            xlab("State")+ylab("Count")+
            theme(plot.title = element_text(hjust = 0.5))
        ggpubr::ggarrange(bar2,ncol = 1)
        
    
    })
    output$bar=renderPlot(plot_bar())
    
    plot_map=reactive({
        hurr_map2<-hurr_map%>%
            filter(Year>=input$from1&Year<=input$to_map)%>%
            group_by(county, long,lat,group)%>%
            summarize(hurricanes=sum(hurricanes))
        
        hurr_map3 <- ggplot() + 
            geom_polygon(data=AllCounty, aes(x=long, y=lat, group=group),
                         color="gray", fill="white", size = .1 ) + 
            geom_polygon(data = hurr_map2, aes(x = long, y = lat, group = group,fill = `hurricanes`), color = "grey", size = 0.2, alpha = 1.6) +
            scale_fill_gradient(low="steelblue",high="blue")+
            geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),color="black", fill="white",  size = .5, alpha = .3) +
            ggtitle("The Number of Hurricanes in Each County") +
            xlab("Longtitude")+ylab("Latitude")+labs(fill="Count")+
            theme(plot.title = element_text(hjust = 0.5))
        ggpubr::ggarrange(hurr_map3,ncol = 1)
        
    })
    output$map1=renderPlot(plot_map())
    
    
    s_year=reactive({
        sort(unique(final_hurricane_ob$year))
    })
    output$ui_year=renderUI({
        checkboxGroupInput(
            inputId = "year",
            label = "Year",
            choices = s_year(),
            selected = s_year()
        )
    })
    
    output$ui_to=renderUI({
        selectInput(
            inputId = "to",
            label = "To:",
            choices = as.character(subset(sort(unique(as.numeric(hurr_num$Year))),sort(unique(as.numeric(hurr_num$Year)))>input$from))
        )
        
    })
    
    
    output$ui_to_map=renderUI({
        selectInput(
            inputId = "to_map",
            label = "To:",
            choices = as.character(subset(sort(unique(as.numeric(hurr_num$Year))),sort(unique(as.numeric(hurr_num$Year)))>input$from1))
        )
        
    })
    
    plot_damage=reactive({
        options(
            ggplot2.continuous.colour = "viridis",
            ggplot2.continuous.fill = "viridis"
        )
        x1 <- filter(final_hurricane_ob,year%in%input$year)
        p1 <- ggplot()+geom_polygon(x1, mapping = aes(x=long,y=lat,group=group, fill=totalObligated_sum_million))+
            geom_path(map_states, mapping=aes(x=long, y=lat, group=group),color="grey")+
            geom_path(map_states_border, mapping=aes(x=long, y=lat, group=group),color="black")+
            labs(fill="Sum of Total Obligated
       in million")+
            xlim(min(map_states$long),max(map_states$long))+
            ylim(min(map_states$lat),max(map_states$lat))+
            #change the name of x, y, and title
            xlab("Longtitude")+ylab("Latitude")+
            ggtitle("Total Obligated Expenses in Each Area")+
            theme(plot.title = element_text(hjust = 0.5))
        
        
       
        ggpubr::ggarrange(p1,ncol = 1)
        
    })
    output$map=renderPlot(plot_damage())
    
    plot_count=reactive({
        options(
            ggplot2.continuous.colour = "viridis",
            ggplot2.continuous.fill = "viridis"
        )
        x1 <- filter(final_hurricane_ob,year%in%input$year)
        p2 <- ggplot()+geom_polygon(x1, mapping = aes(x=long,y=lat,group=group, fill=count))+
            geom_path(map_states, mapping=aes(x=long, y=lat, group=group),color="grey")+
            geom_path(map_states_border, mapping=aes(x=long, y=lat, group=group),color="black")+
            xlim(min(map_states$long),max(map_states$long))+
            ylim(min(map_states$lat),max(map_states$lat))+
            #change the name of x, y, and title
            xlab("Longtitude")+ylab("Latitude")+
            labs(fill="Count")+
            ggtitle("Numbers of Expenses in Each Area")+
            theme(plot.title = element_text(hjust = 0.5))
        ggpubr::ggarrange(p2,ncol = 1)
        
        
    })
    output$map_count=renderPlot(plot_count())
    
    
    s_cost_cate = reactive({
        sort(unique(final_hurricane$cost_cate))
    })
    output$cost_cate=renderUI({
        checkboxGroupInput(
            inputId = "cost",
            label = "Sum of Cost From 1998 To 2020",
            choices = s_cost_cate(),
            selected = s_cost_cate()
        )
    })
    
    plot_cost=reactive({
        x <- filter(final_hurricane,damageCategoryCode%in%input$damagecategorycode,cost_cate%in%input$cost)
        p <- ggplot(x, mapping = aes(x=cost_cate, y=cost_dollar))+
            geom_bar(stat = "identity", fill = "#75AADB", border = "white")+
            xlab("Damage Category")+
            ylab("Expenses($ in million)")+
            labs(fill="Expense Categories")+
            ylim(0, max(final_hurricane$cost_dollar))
        ggpubr::ggarrange(p,ncol = 1)
    })
    output$p=renderPlot(plot_cost())
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- hurricanes
        if (input$disasterNumber != "All") {
            data <- data[data$disasterNumber == input$disasterNumber,]
        }
        if (input$declarationYear != "All") {
            data <- data[data$declarationYear == input$declarationYear,]
        }
        if (input$obligatedYear != "All") {
            data <- data[data$obligatedYear == input$obligatedYear,]
        }
        if (input$damageCategory != "All") {
            data <- data[data$damageCategory == input$damageCategory,]
        }
        
        data
    }))
    
    
}

shinyApp(ui = ui, server = server)

