server <- function(input, output) {
  
  # SPARK VALUE BOX THEME ------------------------------------------------------
  valueBoxSpark <- 
    function(value, 
             title, 
             sparkobj = NULL, 
             subtitle, 
             info = NULL, 
             icon = NULL, 
             color = "aqua",  
             width = 12, 
             href = NULL){
      shinydashboard:::validateColor(color)
      
      if (!is.null(icon))
        shinydashboard:::tagAssert(icon, type = "i")
      
      info_icon <- tags$small(
        tags$i(
          class = "fa fa-info-circle fa-lg",
          title = info,
          `data-toggle` = "tooltip",
          style = "color: rgba(255, 255, 255, 0.75);"
        ),
        # bs3 pull-right 
        # bs4 float-right
        class = "pull-right float-right"
      )
      
      boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
          class = "inner",
          tags$small(title),
          if (!is.null(sparkobj)) info_icon,
          h3(value),
          if (!is.null(sparkobj)) sparkobj,
          p(subtitle)
        ),
        # bs3 icon-large
        # bs4 icon
        if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
      )
      
      if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
      
      div(
        class = if (!is.null(width)) paste0("col-sm-", width), 
        boxContent
      )
    }
  
  
  # OVERVIEW TAB - START -------------------------------------------------------
  
  output$overviewSalesGrowth <- renderValueBox({
    salesGrowth <- 
      df_clean %>% 
      group_by(Year) %>% 
      summarise(`Total Sales` = sum(Sales)) %>% 
      mutate_each(funs(factor(.)), c("Year")) %>% 
      mutate(GrowthValue = `Total Sales` - lag(`Total Sales`),
             GrowthPerc = (`Total Sales` - lag(`Total Sales`)) / `Total Sales`) %>%
      mutate(GrowthValue = replace_na(GrowthValue, 0),
             GrowthPerc = replace_na(GrowthPerc, 0))
    
    hcSalesGrowth <- 
      salesGrowth %>% 
      hchart("area", hcaes(x = Year, y = GrowthValue), name = "Sales Growth") %>% 
      hc_size(height = 50) %>%
      hc_credits(enabled = F) %>% 
      hc_tooltip(enabled = F) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    vbSalesGrowth <- valueBoxSpark(
      value = dollar(mean(salesGrowth$GrowthValue), prefix = "$", big.mark = ",", decimal.mark = ".", accuracy = .01),
      title = toupper("AVERAGE SALES GROWTH"),
      sparkobj = hcSalesGrowth,
      info = "This is the sales growth from the first day until today",
      subtitle = tagList("Growth per year ",
                         HTML("&uarr;"), 
                         percent(mean(salesGrowth$GrowthPerc), 
                                 decimal.mark = ".", 
                                 accuracy = .01)),
      icon = icon("chart-line"),
      color = "purple",
      href = NULL
    )
    
    vbSalesGrowth
  })
  
  output$overviewProfitGrowth <- renderValueBox({
    profitGrowth <- 
      df_clean %>% 
      group_by(Year) %>% 
      summarise(`Total Profit` = sum(Profit)) %>% 
      mutate_each(funs(factor(.)), c("Year")) %>% 
      mutate(GrowthValue = `Total Profit` - lag(`Total Profit`),
             GrowthPerc = (`Total Profit` - lag(`Total Profit`)) / `Total Profit`) %>%
      mutate(GrowthValue = replace_na(GrowthValue, 0),
             GrowthPerc = replace_na(GrowthPerc, 0))
    
    hcProfitGrowth <- 
      profitGrowth %>% 
      hchart("area", hcaes(x = Year, y = GrowthValue), name = "Profit Growth") %>% 
      hc_size(height = 50) %>%
      hc_credits(enabled = F) %>% 
      hc_tooltip(enabled = F) %>%
      hc_add_theme(hc_theme_sparkline_vb())
    
    vbProfitGrowth <- 
      valueBoxSpark(
        value = dollar(mean(profitGrowth$GrowthValue), prefix = "$", big.mark = ",", decimal.mark = ".", accuracy = .01),
        title = toupper("AVERAGE PROFIT GROWTH"),
        sparkobj = hcProfitGrowth,
        info = "This is the profit growth from the first day until today",
        subtitle = tagList("Growth per year ",
                           HTML("&uarr;"), 
                           percent(mean(profitGrowth$GrowthPerc), 
                                   decimal.mark = ".", 
                                   accuracy = .01),
        ),
        icon = icon("dollar"),
        color = "purple",
        href = NULL
      )
    
    vbProfitGrowth
  })
  
  output$overviewSalesSeason <- renderValueBox({
    salesSeason <- 
      df_clean %>% 
      mutate(Month = month(`Order Date`, label = T, abbr = F)) %>% 
      group_by(Year, Month) %>% 
      summarise(Sales.Number = length(Sales)) %>% 
      group_by(Month) %>% 
      mutate(Sales.Number = mean(Sales.Number))
    
    peakSeason <- salesSeason[salesSeason$Sales.Number == max(salesSeason$Sales.Number), ]
    
    peakSeason2 <- 
      df_clean %>% 
      mutate(Month = month(`Order Date`, label = T, abbr = F)) %>%
      filter(Month == peakSeason$Month[1]) %>% 
      group_by(Year, Month) %>% 
      summarise(Sales.Number = length(Sales))
    
    hcSalesSeason <- 
      peakSeason2 %>% 
      hchart(
        "area",
        hcaes(x = Year, y = Sales.Number)) %>% 
      hc_size(height = 50) %>% 
      hc_tooltip(enabled = F) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    vbSalesSeason <- 
      valueBoxSpark(
        value = peakSeason$Month[1],
        title = toupper("PEAK SALES MONTH"),
        sparkobj = hcSalesSeason,
        info = "Graph showing number of time on peak month each year",
        subtitle = paste("Avg. sales on peak month :",
                         number(peakSeason$Sales.Number[1], 
                                big.mark = ","),
                         "transaction"),
        icon = icon("calendar-alt"),
        color = "purple",
        href = NULL
      )
    
    vbSalesSeason
  })
  
  output$overviewProfitbyRegion <- renderEcharts4r({
    df_clean %>% 
      group_by(Region) %>% 
      summarise(Profit = sum(Profit)) %>% 
      arrange(Profit) %>% 
      e_chart(Region) %>%
      e_pie(Profit, radius = c ("50%", "75%")) %>%
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_title(text = "Profit by Region",
              left = "center",
              top = "0") %>% 
      e_legend(F) %>%
      e_tooltip(trigger = "item",
                formatter = JS("
                           function(params){return(
                           '<b>' + params.name + '</b>'
                           + ' : $'
                           + (params.value).toLocaleString('en-US', 
                           {maximumFractionDigits : 2, minimumFractionDigits: 2})
                           )}
                           "))
  })
  
  output$overviewProfitbySegment <-  renderEcharts4r({
    df_clean %>% 
      group_by(Segment) %>% 
      summarise(Profit = sum(Profit)) %>% 
      arrange(Profit) %>% 
      e_chart(Segment) %>% 
      e_pie(Profit, radius = c ("50%", "75%")) %>%
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_title(text = "Profit by Segment",
              left = "center",
              top = "0") %>% 
      e_legend(F) %>% 
      e_tooltip(trigger = "item",
                formatter = JS("
                            function(params){return(
                            '<b>' + params.name + '</b>' 
                            + ': $' 
                            + (params.value).toLocaleString('en-US', 
                            {maximumFractionDigits: 2, minimumFractionDigits: 2})
                            )}
                            "))  
  })
  
  # OVERVIEW TAB - END ---------------------------------------------------------
  
  
  # Analysis1 TAB - START ------------------------------------------------------
  
  output$profitSegment <- renderEcharts4r({
    
    df_clean %>% 
      filter(State == input$stateSelector) %>% 
      filter(Mon.Year >= input$dateSelector[1] & Mon.Year <= input$dateSelector[2]) %>%
      filter(Category %in% input$categorySelector) %>% 
      group_by(Segment, Mon.Year) %>% 
      summarise(Profit = sum(Profit)) %>% 
      group_by(Segment) %>%
      e_charts(Mon.Year) %>% 
      e_line(Profit) %>% 
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_title(text = "Profit for Each Segment",
              top = "0",
              left = "center") %>% 
      e_legend(top = "30") %>% 
      e_y_axis(formatter = e_axis_formatter(style = "currency", currency = "USD")) %>% 
      e_axis_labels(y = "Profit") %>% 
      e_mark_point(data = list(type = "max"),
                   title = "Max") %>% 
      e_mark_point(data = list(type = "min"), 
                   title = "Min") %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                   function(params) {return(
                   '<b>' + params.value[0] + '</b>'
                   + ': $'
                   + params.value[1].toLocaleString('en-US', 
                   {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                   )}
                   ")
      )
    
  })
  
  output$salesSegment <- renderEcharts4r({
    df_clean %>% 
      filter(State == input$stateSelector) %>% 
      filter(Mon.Year >= input$dateSelector[1] & Mon.Year <= input$dateSelector[2]) %>%
      filter(Category %in% input$categorySelector) %>% 
      group_by(Segment, Mon.Year) %>% 
      summarise(Sales = sum(Sales)) %>% 
      group_by(Segment) %>% 
      e_charts(Mon.Year) %>% 
      e_line(Sales) %>% 
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_title(text = "Value of Sales for Each Segment",
              top = "0",
              left = "center") %>% 
      e_legend(top = "30") %>% 
      e_y_axis(formatter = e_axis_formatter(style = "currency", currency = "USD")) %>% 
      e_axis_labels(y = "Value of Sales") %>% 
      e_mark_point(data = list(type = "max"),
                   title = "Max") %>% 
      e_mark_point(data = list(type = "min"), 
                   title = "Min") %>% 
      e_tooltip(
        trigger = "item",
        formatter = JS("
                   function(params) {return(
                   '<b>' + params.value[0] + '</b>'
                   + ': $'
                   + params.value[1].toLocaleString('en-US', 
                   {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                   )}
                   ")
      )
  })
  
  output$profitableSegment <- renderEcharts4r({
    df_clean %>% 
      filter(State == input$stateSelector) %>% 
      filter(Mon.Year >= input$dateSelector[1] & Mon.Year <= input$dateSelector[2]) %>%
      filter(Category %in% input$categorySelector) %>%
      group_by(Segment) %>% 
      summarise(Total = sum(`Sales`)) %>%
      arrange(Total) %>% 
      e_charts(Segment) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      # e_y_axis(inverse = TRUE) %>% 
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_legend(show = FALSE) %>%
      e_title("Most Profitable Segment", 
              top = "0", 
              left = "center") %>%
      e_x_axis(name = "Profit",
               nameLocation = "center",
               nameGap = "25",
               formatter = e_axis_formatter(style = "currency", currency = "USD")) %>% 
      e_tooltip(trigger = "item", 
                formatter = JS("
                           function(params){return(
                           params.value[1] + ' : '
                           +params.value[0]
                           )}
                           "))
  })
  
  output$shippingStats <- renderEcharts4r({
    df_clean %>% 
      filter(State == input$stateSelector) %>% 
      filter(Mon.Year >= input$dateSelector[1] & Mon.Year <= input$dateSelector[2]) %>%
      filter(Category %in% input$categorySelector) %>% 
      group_by(Segment, `Ship Mode`) %>% 
      summarise(Total = length(`Ship Mode`)) %>% 
      arrange(Total) %>% 
      group_by(Segment) %>% 
      e_charts(`Ship Mode`) %>% 
      e_bar(Total, stack = "stack") %>% 
      e_flip_coords() %>% 
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_title(text = "Ship Mode by Segment",
              top = "0",
              left = "center") %>% 
      e_legend(top = "30") %>% 
      e_axis(name = "Number of Shipping (shipments)",
             nameLocation = "center",
             nameGap = "25") %>% 
      e_tooltip(trigger = "item", 
                formatter = JS("
                           function(params){return(
                           '<b>' + params.value[1] + '</b>'
                           + ' : ' 
                           + params.value[0]
                           + ' shipments'
                           )}
                           "))
  })
  
  # Analysis1 TAB - END --------------------------------------------------------
  
  
  # Analysis2 TAB - START ------------------------------------------------------
  
  output$profitableSubCategory <- renderEcharts4r({
    df_clean %>% 
      filter(State == input$stateSelector1) %>% 
      filter(Mon.Year >= input$dateSelector1[1] & Mon.Year <= input$dateSelector1[2]) %>%
      filter(Category %in% input$categorySelector1) %>%
      group_by(`Sub-Category`) %>% 
      summarise(Total = sum(`Sales`)) %>%
      arrange(Total) %>% 
      e_charts(`Sub-Category`) %>% 
      e_bar(Total) %>% 
      e_flip_coords() %>% 
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_legend(show = FALSE) %>%
      e_title("Profitable Sub-category", 
              top = "0", 
              left = "center") %>%
      e_x_axis(name = "Profit",
               nameLocation = "center",
               nameGap = "25",
               formatter = e_axis_formatter(style = "currency", currency = "USD")) %>% 
      e_tooltip(trigger = "item", 
                formatter = JS("
                           function(params){return(
                           params.value[1] + ' : '
                           +params.value[0]
                           )}
                           "))
  })
  
  output$missedprofitSubCategory <- renderEcharts4r({
    
    df_clean%>% 
      filter(State == input$stateSelector1) %>% 
      filter(Mon.Year >= input$dateSelector1[1] & Mon.Year <= input$dateSelector1[2]) %>%
      filter(Category %in% input$categorySelector1) %>%
      group_by(`Sub-Category`) %>% 
      summarise(`Missed Profit` = sum(Profit)) %>% 
      arrange(-`Missed Profit`) %>%
      e_charts(`Sub-Category`) %>%
      e_bar(`Missed Profit`) %>%
      e_theme_custom("www/Chart_Theme.json") %>% 
      e_legend(show = FALSE) %>%
      e_flip_coords() %>%
      e_x_axis(name = "Missed Profit",
               nameLocation = "center",
               nameGap = "25",
               formatter = e_axis_formatter(style = "currency", currency = "USD")) %>% 
      e_title(text = "Most Missed Profit by Sub-category (Returned)",
              left = "center",
              top = "0") %>% 
      e_tooltip(trigger = "item",
                formatter = JS("
            function(params){
            return('<b>' + params.name + ':' + '</b>' + ' $' + 
            params.value[0]) 
            }"))
  })
  
  # Analysis2 TAB - END --------------------------------------------------------
  
  
  # MAP TAB - START ------------------------------------------------------------
  
  output$Mapping <- renderEcharts4r({
    df_map <- 
      df_clean %>% 
      filter(Category == input$categorycheckSelector,
             Year == input$yearSelector)
    
    if (input$valueSelector == "Profit") {
      json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
      plot_map <- 
        df_map %>%
        group_by(State) %>% 
        summarise(Total = sum(Profit)) %>% 
        e_charts(State) %>% 
        e_map_register("USA", json) %>% 
        e_map(Total, map = "USA") %>% 
        e_visual_map(Total) %>% 
        e_theme_custom("www/Chart_Theme.json") %>%
        e_tooltip(trigger = "item",
                  formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ':' + '</b>' 
                                 + ' $' + 
                                 (params.value).toLocaleString('en-US', 
                                 {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                                 )}
                                 "))
    }
    else if (input$valueSelector == "Sales") {
      json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
      plot_map <- 
        df_map %>%
        group_by(State) %>% 
        summarise(Total = sum(Sales)) %>% 
        e_charts(State) %>% 
        e_map_register("USA", json) %>% 
        e_map(Total, map = "USA") %>% 
        e_visual_map(Total) %>% 
        e_theme_custom("www/Chart_Theme.json") %>%
        e_tooltip(trigger = "item",
                  formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ':' + '</b>' 
                                 + ' $' 
                                 + (params.value).toLocaleString('en-US', 
                                 {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                                 )}
                                 "))
    }
    else {
      json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
      plot_map <- 
        df_map %>%
        group_by(State) %>% 
        summarise(Total = length(Sales)) %>% 
        e_charts(State) %>% 
        e_map_register("USA", json) %>%
        e_map(Total, map = "USA") %>% 
        e_visual_map(Total) %>% 
        e_theme_custom("www/Chart_Theme.json") %>%
        e_tooltip(trigger = "item",
                  formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ': ' + '</b>' 
                                 + params.value 
                                 + ' transactions'
                                 )}
                                 "))
    }
    
    plot_map
    
  })
  
  # MAP TAB - END --------------------------------------------------------------
  
  
  # Data Source TAB - END ------------------------------------------------------
  
  output$database <- DT::renderDataTable(database)
  
  # Data Source TAB - END ------------------------------------------------------
  
  
}