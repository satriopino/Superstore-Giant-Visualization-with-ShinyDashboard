## UI Dashboard

# Header Dashboard
header <- dashboardHeader(title = 'Superstore Giant Dashboard', titleWidth = 285)

# Sidebar Dashboard
sidebar <- dashboardSidebar(width = 285,
  sidebarMenu(
    menuItem(text = "Overview", tabName = "overview", icon = icon("home")),
    menuItem(text = "Superstore Segment Analysis", tabName = "analysis1", icon = icon("chart-line")),
    menuItem(text = "Superstore Product Analysis", tabName = "analysis2", icon = icon("list")),
    menuItem(text = "Map", tabName = "map", icon = icon("globe")),
    menuItem(text = "Data Source", tabName = "data", icon = icon("database")),
    menuItem(text = "About", tabName = "about", icon = icon("user"))
  )
)

# Body Dashboard
body <- dashboardBody(
    # Tag CSS - Start ----------------------------------------------------------
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      # Tag CSS - End ----------------------------------------------------------
    ),
    # Tab Content - Start ------------------------------------------------------
    tabItems(
      # Overview Tab - Start ---------------------------------------------------
      tabItem(
        tabName = "overview",
        fluidPage(fluidRow(
          valueBoxOutput(width = 4, outputId = "overviewSalesGrowth"),
          valueBoxOutput(width = 4, outputId = "overviewProfitGrowth"),
          valueBoxOutput(width = 4, outputId = "overviewSalesSeason")
        ),
        fluidRow(
          box(width = 6,
            echarts4rOutput(outputId = "overviewProfitbyRegion")
          ),
          box(width = 6,
            echarts4rOutput(outputId = "overviewProfitbySegment")
          )
          
        )
        )
        # Overview Tab - End ---------------------------------------------------
      ),
      
      
      # Analysis1 Tab - Start --------------------------------------------------------
      tabItem(
        tabName = "analysis1",
        fluidPage(fluidRow(
         box(
           background = "black",
           width = 4,
           height = 80,
           selectInput(inputId = "stateSelector",
                       label = "Choose State",
                       choices = sort(unique(df_clean$State)),
                       selected = "New York")
         ),
         box(
           background = "black",
           width = 4,
           height = 80,
           dateRangeInput(inputId = "dateSelector",
                       label = "Choose Order Date",
                       start = "2014-01-01",
                       end = "2017-12-31")
         ), 
         box(
           background = "black",
           width = 4,
           height = 80,
           selectInput(inputId = "categorySelector",
                       label = "Choose Category",
                       choices = sort(unique(df_clean$Category)),
                       selected = "Furniture",
                       multiple = TRUE)
         )
        ),
        fluidRow(
          box(
            width = 6,
            echarts4rOutput(outputId = "profitSegment")
          ),
          box(
            width = 6,
            echarts4rOutput(outputId = "salesSegment")
          )
        ),
        fluidRow(
          box(
            width = 12,
            echarts4rOutput(outputId = "profitableSegment")
          )
        ),
        fluidRow(
          box(
            width = 12,
            echarts4rOutput(outputId = "shippingStats")
          )
        )
        )
        # Analysis1 Tab - End --------------------------------------------------
      ),
      
      
      # Analysis2 Tab - Start --------------------------------------------------
      tabItem(
        tabName = "analysis2",
        fluidPage(fluidRow(
          box(
            background = "black",
            width = 4,
            height = 80,
            selectInput(inputId = "stateSelector1",
                        label = "Choose State",
                        choices = sort(unique(df_clean$State)),
                        selected = "New York")
          ),
          box(
            background = "black",
            width = 4,
            height = 80,
            dateRangeInput(inputId = "dateSelector1",
                           label = "Choose Order Date",
                           start = "2014-01-01",
                           end = "2017-12-31")
          ), 
          box(
            background = "black",
            width = 4,
            height = 80,
            selectInput(inputId = "categorySelector1",
                        label = "Choose Category",
                        choices = sort(unique(df_clean$Category)),
                        selected = "Furniture",
                        multiple = TRUE)
          )
        ), # End fluidRow Input Analysis2
        
        fluidRow(
          box(
            width = 12,
            echarts4rOutput(outputId = "profitableSubCategory")
          )
        ),
        fluidRow(
          box(
            width = 12,
            echarts4rOutput(outputId = "missedprofitSubCategory")
          )
        )
        )
        # Analysis2 Tab - End --------------------------------------------------
      ),
      
      
      # Map Tab - Start --------------------------------------------------------
      tabItem(
        tabName = "map",
        fluidPage(fluidRow(
          box(
            background = "black",
            width = 4,
            height = 80,
            selectInput(
              inputId = "valueSelector",
              label = "Choose Value",
              choices = c("Number of transactions", "Profit", "Sales"),
              selected = "Profit")
            ),
          box(
            background = "black",
            width = 4,
            height = 80,
            selectInput(
              inputId = "categorycheckSelector",
              label = "Choose category",
              choices = sort(unique(df_clean$Category)),
              selected = "Furniture")
            ),
          box(
            background = "black",
            width = 4,
            height = 80,
            selectInput(
              inputId = "yearSelector",
              label = "Choose Year",
              choices = sort(unique(df_clean$Year)),
              selected = "2014")
          )
        ), # End fluidRow Input Map
        
        fluidRow(
          box(
            width = 20,
            height = "2160px",
            echarts4rOutput(outputId = "Mapping")
          )
        )
        )
        # Map Tab - End --------------------------------------------------------
      ),
      
      
      # Data Source Tab - Start ------------------------------------------------
      tabItem(tabName = "data",
              fluidRow(
                box(background = "purple",
                    width = 12,
                    title = h3("Superstore Giant Database", align = "center"),
                    DT::dataTableOutput(outputId = "database")
                    ),
              )
      
        # Data Source Tab - End ------------------------------------------------
      ),
      
      # About Tab- Start ------------------------------------------------------
      tabItem(
        tabName = "about",
        fluidPage(
          h1("Superstore Giant Dashboard"),
          p("By ", a("Muhammad Satrio Pinoto Negoro", href = "https://www.linkedin.com/in/muhammadsatriopinotonegoro/")),
          h2("Dataset"),
          h3("Context"),
          p("With growing demands and cut-throat competitions in the market, a Superstore Giant is seeking your knowledge in understanding what works best for them. They would like to understand which products, regions, categories and customer segments they should target or avoid. The Dataset published on", 
            a("Kaggle.", href = "https://www.kaggle.com/datasets/vivek468/superstore-dataset-final")),
          h3("Metadata"),
          p("Row ID => Unique ID for each row."),
          p("Order ID => Unique Order ID for each Customer."),
          p("Order Date => Order Date of the product."),
          p("Ship Date => Shipping Date of the Product."),
          p("Ship Mode=> Shipping Mode specified by the Customer."),
          p("Customer ID => Unique ID to identify each Customer."),
          p("Customer Name => Name of the Customer."),
          p("Segment => The segment where the Customer belongs."),
          p("Country => Country of residence of the Customer."),
          p("City => City of residence of of the Customer."),
          p("State => State of residence of the Customer."),
          p("Postal Code => Postal Code of every Customer."),
          p("Region => Region where the Customer belong."),
          p("Product ID => Unique ID of the Product."),
          p("Category => Category of the product ordered."),
          p("Sub-Category => Sub-Category of the product ordered."),
          p("Product Name => Name of the Product"),
          p("Sales => Sales of the Product."),
          p("Quantity => Quantity of the Product."),
          p("Discount => Discount provided."),
          p("Profit => Profit/Loss incurred."),
          h2("Code"),
          p("Find out my code in ", a("GitHub", href = "https://github.com/satriopino/Superstore-Giant-Visualization-with-ShinyDashboard"))
        )
        # About Tab - End ------------------------------------------------------
      )
      
    ) # End tabItems
  ) # End dashboardBody


# Arrange Page -----------------------------------------------------------------
ui <- dashboardPage(header, sidebar, body)