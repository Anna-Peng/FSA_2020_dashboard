########################################  UI subsubmodules  ########################################
socialModuleUIcontrol <- function(id) {
  ns <- NS(id)
  tagList(div(style = 'margin-top: 50px;') # space between filter panel and menu items
    ,div(style = 'width: auto; font-size: 90%; padding-left: 10px; padding-right: 10px; margin-right: 0px; margin-left: 0px; '
        ,h4("Filters")
        ,selectInput(inputId = ns('selCloudMultiName')
            ,label = 'Select 1 or more search terms'
            ,choices = ""
            ,multiple = TRUE
            ,selectize = FALSE
            ,size = 4
        )
        ,selectInput(inputId = ns("selCloudMultiLocation")
            ,label = "Choose 1 or more location"
            ,choices = ""
            ,multiple = TRUE
            ,selectize = FALSE
            ,size = 4
        )
        ,actionButton(ns("updateCloud"), "Update")
    )
  )
}
socialModuleUIpage <- function(id, IN_MD_SOCIAL) {
  ns <- NS(id)
  tagList(div(style = "max-width: 1200px; align: center;"
    ,fluidRow(box(width = 12
        ,includeMarkdown(IN_MD_SOCIAL)
    ))
    )
    ,fluidRow(box(width = 12
    ,column(3
        ,sliderInput(ns("selCloudMaxWords")
        ,"Maximum number of words"
        ,min = 1
        ,max = 200
        ,value = 80
        ,step = 10
        )
        ,h4(textOutput(ns("cloudInfo")), align = 'center')
    )
    ,column(9
        ,plotOutput(ns("cloudWordcloud"))
    )
    
    
    ))
    ,fluidRow(box(width = 12
    ,title = "Word frequency graph"
    ,fluidRow(column(3
        ,sliderInput(ns("selCloudMaxFreq")
        ,"Maximum number of words"
        ,min = 20
        ,max = 50
        ,value = 20
        ,step = 5
        ))
    )
    ,fluidRow(column(12
        ,plotOutput(ns("cloudFreq"))
    ))
    ,textOutput(ns("cloudOmitWords"))
    ))
  )
}
########################################  server subsubmodules  ########################################
socialModuleServer <- function(id, IN_OMITTED_WORDS, IN_FOOD_DICT, d.listings,  avail.platforms.social, r.socialpageopened) {moduleServer(id, function(input, output, session) {
    # input options
    r.selCloud <- reactiveValues(selCloudMultiName = NULL, selCloudMultiLocation = NULL)
    observeEvent(
        r.socialpageopened(),
        {
            updateSelectInput(session, "selCloudMultiName", choices = r.search_terms(), selected = r.search_terms()[[1]])
            updateSelectInput(session, "selCloudMultiLocation", choices = r.location_terms(), selected = r.location_terms())
            r.selCloud$selCloudMultiName <<- r.search_terms()[[1]]
            r.selCloud$selCloudMultiLocation <<- r.location_terms()
        },
        once = TRUE, priority = 100)
    observeEvent(
        input$updateCloud,{
            r.selCloud$selCloudMultiLocation <<- input$selCloudMultiLocation
            r.selCloud$selCloudMultiName <<- input$selCloudMultiName
    })
    ############## Wordcloud preparation ##############
    r.listings.social <- reactive({
        ids <- avail.platforms.social()$PlatformID
        d.listings %>%
            filter(PlatformID %in% ids) %>%
            distinct()
    })
    r.search_terms <- reactive({
        search_terms <- r.listings.social() %>%
            select(ScrapingSearch) %>%
            distinct() %>%
            collect() %$%
            ScrapingSearch
        search_terms
    })
    r.location_terms <- reactive({
        location_terms <- r.listings.social() %>%
            select(PlatformBusinessAddress) %>%
            distinct() %>%
            collect() %$%
            PlatformBusinessAddress
        location_terms
    })
    r.rm_words <- reactive({
        r.search_terms() %>%
            strsplit('\\s+') %>%
            {c(r.omitted_words(),unlist(.))} %>%
            unique()
    })

    r.listings.social.multiname <- reactive({
        req(r.selCloud$selCloudMultiName)
        selCloudMultiName <- r.selCloud$selCloudMultiName
        listings.social.multiname <- r.listings.social() %>%
            filter(ScrapingSearch %in% selCloudMultiName)
        listings.social.multiname
    })
    r.terms <- reactive({
        selCloudMultiLocation <- r.selCloud$selCloudMultiLocation
        withProgress({
            setProgress(message = "Processing corpus...")
            # Get the filtered listings
            listings.social.multiname.location <- 
                r.listings.social.multiname() %>%
                select(PlatformID, PlatformBusinessName, PlatformBusinessAddress, PlatformDescription, ScrapingSearch) %>%
                filter(PlatformBusinessAddress %in% selCloudMultiLocation) %>%
                collect() %>%
                mutate(row_text = paste(PlatformBusinessName, PlatformDescription, sep = " "))
            # Flatten whole text into a cell
            flat_text <- listings.social.multiname.location$row_text 
            # Vectorise text
            termmatrix <- getTermMatrix(text = flat_text, food_dict = r.food_dict(), rm_words = r.rm_words())
        })
        return(termmatrix)
    })

    
        ######################################## Word cloud  ########################################
    r.food_dict <- reactive({
        dict <- read.csv(IN_FOOD_DICT, header = FALSE)
        as.character(dict[,1])
    })
    r.omitted_words <- reactive({
        omitted <- read.csv(IN_OMITTED_WORDS, header = FALSE)
        as.character(omitted[,1])  
    })
    ######################################## Word cloud  ########################################
    # wordcloud with reactive filters (AP)
    output$cloudWordcloud <- renderPlot({ 
        req(r.terms())
        v <- r.terms()
        #corpus_size <- length(v)
        maxwords <- input$selCloudMaxWords
        words.freq <- v[[1]]
        colors <- v[[2]]
        wordcloud(names(words.freq), words.freq, scale=c(3.5,0.2), colors = colors,
                  ordered.colors = TRUE, random.order = FALSE, max.words = 80)
    })
    
    #Frequency plot with maxFreq slider 
    output$cloudFreq <- renderPlot({
        v <- r.terms()
        words.freq <- v[[1]]
        df <- stack(words.freq)
        maxfreq = input$selCloudMaxFreq
        ggplot(data=df[1:maxfreq,]) +
            geom_bar(aes(x=ind,y=values),stat="identity", fill = "#0f97bd", width = 0.5) +
            xlab("") + ylab("Count") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
                    axis.title=element_text(size=20))
    })
    
    # Text to output corpus size on top of wordcloud
    output$cloudInfo <- renderText({
        v <- r.terms()
        words.freq <- v[[1]]
        corpus_size <- length(words.freq)
        sprintf("Corpus Size: %i words", corpus_size)
    })
    
    # Text to output removed words below frequency graph
    output$cloudOmitWords <- renderText({
        sprintf("Manually Removed Words: %s", paste(sort(c(r.rm_words())), collapse=', ' ))
    })

})}
