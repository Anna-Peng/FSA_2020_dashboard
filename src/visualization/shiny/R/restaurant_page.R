########################################  UI subsubmodules  ########################################
restaurantModuleUIcontrol <- function(id) {
  ns <- NS(id)
  tagList(div(style = 'margin-top: 50px;') # space between filter panel and menu items
    ,div(style = 'width: auto; font-size: 90%; padding-left: 10px; padding-right: 10px; margin-right: 0px; margin-left: 0px;'
      ,h4("Filters")
      ,sliderInput(ns("selRating"), "FHRS value", ticks = FALSE, min = 0, max = 5, value = c(0,5), step = 1) 
      ,selectInput(ns("selPlatforms"), "Select platform(s)", choices ="", multiple = TRUE, selectize = TRUE)
      ,textInput(ns("selAuthorithy"), "Local Authorithy Number/Code", value = "")
      ,checkboxInput(ns("selIncludeUnidentified"), "Include unidentified establishments", value = FALSE) 
      ,checkboxInput(ns("selIncludeUnrated"), "Include unrated establishments", value = FALSE) 
    )
  )
}

restaurantModuleUIpagePlots <- function(id) {
    ns <- NS(id)
    tagList(div(style = "max-width: 1200px; align: center;"
        ,fluidRow(box(width = 12
            ,title = "Summary"
            ,collapsible = TRUE
            ,fluidRow(style = "margin-left: 10px; margin-right: 10px;"
            ,column(width = 3
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoNumRes"), width = "auto")))
                ,div(style = "margin-left:5px;margin-right:5px;", fluidRow(infoBoxOutput(ns("infoNumPlats"), width = 'auto')))
            )
            ,column(width = 3
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoNumLAs"), width = 'auto')))
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoPercBad"), width = 'auto')))
            )
            ,column(width = 3
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoDateRecent"), width = 'auto')))
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoDateOldest"), width = 'auto')))
            )
            ,column(width = 3
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoPercUnidentified"), width = 'auto')))
                ,div(style = "margin-left:5px;margin-right:5px;",fluidRow(infoBoxOutput(ns("infoPercUnrated"), width = 'auto')))
            )
            )
        ))
        ,fluidRow(column(width =5
            ,div(style = "max-width: 500px;", plotOutput(ns("plRatingsDist"), height = "300px"))
            )
            ,column(width =7
                ,div(style = "max-width: 700px;",plotOutput(ns("plRatingsDates"), height = "300px"))
        ))
    ))
}
restaurantModuleUIpage <- function(id, IN_MD_RESTAURANTS) {
    ns <- NS(id)
    tagList(div(style = "max-width: 1200px; align: center;"
            ,fluidRow(box(width = 12
                ,includeMarkdown(IN_MD_RESTAURANTS)
                ))
            ,restaurantModuleUIpagePlots(ns("restaurantsPlots"))
        )
        ,fluidRow(box(width = 12
            ,div(class = "widetable"
            ,dataTableOutput(ns('restaurantlist'))
            )
        ))
    )
}
########################################  Server submodule  ########################################
restaurantModuleServer <- function(id, avail.platforms.res, d.est, d.listings, d.plat, r.restaurantspageopened) {moduleServer(id,function(input, output, session) {
    ############## Populate the selection box ##############
    observeEvent(
        r.restaurantspageopened(),
        updateSelectInput(session, "selPlatforms", choices = avail.platforms.res()$PlatformName, selected = avail.platforms.res()$PlatformName)
    )

    ############## restaurants that are on the page ##############
    r.sel.res.platforms <- reactive({
        if(is.null(input$selPlatforms)) {
            res <- avail.platforms.res()
        } else {
            res <- filter(avail.platforms.res(), PlatformName %in% input$selPlatforms)
        }
        res
    })
    # establishment filter 1
    r.est.rated <- reactive({
        # include or exclude unindentified businesses
        res <- if(input$selIncludeUnidentified){
            res <- d.est
        } else{
            res <- d.est %>% filter(!is.na(FHRSID))
        }
        return(res)
    })
    # establishment filter 2
    r.est.rated.filt <- reactive({
        ratings = paste(input$selRating[[1]]: input$selRating[[2]])
        res <- if(input$selIncludeUnrated){
            r.est.rated() %>% 
                filter(RatingValue %in% ratings | RatingValue %not like% "_" | is.na(RatingValue))
        } else{
            r.est.rated() %>% 
                filter(RatingValue %in% ratings | is.na(RatingValue))
        }
        return(res)
    })
    # establishment filter 3
    r.input.selAuthorithy <- reactive(input$selAuthorithy) %>% debounce(1000)
    r.est.rated.filt.LA <- reactive({
        localauthtext = r.input.selAuthorithy()
        localauthtnum = suppressWarnings(as.numeric(localauthtext))
        if(localauthtext != ""){
            if(is.na(localauthtnum)){
                localauthtext2 = str_c("%", localauthtext, "%")
                res <- r.est.rated.filt() %>%
                    filter(LocalAuthorityName %like% localauthtext2)
            } else {
                res <- r.est.rated.filt() %>%
                    filter(LocalAuthorityCode == localauthtnum)
            }
        } else {
            res <- r.est.rated.filt()
        }
        return(res)
    })
    # establishment filter 4 + clean up table
    r.restaurantlist <- reactive({
        platforms.selected <- r.sel.res.platforms()$PlatformName
        res <- r.est.rated.filt.LA() %>%
            inner_join(select(d.listings, EstablishmentID, PlatformID), by = "EstablishmentID") %>% 
            inner_join(select(d.plat, PlatformID, PlatformName), by = "PlatformID") %>%
            filter(PlatformName %in% platforms.selected) %>% 
            collect() %>%
            group_by(EstablishmentID) %>%
            summarise_all(~str_c(unique(.), collapse = ", ")) %>%
            ungroup()%>%
            select(-EstablishmentID, -PlatformID, -BusinessTypeID) %>%
            mutate(RatingDate = ifelse(ymd(ymd_hms(RatingDate)) == "1901-01-01", NA, RatingDate),
                    RatingDate = ymd(ymd_hms(RatingDate)))  
        res
    })
    # output table
    output$restaurantlist <- renderDataTable({
        r.restaurantlist() %>%
        select(BusinessName, BusinessAddress, FHRSID, LocalAuthorityName,
            LocalAuthorityCode, RatingValue, RatingDate) %>%
        rename_all(~ gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', .)) %>%
            datatable(
                selection = "none", 
                rownames = FALSE,
                extensions = 'Buttons', 
                options = list(pageLength = 50,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )) 
    })
    ############## top of the page plots submodule ##############
    restaurantModuleServerPlots("restaurantsPlots", d.est, d.listings,  r.sel.res.platforms, r.restaurantlist)
})}



########################################  Server sub-submodule  ########################################
restaurantModuleServerPlots <- function(id, d.est, d.listings,  r.sel.res.platforms, r.restaurantlist) {moduleServer(id,function(input, output, session) {
    
    # reactive to calc percentages of unidentified etc for the boxes at top
    r.percboxes <- reactiveValues(infoPercUnidentified = 0, infoPercUnrated = 0)
    observe({
        platformids.selected <- r.sel.res.platforms()$PlatformID
        dat <- d.est %>%
            inner_join(select(d.listings, EstablishmentID, PlatformID), by = "EstablishmentID") %>% 
            filter(PlatformID %in% platformids.selected ) %>% 
            collect() %>%
            summarise(n = n(),
                infoPercUnidentified = sum(is.na(RatingValue)) / n * 100,
                percRated = sum(RatingValue %in% paste(0:5)) / n * 100,
                infoPercUnrated = 100 - infoPercUnidentified - percRated)
        r.percboxes$infoPercUnrated <<- dat$infoPercUnrated
        r.percboxes$infoPercUnidentified <<- dat$infoPercUnidentified
    })

    output$infoNumRes <- renderInfoBox({
        num <- r.restaurantlist() %>%
            summarise(num = n()) %>%
            collect() %$%
            num
        infoBox(
            "Food business operators",
            sprintf("%i FBOs", num),
            icon = icon("hashtag", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoNumPlats <- renderInfoBox({
        num <- nrow(r.sel.res.platforms())
        infoBox(
            "platforms",
            sprintf("%i Platforms", num),
            icon = icon("hashtag", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoNumLAs <- renderInfoBox({
        num <- r.restaurantlist() %>%
            group_by(LocalAuthorityCode) %>%
            summarise(num = 1) %>%
            collect() %$%
            num %>%
            sum
        infoBox(
            "local authorities",
            sprintf("%i LAs", num),
            icon = icon("hashtag", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoPercBad <- renderInfoBox({
        num <- r.restaurantlist() %>%
            select(RatingValue) %>%
            filter(str_detect(RatingValue, "[0-5]")) %>%
            collect() %$%
            {sum(RatingValue < 3)/length(RatingValue)* 100}
        infoBox(
            "Low rating (<3)",
            sprintf("%.1f%%", num),
            icon = icon("percentage", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoDateRecent <- renderInfoBox({
        num <- r.restaurantlist() %>%
            arrange(desc(RatingDate)) %>%
            head(1) %>%
            collect() %$%
            RatingDate
        infoBox(
            "Most recent rating",
            paste(num),
            icon = icon("calendar", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoDateOldest <- renderInfoBox({
        num <- r.restaurantlist() %>%
            arrange(RatingDate) %>%
            head(1) %>%
            collect() %$%
            RatingDate 
        infoBox(
            "Oldest rating",
            paste(num),
            icon = icon("calendar", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoPercUnidentified <- renderInfoBox({
        infoBox(
            "Unidentified",
            sprintf("%.1f%%", r.percboxes$infoPercUnidentified),
            icon = icon("percentage", lib = "font-awesome"),
            fill = TRUE
        )
    })
    output$infoPercUnrated <- renderInfoBox({
        infoBox(
            "Unrated",
            sprintf("%.1f%%", r.percboxes$infoPercUnrated),
            icon = icon("percentage", lib = "font-awesome"),
            fill = TRUE
        )
    })

    output$plRatingsDist <- renderPlot({
        renameF <- function(x) {
            gsub('([[:upper:]])', '\n\\1', x) %>% 
            str_trim
        }
        dat = r.restaurantlist() %>%
            select(RatingValue) %>%
            mutate(RatingValue = ifelse(is.na(RatingValue), "Unidentified", RatingValue))%>%
            mutate(RatingValue = renameF(RatingValue))

        pl <- dat %>%
            ggplot(aes(RatingValue)) +
            geom_bar(fill = "#20c1ed") +
            theme_classic(base_size = 16) +
            ylab("No. of establishments")+
            xlab("FHRS value") +
            ggtitle("FHRS distribution") +
            coord_flip()

        return(pl)
    })
    output$plRatingsDates <- renderPlot({
        dat <- r.restaurantlist() %>%
        select(RatingDate, RatingValue) %>%
        mutate(RatingValue = ifelse(is.na(RatingValue), "Unidentified", RatingValue))
        dat.mn <- dat %>%
            group_by(RatingValue) %>%
            summarise(RatingDate = median(RatingDate)) %>%
            mutate(y=as.numeric(as.factor(unique(RatingValue))),
                ymin = y-.5, yend = y+.5)
        # Older than 5 years is clipped to 5 years ago (but not in median calculation above)
        dat %>%
        mutate(now = Sys.time() %>% paste %>% ymd_hms %>% as_date,
            RatingDate = ifelse(RatingDate < (now - years(5)), paste(now - years(5)), paste(RatingDate)) %>% ymd)  %>%
        ggplot(aes(RatingDate, RatingValue)) +
            geom_violin(fill = "#20c1ed") +
            geom_jitter(size = .8, alpha = .6) +
            geom_segment(aes(x = RatingDate, xend = RatingDate, y = ymin, yend =yend), color = "red", size =1.5, data = dat.mn)+
            theme_classic(base_size = 16) +
            ylab("FHRS value")+
            xlab("Last inspection date") +
            ggtitle("Last inspection")
    })  
})}
