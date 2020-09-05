########################################  UI subsubmodules  ########################################
ecosystemModuleUIcontrolSchematic <- function(id) {
    ns <- NS(id)
    tagList(div(style = 'margin-top: 50px;') # space between filter panel and menu items
        ,div(style = 'width: auto; font-size: 90%; padding-left: 10px; padding-right: 10px; margin-right: 0px; margin-left: 0px; '
            ,h4("Filters")
            ,radioButtons(ns("selEcoSimple"),NULL, choices ="")
            ,conditionalPanel(condition = "['Examples'].includes(input.selEcoSimple)"
                ,ns = ns
                ,selectInput(ns("selEcoExample"), "Select ecosystem path", choices ="", multiple = FALSE, selectize = TRUE)
            )   
        )
    )
}
ecosystemModuleUIcontrolLists <- function(id) {
    ns <- NS(id)
    tagList(div(style = 'margin-top: 50px;') # space between filter panel and menu items
        ,div(style = 'width: auto; font-size: 90%; padding-left: 10px; padding-right: 10px; margin-right: 0px; margin-left: 0px; '
            ,h4("Filters")
            ,radioButtons(ns("selByNode"), "Filterset", choices = c(`Typology` = "bytype", `Ecosystem node` = "bynode"), inline = T)
            ,conditionalPanel(condition = "['bytype'].includes(input.selByNode)"
                ,ns = ns
                ,selectInput(ns("selFoodtype"), "Food types", choices ="", multiple = TRUE, selectize = TRUE)
                ,selectInput(ns("selServicetype"), "Service types", choices ="", multiple = TRUE, selectize = TRUE)
                ,selectInput(ns("selBusinessmodeltype"), "Business Model types", choices ="", multiple = TRUE, selectize = TRUE)
                ,checkboxInput(ns("selPlatispresent"), "Show only analysed platform types", value = FALSE) 
            )
            ,conditionalPanel(condition = "['bynode'].includes(input.selByNode)"
                ,ns = ns
                ,selectInput(ns("selNode"), "Ecosystem node", choices ="", multiple = TRUE, selectize = TRUE)
            )
        )
    )
}
ecosystemModuleUIpageTypology <- function(id, IN_MD_TYPOLOGY) {
    ns <- NS(id)
    tagList(div(style = "max-width: 1200px; align: center;"
            ,fluidRow(box(width = 12
            ,includeMarkdown(IN_MD_TYPOLOGY)
            ))
        )
        ,fluidRow(box(width = 12
            ,div(class = "widetable"
                ,DT::dataTableOutput(ns('platformtypologylist'))
                ,conditionalPanel(condition = "input.selPlatispresent"
                  ,ns = ns
                  ,tags$small("*NB some platforms have multiple platform types and vice versa")
              )
            )
        ))
    )
}
ecosystemModuleUIpagePlatforms <- function(id, IN_MD_PLATFORMS) {
    ns <- NS(id)
    tagList(div(style = "max-width: 1200px; align: center;"
        ,fluidRow(box(width = 12
            ,includeMarkdown(IN_MD_PLATFORMS)
            ))
        )
        ,fluidRow(box(width = 12
            ,div(class = "widetable"
                ,DT::dataTableOutput(ns('platformlist'))
                ,downloadButton(ns("downloadPlatformsList"), "CSV")
            )
        ))
    )
}
ecosystemModuleUIpageEcosystem <- function(id, IN_MD_ECOSYSTEM) {
    ns <- NS(id)
    tagList(div(style = "max-width: 1200px; align: center;"
        ,fluidRow(box(width =12
            ,includeMarkdown(IN_MD_ECOSYSTEM)
        ))
      )
        ,div(style = "max-width: 1200px; align: center;"
            ,fluidRow(box(width = 12
                ,div(class = "ecoWrapper"
                    ,div(class = "eco"
                        ,div(class = "flexbox-centering"
                            ,visNetworkOutput(ns("ecoNetwork"), width = "100%", height = "100%")
                        )
                    )
                )
            ))
        )
        ,fluidRow(box(width = 6
                ,title = "Platformtype information"
                ,htmlOutput(ns("selNodeID"))
                ,conditionalPanel(condition = '!isNaN(input.current_node_id)'
                    ,ns = ns
                    ,actionButton(ns("viewEcoTypes"), "View in platformTypes list")
                    ,DT::dataTableOutput(ns('ecosysTypologies'))
                )
            )
            ,box(width = 6
                ,title = "Platform information"
                ,conditionalPanel(condition = '!isNaN(input.current_node_id)'
                    ,ns = ns
                    ,textOutput(ns("ecoPlatTypes"))
                    ,actionButton(ns("viewEcoPlats"), "View in platform list")
                    ,DT::dataTableOutput(ns('ecosysPlatforms'))
                )
            )
        )
    )
}
########################################  server subsubmodules  ########################################
ecosystemModuleServer <- function(id, IN_ECO_NODES, IN_ECO_EDGES, IN_ECO_SPECS, d.listings, d.platLink, d.plat, d.platTyp, r.ecopageopened, r.listpageopened, avail.platforms, parent) {moduleServer(id,function(input, output, session) {
    ############## Populate the input boxes ##############
    # ecosystem page input boxes
    avail.nodes <- reactive({
        d.platTyp %>%
            select(EcosystemNode) %>%
            collect() %$%
            EcosystemNode
    })
    r.eco <- reactiveValues(nodes = NULL, edges = NULL, specs = NULL)
    observeEvent(
            r.ecopageopened(),# Load the ecosystem info once the page is opened and fill in the input buttons
        {
            r.eco$nodes <<- read_tsv(IN_ECO_NODES, col_types = cols()) %>%
                mutate(id = 1:n())
            r.eco$edges <<- read_tsv(IN_ECO_EDGES, col_types = cols())%>%
                mutate(id = 1:n())
            r.eco$specs <<- read_tsv(IN_ECO_SPECS, col_types = cols())
            complexities <- r.eco$specs %>%
                filter(!str_detect(ecosys_dropdown, "eg_")) %$%
                set_names(ecosys_dropdown, ecosys_dropdown_title)
            examples <- r.eco$specs %>%
                filter(!(ecosys_dropdown %in% complexities))  %$%
                set_names(ecosys_dropdown, ecosys_dropdown_title)
            updateSelectInput(session, "selNode", choices = sort(avail.nodes()))
            updateRadioButtons(session, "selEcoSimple",     choices = c(complexities, Examples = "Examples"))
            updateSelectInput(session, "selEcoExample", choices = examples)
        },
        once = TRUE, priority = 100
    )
    # List pages input boxes
    avail.foodtypes <- reactive({
        req(c(r.listpageopened(), r.ecopageopened()))
        d.platTyp %>%
            collect() %$% 
            FoodType %>% 
            unique()
    })
    avail.servicetypes <- reactive({
        req(r.listpageopened())
        d.platTyp %>%
            collect() %$% 
            ServiceType %>% 
            unique()
    })
    
    avail.businessmodeltypes <- reactive({
        req(r.listpageopened())
        d.platTyp %>%
            collect() %$% 
            BusinessModelType %>% 
            unique()
    })
    observeEvent(
        r.listpageopened(),
        {
            updateSelectInput(session, "selFoodtype", choices = avail.foodtypes())
            updateSelectInput(session, "selServicetype", choices = avail.servicetypes())
            updateSelectInput(session, "selBusinessmodeltype", choices = avail.businessmodeltypes()) 
            
        },
        once = TRUE, priority = 100
    )
    ############## Apply filter buttons (ecosystem page) ##############
    r.econodeselected <- reactive({
        if(is.null(input$current_node_id)){
            "deselected"
        } else{
            input$current_node_id
        }
    }) %>% debounce(50) # so going from one node to other it doesn't switch to "deselected in between"
    observeEvent(input$viewEcoPlats,{
        updateRadioButtons(session, "selByNode", selected = "bynode")
        updateSelectInput(session, "selNode", selected = r.econodeselected())
        updateTabItems(parent, inputId = "tabs", selected = "platforms") 
    })
    observeEvent(input$viewEcoTypes,{
        updateRadioButtons(session, "selByNode", selected = "bynode")
        updateSelectInput(session, "selNode", selected = r.econodeselected())
        updateTabItems(parent, inputId = "tabs", selected = "typology") 
    })

    ############## ecosystem ##############
    r.selEcoPath <- reactive({
        req(input$selEcoSimple)
        req(input$selEcoExample)
        if(input$selEcoSimple == "Examples"){
            input$selEcoExample
        } else{
            input$selEcoSimple
        }
    })
    # update network depending on dropdown
    observeEvent(r.selEcoPath(), {
        # nodes and edges df have columns that are specified in specs.sel, overwrite visnetwork column with
        # the specs.sel specified columns
        # Example:
        # !!(specs.sel$nodecolor) takes the value of "specs.sel$nodecolor" (="ff_int_mcdonalds_color")
        # and overwrites color column with the ff_int_mcdonalds_title column
        specs.sel  <- r.eco$specs %>% 
            filter(ecosys_dropdown == r.selEcoPath())
        nodes.sel <- r.eco$nodes %>%
            rename(
                font.color = !!(specs.sel$fontcolor),
                title = !!(specs.sel$title),
                tmp = !!(specs.sel$nodecolor)) %>%
            mutate(tmp2 = defineColor(tmp, r.eco$nodes$color),
                color = lapply(tmp2, function(x) {list(background = x, border = x)}),
                hidden =   is.na(tmp),
                font.size = 24,
                font.background = "white")
        edges.sel <- r.eco$edges %>%
            rename(color = !!(specs.sel$edgecolor),
                width = !!(specs.sel$width)) %>%
            mutate(hidden = color == "white")
        ns <- session$ns
        visNetworkProxy(ns("ecoNetwork")) %>%
            visRemoveNodes(filter(nodes.sel, hidden)$id) %>%
            visUpdateNodes(nodes.sel)  %>%
            visRemoveEdges(filter(edges.sel, hidden)$id) %>%
            visUpdateEdges(edges.sel) %>%
            visFit( animation =  list(duration = 500, easingFunction = "easeInOutQuad"))
            
    })
    r.platlist.ecosystem <- reactiveValues(econode.platforms = NULL, econode.platformtypes = NULL)
    observe({
        selectednodes <- r.econodeselected()
        r.platlist.ecosystem$econode.platformtypes <<- r.platTyp.anno() %>%
            filter(EcosystemNode %in% selectednodes)
          # Get the correct filter for types
        PlatformID.selected = r.platlist.ecosystem$econode.platformtypes %>%
            select(PlatformTypeID) %>%
            inner_join(d.platLink, by = "PlatformTypeID") %>% 
            inner_join(d.plat, by = "PlatformID") %>%
            select(PlatformID) %>%
            collect()
        r.platlist.ecosystem$econode.platforms <<- r.plat.anno() %>%
            semi_join(PlatformID.selected, by = "PlatformID")
    })

    ##############  Platform and platformtype  ##############
    #
    r.plat.anno <- reactive({
      d.plat %>%
        left_join(d.platLink, by = "PlatformID") %>%
        left_join(d.platTyp, by = "PlatformTypeID") %>%
        collect() %>%
        mutate(PlatformType = str_c(FoodType, ServiceType, BusinessModelType, sep = " -> ")) %>%
        group_by(PlatformID) %>% 
        summarise_all( ~str_c(unique(.), collapse = ",<br>")) %>%
        # AMC to add extra info on Platforms change for: summarise_at(vars(PlatformName, PlatformURL, PlatformType, BusinessNameAvailable, BusinessAddressAvailable, FHRSShown, FHRSRequiredOnRegistration, ScrapingRestrictions, ScrapingRestrictionsURL, OfficialAPIURL, RobotsURL, RobotsAllowsSearch, Javascript, LastUpdated, LastUpdatedBy), ~str_c(unique(.), collapse = ",<br>")) %>%
        ungroup() 
    })
    r.platTyp.anno <- reactive({
        d.platLink %>%
          semi_join(d.listings, by = "PlatformID") %>%
          group_by(PlatformTypeID) %>%
          summarise(NumberOfPlatformsAnalysed = n()) %>%
          {left_join(d.platTyp, ., by = "PlatformTypeID")} %>%
          mutate(NumberOfPlatformsAnalysed = ifelse(is.na(NumberOfPlatformsAnalysed), 0, NumberOfPlatformsAnalysed)) 
    })
    
    # contains the Platform and platformtype lists, once for bytype and once for bynode
    r.platlist.lists <- reactiveValues(bytype.platforms = NULL,bytype.platformtypes = NULL, 
        bynode.platforms = NULL, bynode.platformtypes = NULL)
    # bytype page
    observe({   
        req(avail.foodtypes())
        req(avail.servicetypes())
        req(avail.businessmodeltypes())
        foods =  if(is.null(input$selFoodtype)) {avail.foodtypes()} else {input$selFoodtype}
        services =  if(is.null(input$selServicetype)) {avail.servicetypes()} else {input$selServicetype}
        businessmodels =  if(is.null(input$selBusinessmodeltype)) {avail.businessmodeltypes()} else {input$selBusinessmodeltype}
        onlyanalysed = input$selPlatispresent
        r.platlist.lists$bytype.platformtypes <<- r.platTyp.anno() %>%
            filter(
                FoodType %in% foods,
                ServiceType %in% services,
                BusinessModelType %in% businessmodels
                ) %>% 
            {if(onlyanalysed) filter(., NumberOfPlatformsAnalysed > 0) else .} 
        # Get the correct filter for types
        PlatformID.selected = r.platlist.lists$bytype.platformtypes %>%
            select(PlatformTypeID) %>%
            inner_join(d.platLink, by = "PlatformTypeID") %>% 
            inner_join(d.plat, by = "PlatformID") %>%
            select(PlatformID) %>%
            collect()
        r.platlist.lists$bytype.platforms <<- r.plat.anno() %>%
            {if(onlyanalysed) filter(., PlatformID %in% avail.platforms()$PlatformID) else .} %>%
            semi_join(PlatformID.selected, by = "PlatformID")
    })
    # bynode page
    observe({
        selectednodes <- if(length(input$selNode) == 0 ) {
            avail.nodes()
        } else {
            input$selNode
        }
        r.platlist.lists$bynode.platformtypes <<- r.platTyp.anno() %>%
                filter(EcosystemNode %in% selectednodes)
          # Get the correct filter for types
        PlatformID.selected = r.platlist.lists$bynode.platformtypes %>%
            select(PlatformTypeID) %>%
            inner_join(d.platLink, by = "PlatformTypeID") %>% 
            inner_join(d.plat, by = "PlatformID") %>%
            select(PlatformID) %>%
            collect()
        r.platlist.lists$bynode.platforms <<- r.plat.anno() %>%
             semi_join(PlatformID.selected, by = "PlatformID")
    })

    ##############  Outputs of all three pages  ##############
    ## ecosystem outputs ##
    # Empty network at first
    output$ecoNetwork <- renderVisNetwork({
        # Note the update of this output elsewere
        visNetwork(mutate(r.eco$nodes, hidden = T), mutate(r.eco$edges, hidden = T)) %>%  
            visNodes(fixed = TRUE, borderWidth = 2, borderWidthSelected = 4) %>%
            visPhysics(stabilization = FALSE) %>%
            visEvents(type = "once", startStabilizing = "function() {
                this.moveTo({scale:.9})}") %>%
            visInteraction(dragNodes = FALSE, 
                dragView = FALSE, 
                zoomView = FALSE) %>%
            visEvents(selectNode = sprintf("function(nodes) {
                    Shiny.setInputValue('%s-current_node_id', nodes.nodes);
                ;}", id),
                deselectNode = sprintf("function(nodes) {
                    Shiny.setInputValue('%s-current_node_id', NaN, {priority: 'event'});
                ;}", id)
            )
    })
    output$selNodeID <- renderUI({
        if(r.econodeselected() == "deselected"){
            "Select a box above for more information"
        } else{
            n = tally(r.platlist.ecosystem$econode.platformtypes) %>%
              collect() %$% n
            sprintf("Selected node ID = %s<br/>%i PlatformTypes annotated for this node", 
                r.econodeselected(), n) %>%
            HTML
        }
    })
    output$ecoPlatTypes <- renderText({
        sprintf("%i platforms annotated for this node", nrow(r.platlist.ecosystem$econode.platforms)) 
    })
    output$ecosysTypologies <- renderDataTable({
        r.platlist.ecosystem$econode.platformtypes %>%
            select(FoodType, ServiceType, BusinessModelType) %>%        
            collect()  %>%
            rename_all(~ gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', .)) %>%
            datatable(
                selection = "none", 
                rownames = FALSE,
                options = list(
                    paging = FALSE,
                    searching = FALSE
                )) %>%
            formatStyle(
                "Food Type",
                target = "row",
                backgroundColor =  styleEqual(avail.foodtypes() , c("#E5E2DC", "#EFD4DA", "#CFC8CE"))
            )   
    })
    output$ecosysPlatforms <- renderDataTable({
        r.platlist.ecosystem$econode.platforms %>%
            select(PlatformName) %>%        
            rename_all(~ gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', .)) %>%
            datatable(
                selection = "none", 
                rownames = FALSE,
                options = list(
                    paging = FALSE,
                    searching = FALSE
                ))
    })
    ## big list outputs ## 
    output$platformtypologylist <- renderDT({
        if(input$selByNode == "bytype") {
            req(r.platlist.lists$bytype.platformtypes)
            platlist <- r.platlist.lists$bytype.platformtypes
        } else {
            req(r.platlist.lists$bynode.platformtypes)
            platlist <- r.platlist.lists$bynode.platformtypes
        }
        platlist %>%
        select(-PlatformTypeID, -EcosystemNode) %>%
        as.data.frame() %>% 
        rename_all(~ gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', .)) %>%
            datatable(
                selection = "none", 
                rownames = FALSE,
                extensions = 'Buttons', 
                options = list(pageLength = 50,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                )) %>%
            formatStyle(
                "Food Type",
                target = "row",
                backgroundColor =  styleEqual(avail.foodtypes(), c("#E5E2DC", "#EFD4DA", "#CFC8CE"))
            )   
    }) 
    output$platformlist <- renderDT({
        {if(input$selByNode == "bytype") {r.platlist.lists$bytype.platforms} else {r.platlist.lists$bynode.platforms}} %>%
            select(PlatformName, PlatformURL, PlatformType) %>% 
            as.data.frame() %>% 
            mutate(PlatformURL = generateHref(PlatformURL)) %>%
            mutate(MoreInfo = buttonInput(
                FUN = actionButton,
                len = n(),
                id = 'platformrow_',
                label = "More Info",
                onclick = sprintf('Shiny.setInputValue(\"%s-moreInfoPlatform\",  this.id, {priority: \"event\"})', id)
            )) %>%
            select(PlatformName, PlatformURL, PlatformType, MoreInfo) %>%
            rename_all(~ gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', .)) %>%
            datatable(
                selection = "none",
                escape = FALSE,
                rownames = FALSE,
                options = list(pageLength = 50)) 
    }) 
     output$downloadPlatformsList <- downloadHandler(
        filename = function() {"Platforms_list.csv"},
        content = function(file) {
            {if(input$selByNode == "bytype") {r.platlist.lists$bytype.platforms} else {r.platlist.lists$bynode.platforms}} %>%
                write_csv(file)
        }
    )
    ############## Modal pop up ##############
    observeEvent(input$moreInfoPlatform, {
        rown <- str_extract(input$moreInfoPlatform, "[:digit:]+") %>% as.numeric()
        row <- r.plat.anno() %>%
        {if(input$selByNode == "bytype") {semi_join(., r.platlist.lists$bytype.platforms, by = "PlatformID")} else {semi_join(., r.platlist.lists$bynode.platforms, by = "PlatformID")}} %>%
            slice(rown)
        row %$%
            showModal(modalDialog(
                h2(PlatformName),
                # "Platform URL:",
                tags$a(href = PlatformURL, PlatformURL), br(),
                # AMC to add extra info on Platforms uncomment: 
                "Business Name Available:",
                BusinessNameAvailable, br(),
                "Business Address Available:",
                BusinessAddressAvailable, br(),
                "FHRS Shown:",
                FHRSShown, br(),
                "FHRS Required On Registration:",
                FHRSRequiredOnRegistration, br(),
                "Scraping Restrictions:",
                ScrapingRestrictions, br(),
                "Scraping Restrictions URL:",
                ScrapingRestrictionsURL, br(),
                "Official API URL:",
                OfficialAPIURL, br(),
                "Robots URL:",
                tags$a(href = RobotsURL, RobotsURL), br(),
                "Robots Allows Search:",
                RobotsAllowsSearch, br(),
                "Javascript:",
                Javascript, br(),
                "Last Updated:",
                LastUpdated, br(),
                "Last Updated By:",
                LastUpdatedBy, br(),
                easyClose = TRUE,
                footer = modalButton("Close")
            ))
    })
})}
