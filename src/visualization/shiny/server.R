
function(input, output, session) {
    ########################################## Database ##########################################
    ############## open sql connection ##############
    con <- dbConnect(RSQLite::SQLite(), IN_DB, flags = SQLITE_RO)
    onStop(function() {
      dbDisconnect(con)
    })
    # assign tables to variables
    # print(dbListTables(con)) # lists tables
    d.listings <- tbl(con, "Listings") 
    d.est <- tbl(con, "Establishments")
    d.busTyp <- tbl(con, "BusinessTypes")
    d.plat <- tbl(con, "Platforms") 
    d.platLink <- tbl(con, "PlatformPlatformTypes")
    d.platTyp <- tbl(con, "PlatformTypes")

    ############## split platforms over 'restaurants' and 'social'  ##############
    # all platforms
    avail.platforms <- reactive({
        d.plat %>%
        semi_join(d.listings, by = "PlatformID") %>%
        select(PlatformName, PlatformID) %>%
        collect() 
    })
    # platforms of social page
    # If ANY businesstype of a platform is social put it in "avail.platforms.social"
    avail.platforms.social <- reactive({
        d.platTyp %>%
        filter(BusinessModelType %like% "%Social%")  %>%
        {semi_join(d.platLink, ., by = "PlatformTypeID")} %>%
        {semi_join(d.plat, ., by = "PlatformID")} %>%
        semi_join(d.listings, by = "PlatformID") %>%
        collect() %>%
        select(PlatformName, PlatformID)
    })
    # Platforms of restaurants page
    # all other platforms get put into "avail.platforms.res"
    avail.platforms.res <- reactive({
        avail.platforms() %>%
            anti_join(avail.platforms.social(), by = "PlatformID")
    })
    ########################################## Restaurants page ##########################################
    r.restaurantspageopened <- reactive({        
        if(input$tabs == "restaurants"){
            return(TRUE)
        } else{
            return(NULL)
        }
    })
    restaurantModuleServer("restaurants", avail.platforms.res,  d.est, d.listings, d.plat, r.restaurantspageopened)
    
    ########################################## Social page ##########################################
    r.socialpageopened <- reactive({        
        if(input$tabs == "social"){
            return(TRUE)
        } else{
            return(NULL)
        }
    })
    socialModuleServer("social", IN_OMITTED_WORDS, IN_FOOD_DICT, d.listings,  avail.platforms.social, r.socialpageopened)

    ########################################## ecosystem pages ##########################################
    r.ecopageopened <- reactive({        
        if(input$tabs == "ecosystem"){
            return(TRUE)
        } else{
            return(NULL)
        }
    })
    r.listpageopened <- reactive({        
        if(input$tabs %in% c("platforms", "typology")){
            return(TRUE)
        } else{
            return(NULL)
        }
    })
    ecosystemModuleServer("ecosystem", IN_ECO_NODES, IN_ECO_EDGES, IN_ECO_SPECS, 
        d.listings, d.platLink, d.plat, d.platTyp, r.ecopageopened, r.listpageopened, 
        avail.platforms, session)
}
