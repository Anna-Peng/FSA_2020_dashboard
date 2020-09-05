
#### HEADER ####
header = dashboardHeader(title = "Online FBO dashboard")

# form-group shiny-input-container
#### SIDEBAR ####
sidebar =  dashboardSidebar(width = 230
  ,tags$head(tags$style(HTML("
    section.sidebar .shiny-input-container {
        /* Proper spacing around inputs. */
    padding: 0px 0px 0px 0px;
        /* Wrap content (important for inline inputs). */
    white-space: normal;
    }"
    )
  ))
  ,sidebarMenu(id = "tabs"
    ,menuItem("Start", tabName = "start", icon = icon("dashboard"))
    ,menuItem("Ecosystem", tabName = "ecosystemAll",icon = icon("leaf", lib = "font-awesome")
      ,startExpanded = TRUE
      ,menuSubItem("Schematic", tabName = "ecosystem")
      ,menuSubItem("Platform typology", tabName = "typology")
      ,menuSubItem("Platforms", tabName = "platforms")    
      )
    ,menuItem("Restaurants", tabName = "restaurants", icon = icon("utensils", lib = "font-awesome"))
    ,menuItem("Social", tabName = "social", icon = icon("facebook", lib = "font-awesome", class = "fa"))
    ,br()
    ,conditionalPanel(condition = "['ecosystem'].includes(input.tabs)"
      ,ecosystemModuleUIcontrolSchematic("ecosystem")
    )
    ,conditionalPanel(condition = "['typology', 'platforms'].includes(input.tabs)"
      ,ecosystemModuleUIcontrolLists("ecosystem")
    )
    ,conditionalPanel(condition = "['restaurants'].includes(input.tabs)"
      ,restaurantModuleUIcontrol("restaurants")
    )
    ,conditionalPanel(condition = "['social'].includes(input.tabs)"
      ,socialModuleUIcontrol("social")
    )
  )
)

#### BODY ####
body  =  dashboardBody(tags$head(tags$style(HTML("
  div.widetable {
    font-size: 90%; 
    overflow-x: scroll;
  },
  .footer {
    position: fixed;
    left: 0;
    bottom: 0;
    width: 100%;
    background-color: red;
    color: white;
    text-align: center;
    }
  .info-box {
    min-height: 45px;
    margin-top: 10px;
    margin-bottom: 10px;
    margin-left: 5px;
    margin-right: 5px;
    padding-right:5px;
    padding-left: 5px;
    position: relative;
    overflow:hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    max-width: 260px;
    } 
  .info-box-icon {
    height: 100%;
    position: absolute;
    left:0;
    top:0; 
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 50px;
    font-size:220%;
    }
  .info-box-content {
    padding-top: 0px;
    margin-left: 50px;
    padding-bottom: 0px;
  }
  .modal-backdrop
  {
      opacity:0.05 !important;
  }
  .modal
  {
    background: rgba(0,0,0,.05);
  }
  .modal-content{
    position: relative;
    display: flex;
    flex-direction: column;
    margin-top: 50%;
  }
  .ecoWrapper {
    height: 0;
    overflow: hidden;
    padding-top: 60%;
    position: relative;
  }
  .eco{
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
  }
  .flexbox-centering {
    height: 100%;
  }")))
  ,tabItems(tabItem(tabName = "start"
      ,div(style = "max-width: 1200px; align: center;"
        ,fluidRow(box(width = 12
          ,includeMarkdown(IN_MD_START)
        ))
      )
    )
    ,tabItem(tabName = "ecosystem"
      ,ecosystemModuleUIpageEcosystem("ecosystem", IN_MD_PLATFORMS)
    )
    ,tabItem(tabName = "typology" 
      ,ecosystemModuleUIpageTypology("ecosystem", IN_MD_TYPOLOGY)
    )
    ,tabItem(tabName = "platforms"
      ,ecosystemModuleUIpagePlatforms("ecosystem", IN_MD_PLATFORMS)
    )
    ,tabItem(tabName = "restaurants"
      ,restaurantModuleUIpage("restaurants", IN_MD_RESTAURANTS)
    )
    ,tabItem(tabName = "social"
      ,socialModuleUIpage("social", IN_MD_SOCIAL)
    )
  )
)



#### Shiny UI ####
dashboardPage(header, sidebar, body)
