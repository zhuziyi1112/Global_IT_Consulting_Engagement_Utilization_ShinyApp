server = function(input, output,session) {
  
    # Spinner
    output$tb2 <- renderTable({
    Sys.sleep(2) # system sleeping for 1.5 seconds
    as.matrix(filter(Profile,Profile$`Consultant Name`== input$c1))
    })
  
    # List_of_Consultant
    output$ex1 <- DT::renderDataTable(
        DT::datatable(List_of_Consultant,filter = 'top', options = list(pageLength = 15),rownames = FALSE)
    )
    # Area_of_Interest
    output$ex2 <- DT::renderDataTable(
        DT::datatable(
          Area_of_Interest, filter = 'top',options = list(pageLength = 15,rownames = FALSE)
        )
    )
    # Language
    output$ex3 <- DT::renderDataTable(
        DT::datatable(Language, filter = 'top', options = list(pageLength = 15),rownames = FALSE)
    )
    # Project_Experience
    output$ex4 <- DT::renderDataTable(
        DT::datatable(Project_Experience, filter = 'top', options = list(pageLength = 15),rownames = FALSE)
    )
    # Skill_Level
    output$ex5 <- DT::renderDataTable(
      DT::datatable(Skill_Level,filter = 'top', options = list(pageLength = 15),rownames = FALSE)
    )
    
    # Client_Project
    output$ex6 <- DT::renderDataTable({
      datatable(Client_Project, rownames=FALSE,filter = 'top') %>% 
        formatStyle(input$selected1, background="lightblue", fontWeight='bold')
    })
    
    # To output Data Table based on multiple inputs
    output$tb1 <- DT::renderDataTable({
      datatable(filter(Client_Project2, Client %in% input$p1,`Project Name` %in% input$p2, City %in% input$p3), 
                rownames=FALSE, filter = 'top')
    })
    
    
    #filtered choice
    
    filteredChoices <- reactive({ 
      Client_Project2$`Project Name` [Client_Project2$`Client` %in% input$p1]
      Client_Project2$`Project Name` [Client_Project2$`Project Name` %in% input$p2]
      Client_Project2$`Project Name` [Client_Project2$`City` %in% input$p3]
    })
    
    observeEvent(filteredChoices(), {
      updatePickerInput(session, inputId = 'p2', label = '-2-  Project Name:', choices = filteredChoices(), selected = filteredChoices())
    })
    
    
    # session$user is non-NULL only in authenticated sessions
    output$userpanel <- renderUI({
      if (!is.null(session$user)) {
        sidebarUserPanel(
          span("Logged in as ", session$user),
          subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
      }
    })
    
    #Global project distribution map using Leadflet
    output$mymap <- renderLeaflet({
      leaflet(PMaster) %>% addTiles() %>% addProviderTiles("CartoDB.DarkMatter",group = "Dark Mode") %>% 
        addProviderTiles("CartoDB.Positron",group = "Light Mode") %>% 
        addLayersControl(
          baseGroups = c("Dark Mode", "Light Mode"),
          options = layersControlOptions(collapsed = FALSE)
        )%>% 
        addMarkers(clusterOptions = markerClusterOptions,label=sprintf("Project Name: %s \n Client Name:%s",
                                                                       PMaster$label,PMaster$Client_Name),
                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px","color" = label_col),textsize = "13px", direction = "auto"),
                   popup=~as.character(Project_Name)) %>% 
        addTerminator(time = now(), group = "day&light")%>%
        addMeasure()%>% addMiniMap(tiles="CartoDB.DarkMatter")%>% 
        fitBounds(~-100,-60,~60,70) %>% 
        addCircles(lng = ~lng, lat = ~lat, weight = 1,radius = ~sqrt(Project_Budget), popup = ~City_Name)
    })
    
    # Output a skill diagram
    output$SkillPlot <- renderPlot({
      
      Skill_Level2 = RMaster %>% subset(., select=c('Eid','Ename','Area_of_Interest_1','Area_of_Interest_2','Area_of_Interest_3','Python','Machine Learning','Deep Learning','Data Analysis','Asp.Net',
                                                    'Ado.Net','VB.Net','C#','Java','Spring Boot','Hibernate','NLP','CV','JS','React',
                                                    'Node','Angular','Dart','Flutter','Vb.Net')) %>%
        rename(.,'Consultant Name' ='Ename') %>%
        gather(., key = 'Skill', value = 'Skill Level', 'Python':'Vb.Net', 
               na.rm = TRUE) %>%
        filter(.,`Consultant Name` == input$c1)%>%
        mutate(.,Group = Eid)
      
      for (x in 1:nrow(Skill_Level2)) {
        if (Skill_Level2[x,]$`Skill`==Skill_Level2[x,]$`Area_of_Interest_1`|
            Skill_Level2[x,]$`Skill`==Skill_Level2[x,]$`Area_of_Interest_2`|
            Skill_Level2[x,]$`Skill`==Skill_Level2[x,]$`Area_of_Interest_3`){
          
          Skill_Level2[x,]$Group=1
        } 
        else{
          Skill_Level2[x,]$Group=0
        }
      }
      
      g <- ggplot(data = Skill_Level2, mapping = aes(x = `Skill`, y=`Skill Level`, fill = `Group`))+ 
        geom_col()+ 
        facet_wrap(~ `Consultant Name`) + 
        coord_flip() + 
        xlab("") + 
        ylab ("IT Consultant Skill Levels") + 
        scale_fill_gradient(low = "#3B4449",high = "#F2C00F")+theme_bw() +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
      g
    })
    
   #Output a skill distribution  
    output$SkillPlot2 <- renderPlotly({
      
      Skill_Level3 = RMaster %>% subset(., select=c('Ename','Python','Machine Learning','Deep Learning','Data Analysis','Asp.Net',
                                                    'Ado.Net','VB.Net','C#','Java','Spring Boot','Hibernate','NLP','CV','JS','React',
                                                    'Node','Angular','Dart','Flutter','Vb.Net')) %>%
        rename(.,'Consultant Name' ='Ename') %>%
        gather(., key = 'Skill', value = 'Skill Level', 'Python':'Vb.Net', 
               na.rm = TRUE) %>%
        filter(.,`Consultant Name` == input$c1)
      
       plot_ly(Skill_Level3, 
                   type = 'scatterpolar',
                   mode = "closest",
                   fill = 'toself'
      ) %>%
        add_trace(r = Skill_Level3$`Skill Level`, 
                  theta = Skill_Level3$`Skill`,
                  showlegend = TRUE,
                  mode ="markers",
                  name = Skill_Level3$`Consultant Name`
        )%>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,4)
            )
          ),
          showlegend=TRUE
        )
    })
    
    # update selections
    # updatePickerInput(session=session,inputId = "p4",choices = unique(Client_Project2$'Demanding Domain Skill'), selected = c("Country"))
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
      max_value <- max(state_stat[,input$selected])
      max_state <- 
        state_stat$state.name[state_stat[,input$selected] == max_value]
      infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    output$minBox <- renderInfoBox({
      min_value <- min(state_stat[,input$selected])
      min_state <- 
        state_stat$state.name[state_stat[,input$selected] == min_value]
      infoBox(min_state, min_value, icon = icon("hand-o-down"))
    })
    output$avgBox <- renderInfoBox({
      infoBox(paste("AVG.", input$selected),
              mean(state_stat[,input$selected]), 
              icon = icon("calculator"), fill = TRUE)})
    
    # Output a Experience plot
    output$EPlot <- renderPlot({
      
      Project_Experience2 = RMaster %>% subset(., select=c('Eid','Ename','Area_of_Interest_1','Area_of_Interest_2','Area_of_Interest_3','Total_projects','AI_project_count','ML_project_count',
                                                    'JS_project_count','Java_project_count','DotNet_project_count',
                                                    'Mobile_project_count')) %>%
                           rename(.,'Consultant Name' ='Ename', 'Project Experience' ='Total_projects', AI='AI_project_count',`Machine Learning`='ML_project_count',
                                  JS='JS_project_count',Java='Java_project_count',
                                  DotNet='DotNet_project_count',Mobile='Mobile_project_count') %>%
                            gather(.,key = 'Project Type', value = 'Project Count','AI':'Mobile',na.rm = TRUE) %>%
                            filter(.,`Consultant Name` == input$c1)%>%
                            mutate(.,Group = Eid)
      
      for (x in 1:nrow(Project_Experience2)) {
        if (Project_Experience2[x,]$`Project Type`==Project_Experience2[x,]$`Area_of_Interest_1`|
            Project_Experience2[x,]$`Project Type`==Project_Experience2[x,]$`Area_of_Interest_2`|
            Project_Experience2[x,]$`Project Type`==Project_Experience2[x,]$`Area_of_Interest_3`){
          
          Project_Experience2[x,]$Group=1
        } 
        else{
          Project_Experience2[x,]$Group=0
        }
      }
      
      p <- ggplot(data = Project_Experience2, mapping = aes(x = `Project Type`, y=`Project Count`, fill = `Group`))+ 
        geom_col()+ 
        facet_wrap(~ `Consultant Name`) + 
        coord_flip() + 
        xlab("") + 
        ylab ("Project Experience by IT Domain Skill Type (Years)") + 
        scale_fill_gradient(low = "#3B4449",high = "#F2C00F")+theme_bw() +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
      p
    })
    
    # show statistics using value Box
    output$`1B` <- renderInfoBox({
      b1 = nrow(Client_Project)
      valueBox(subtitle = "Total Number of IT Consultancy Projects in 2021", b1, color = "light-blue", icon = icon("code"),
               width = 3, href = NULL)
    })
    output$`2B` <- renderInfoBox({
      b2 = nrow(List_of_Consultant)
      valueBox(subtitle = "Total Number of IT Consultants", b2, color = "orange", icon = icon("user-circle"),
               width = 3)
    })
    
    output$`3B` <- renderInfoBox({
      b3 = sum(Project_Experience$`Project Experience`)
      valueBox(subtitle = "Accumulated Project Case Volume", b3, color = "navy", icon = icon("list-alt"),
               width = 3)
    })
    
    output$`4B` <- renderInfoBox({
      b4 = length(unique(Client_Project$`Client`))
      valueBox(subtitle = "Total Number of Active Clients", b4, color = "aqua", icon = icon("hourglass-half"),
               width = 3)
    })
    
    output$`5B` <- renderInfoBox({
      b5 = length(unique(Client_Project$`City`))
      valueBox(subtitle = "Global City Presence", b5, color = "teal", icon = icon("city"),
               width = 3)
    })
    
    output$`6B` <- renderInfoBox({
      b6 = USD(sum(Client_Project$`Project Scale`))
      valueBox(subtitle = "Total Project Scale in USD", b6, color = "purple", icon = icon("dollar"),
               width = 3)
    })
    
    # Output a treemap
    

    output$treemap <- renderPlot({
    
    t <- ggplot(data = TPP, mapping = aes(
      subgroup = ISO, fill = interaction(Domain_Skill, ISO), area = Budget))
    
    t + geom_treemap(color="white", size=0.5*.pt, alpha=NA) +
      geom_treemap_subgroup_text(place = "center", alpha = 0.5, grow = TRUE) + 
      geom_treemap_text(mapping = aes(label = Domain_Skill), color = "white",place = "center", grow = FALSE) +
      guides(fill = FALSE)
    
    })
    
    output$treemap2 <- renderPlot({
      
      t <- ggplot(data = TPP, mapping = aes(
        subgroup = Domain_Skill, fill = interaction(Domain_Skill, ISO), area = Budget))
      
      t + geom_treemap(color="white", size=0.5*.pt, alpha=NA) +
        geom_treemap_subgroup_text(place = "center", alpha = 0.5, grow = TRUE) + 
        geom_treemap_text(mapping = aes(label = ISO), color = "white",place = "center", grow = FALSE) +
        guides(fill = FALSE)
      
    })
    
    # Output a skill comparison plot
 
    output$SkillPlot3 <- renderPlotly({
      
      Skill_Level4 = RMaster %>% subset(., select=c('Ename','Python','Machine Learning','Deep Learning','Data Analysis','Asp.Net',
                                                    'Ado.Net','VB.Net','C#','Java','Spring Boot','Hibernate','NLP','CV','JS','React',
                                                    'Node','Angular','Dart','Flutter','Vb.Net')) %>%
        rename(.,'Consultant Name' ='Ename') %>%
        gather(., key = 'Skill', value = 'Skill Level', 'Python':'Vb.Net', 
               na.rm = TRUE) %>%
        filter(.,`Consultant Name` == input$c2)
      
      plot_ly(Skill_Level4, 
              type = 'scatterpolar',
              mode = "closest",
              fill = 'toself'
      ) %>%
        add_trace(r = Skill_Level4$`Skill Level`, 
                  theta = Skill_Level4$`Skill`,
                  showlegend = TRUE,
                  mode ="markers",
                  name = Skill_Level4$`Consultant Name`
        )%>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,4)
            )
          ),
          showlegend=TRUE
        )
    })
    
    #Upload files 
    
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
   #Download files
    
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
      switch(input$dataset,
             "Resource Master" = RMaster,
             "Project Master" = PMaster)
    })
    
    # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
}