# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
shinyServer(function(input, output) {
  #####################################################
  #                     OUTPUTS                       # 
  #####################################################
  
  ######## Onglet des données ########
  
  output$table <- renderDataTable(brutes)
  
  output$df2 <- renderDataTable(brutes)
  
  ########################### Onglet Fonction demandée #########################################
  
  ## CLASSEMENT DES PORTS    ##
  #######################################  
  
  
  # GRAPHIQUE
  output$reg_desc_1 <- renderPlotly({
    rule_by_proto <- brutes[,c("policyid","proto","action")]
    rules_by_port <- rule_by_proto %>% group_by(policyid) %>% count
    rules_by_port <- rules_by_port %>% filter(n>input$reg_pro)
    if(input$Resultat=="proto"){
      graph=rule_by_proto %>%
        group_by(policyid,proto) %>%
        count %>%
        
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))%>%
        ggplot(aes(n, reorder(policyid, n), fill=proto)) + geom_col() + scale_fill_manual(values=c("#ff7000", "#ffac4a")) +labs(y = NULL)
      
      
    }else{
      graph=rule_by_proto %>%
        group_by(policyid,action) %>%
        count %>%
        
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n)) %>%
        ggplot(aes(n, reorder(policyid, n), fill=action))+
        theme(plot.title = element_text(hjust = 0.5))+ geom_col() + scale_fill_manual(values=c("#ff7000", "#ffac4a")) +labs(y = NULL)
      
      
    }
    
    
  })
  
  # DATATABLE
  output$table_reg_desc_1 <- renderDataTable({
    rule_by_proto <- brutes[,c("policyid","proto","action")]
    rules_by_port <- rule_by_proto %>% group_by(policyid) %>% count
    rules_by_port <- rules_by_port %>% filter(n>input$reg_pro)
    if(input$Resultat=="proto"){
      graph=rule_by_proto %>%
        group_by(policyid,proto) %>%
        count %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))
      
      
    }else{
      graph=rule_by_proto %>%
        group_by(policyid,action) %>%
        count %>%
        filter(policyid %in% rules_by_port$policyid) %>%
        arrange(desc(n))
      
      
      
    }
    
    
  })   
  
  ############# 2- Class_port ####################################
  
  ## CLASSEMENT DES PORTS    ##
  #######################################  
  
  # GRAPHIQUE
  output$port_desc <- renderPlotly({
    
    table_port <- brutes %>% group_by(dstport) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    table_port$dstport <- as.numeric(table_port$dstport)
    t <- table_port %>% filter(n>input$top_ports) %>% filter(dstport>input$range_ports[1]) %>% filter(dstport<input$range_ports[2]) %>%select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    df2 %>% filter(variable %in% input$action_choice_p) %>% filter(value >0) %>%  ggplot(aes(value, dstport,fill=variable)) +
      geom_col() +
      labs(y = NULL)+
      scale_fill_manual(values=c(permit = "#ff7000", deny = "#ffac4a"))
    
  })
  
  # DATATABLE
  output$table_port_desc <- renderDataTable({
    table_port <- brutes %>% group_by(dstport) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    table_port$dstport <- as.numeric(table_port$dstport)
    t <- table_port %>% filter(n>input$top_ports) %>% filter(dstport>input$range_ports[1]) %>% filter(dstport<input$range_ports[2]) %>%select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    tab=df2 %>% filter(variable %in% input$action_choice_p) %>% filter(value >0)
    tab
  })
  
  
  ############# 3- Rapprochement des regles ####################################
  
  output$rapp_regles <- renderPlotly({
    
    table_port <- brutes %>% group_by(dstport, policyid) %>%
      summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>%
      arrange(desc(n)) 
    
    t <- table_port %>% filter(policyid %in% input$regles_selec)  %>% select(dstport, deny, permit)
    
    df2 <- melt(t, id.vars='dstport')
    
    df2$dstport <- as.character(df2$dstport)
    df2 %>% filter(variable %in% input$action_choice_p2) %>% filter(value >0) %>%  ggplot(aes(value, dstport,fill=variable)) +
      geom_col() +
      labs(y = NULL)+scale_fill_manual(values=c(permit = "#ff7000", deny = "#ffac4a"))+
      theme_minimal()
    
    
  })
  
  
  ############# 4- Histogrammes ####################################
  
  # Action TCP
  output$act_tcp <- renderPlotly({
    
    table_action <- brutes 
    table_action$dstport <- as.numeric(table_action$dstport)
    print(table_action)
    tt <- table_action %>% filter(dstport>input$range_ports[1]) %>% filter(dstport<input$range_ports[2]) %>%select(dstport,proto, action)
    
    colors <- c(permit = "#ff7000", deny = "#ffac4a") 
    fig <- plot_ly(
      x = unique(tt[tt$proto=="TCP",]$action),
      y = table(tt[tt$proto=="TCP",]$action)[],
      marker = list(color = colors),
      name = "TCP",
      type = "bar")
    fig <- fig %>% layout(title = "Protocol TCP")
  })
  
  # Actions UDP
  output$act_udp <- renderPlotly({
    
    table_action <- brutes 
    table_action$dstport <- as.numeric(table_action$dstport)
    print(table_action)
    tt <- table_action %>% filter(dstport>=input$range_ports[1]) %>% filter(dstport<=input$range_ports[2]) %>%select(dstport,proto, action)
    
    colors <- c(permit = "#ff7000", deny = "#ffac4a") 
    fig <- plot_ly(
      x = unique(tt[tt$proto=="UDP",]$action),
      y = table(tt[tt$proto=="UDP",]$action)[],
      marker = list(color = colors),
      name = "UDP",
      type = "bar"
    )
    fig <- fig %>% layout(title = "Protocol UDP")
  })
  
  ## TOP IP    ##
  #######################################  
  
  # Onglet IP
  
  table_ip <- brutes %>% group_by(ipsrc) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) %>% 
    arrange(desc(n))
  
  table_port <- brutes %>% group_by(dstport) %>%
    summarize(deny=sum(action=="Deny"),permit=sum(action=="Permit"),n=n()) 
  
  output$top_src <- renderPlotly({
    t <- table_ip %>% top_n(input$top_ip,n) %>% select(ipsrc, deny, permit)
    df2 <- melt(t, id.vars='ipsrc')
    
    df2 %>% ggplot(aes(value, ipsrc,fill=variable)) +
      geom_col(position = "fill") +
      labs(y = NULL)+ scale_fill_manual(values=c("#ff7000", "#ffac4a"))
    
    
  })
  
  output$table_ip <- renderDataTable({
    table_ip %>% rename(total=n)
  })
  
  #Plan adressage univ
  output$table_hp <- renderDataTable({
    brutes[-grep(paste(input$id_type,collapse="|"),brutes$ipsrc),] 
  })
  
  # Donnees selon heures
  output$temp_data_heure <- renderPlotly({
    v<-"2022-03-07"
    v<-as.Date(v, "%YYYY-%mm-%dd")
    
    d<-as.integer(format(v, format = "%d"))
    m<-as.integer(format(v, format = "%m"))
    y<-as.integer(format(v, format = "%Y"))
    
    heure <- as.integer(rownames(table(brutes$datetime_hour)))
    y_deny<- table(brutes[brutes$action=="Deny" 
                          ,]$datetime_hour)[]
    count<- table(brutes[brutes$action=="Permit"
                         ,]$datetime_hour)[]
    
    all_hrs<- 0:23
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(count)]
    count[as.character(added_hrs)]<-0
    names(count)[is.na(count)] <- added_hrs
    count[is.na(count)] <- 0
    
    
    added_hrs <- all_hrs[!all_hrs %in% rownames(y_deny)]
    y_deny[as.character(added_hrs)]<-0
    names(y_deny)[is.na(y_deny)] <- added_hrs
    y_deny[is.na(y_deny)] <- 0
    
    hrs <- as.integer(rownames(table(brutes$datetime_hour)))
    
    count<-count[as.character(0:23)]
    y_deny<-y_deny[as.character(0:23)]
    
    
    dt <- data.frame(all_hrs, as.array(count))
    fig <- plot_ly(dt, x = ~all_hrs, y = ~count, name = "Permitted", type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~ y_deny, name = "Denied", connectgaps = TRUE)
    
    fig
  })
  
  #######################################
  ##         ONGLET CLUSTERING         ##
  #######################################
  
  #Dendro
  output$dendogram <- renderPlot({
    
    d = dist(analyse)
    # Hierarchical clustering dendrogram
    hc <- as.dendrogram(hclust(d))
    
    # Colors
    hc <- hc %>%
      color_branches(k = input$nb_clust) %>%
      color_labels(k = input$nb_clust)
    
    # Fan tree plot with colored labels
    # Color in function of the cluster
    hc %>%
      set("nodes_pch", 5) %>%
      set("nodes_cex", 1) %>%
      set("nodes_col", "orange")
    hc %>%
      set("labels_col", k=input$nb_clust) %>%
      set("branches_k_color",k = input$nb_clust)%>%
      raise.dendrogram (1) %>%
      plot(horiz=FALSE, axes=FALSE)
  })
  
  output$clusters_ana <- renderDataTable({
    d = dist(analyse)
    cah = hclust(d, method = "ward.D2")
    
    colors = colors = c(1:input$nb_clust)
    clus = cutree(cah, input$nb_clust)
    grp_cah <- as.data.frame(clus)
    grp_cah$ip <-rownames(grp_cah)
    analyse2 <- analyse
    analyse2$ip <- rownames(analyse2)
    analyse2 <- dplyr::left_join(analyse2,grp_cah,by="ip")
    
    analyse2 %>% filter(clus== input$choose_clust)
    
    
  },options = list(scrollX = TRUE))
  
  #KMeans
  output$km <- renderPlot({
    # Calculer k-means avec k = 3
    set.seed(123)
    res.km <- kmeans(analyse, input$nb_clust, nstart = 25)
    # Clustering K-means montrant le groupe de chaque individu
    
    output$km <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(analyse,
           col = res.km$cluster,
           pch = 20, cex = 3)
      
    })
    
  })
  
  #Network
  
  output$net <- renderVisNetwork({
    
    ip_rel <- brutes %>% group_by(ipsrc,ipdst) %>% count %>% arrange(desc(n))
    ip_rel <- as.data.frame(ip_rel)
    colnames(ip_rel) <- c("source","desti",'weight')
    edges_filter <- filter(ip_rel, weight > input$sup_net)
    
    nodes <- edges_filter %>% 
      select(source, desti) %>% 
      t %>% c %>% unique
    nodes <- as.data.frame(nodes)
    nodes$id <- seq.int(nrow(nodes))
    nodes <- nodes %>%
      select(id, everything())
    colnames(nodes) <- c("id","label")
    
    edges <- edges_filter %>% 
      left_join(nodes, by = c("source" = "label")) %>% 
      rename(from = id)
    edges <- edges %>% 
      left_join(nodes, by = c("desti" = "label")) %>% 
      rename(to = id)
    edges <- select(edges, from, to, weight)
    edges <- mutate(edges, width = weight/100)
    
    visNetwork(nodes,edges, width="100%")%>% 
    visIgraphLayout(layout = "layout_with_fr")%>%
      visOptions(highlightNearest = TRUE)})
})

