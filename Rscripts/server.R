
library(shiny)



# Define server logic 
shinyServer(function(input, output, session) {
  
  ### return run information -----
  runinfo <- reactive({
    
    #paste0("Simulated ", input$outputlayer, " in ", input$year, " with the ", input$production, " parameters and ",  input$scenario, " scenario." )
    #paste0("Simulated ", input$outputlayer, " in ", input$year, " with the ", input$scenario, " scenario." )
  
    })
  
  ### ? -----
  observeEvent(input$deleteCache, {
    session$sendCustomMessage(type = 'message',
                              message = 'deleteCache')
    print("delete cache")
    unlink(paste0(path_rastercache, "/*"), recursive = T)
    unlink(paste0(path_filecache, "/*"), recursive = T)
    
  })
  
  
  ### Return run info pane -----
  # output$PaneRuninfo <- renderText({
  #   runinfo()
  # })
  
  
  ### New input -----
  rnew_input <- reactive({
    print("rnew_input called")
    
    w_idx <- which(input$world == world_shortnames)
    
    #fname_changed <- getFname( world_names[w_idx], input$production, input$scenario, input$year)
    fname_changed <- getFname( world_names[w_idx], input$scenario, input$year)
    
    r_changed <- getRaster(fname_changed, band.name = input$inputlayer, resolution = RESOLUTION_WEB, location = location_UK)
    
    return(r_changed)
  })
  
  
  ### Basemap? -----
  providernew <- reactive({
    print("providernew called")
    
    input$background
  })
  
  
  ### Stats pane? -----
  output$Tab1_StatisticsPane <- renderPlot({
    
    print("draw stat pane")
    
    target_data = rnew()
    
    
    par(mar = c(5.1, 4.1, 4, 1), mfrow=c(1,2))
    hist(getValues(target_data), main="Histogram", xlab= input$outputlayer)
    
  })
  
  ### AFT table? -----
  # output$Tab1_AFTTablePane <- renderDataTable({
  #   
  #   print("draw AFT pane")
  #   
  #   AFT_tb = aftnames # read.csv("Tables/AFT_Names_UK.csv")
  #   # AFT_tb[,c("Name", "Description", "Group", "Type")]
  #   AFT_tb_toplot = AFT_tb[,-1]
  #   
  #   
  #   DT::datatable(AFT_tb_toplot, options= list(paging = FALSE),  editable = F)
  #   
  # })
  # 
  # 
  # ### Behavioural table -----
  # output$Tab1_BehaviouralTablePane <- renderDataTable({
  #   
  #   print("draw behavioural pane")
  #   
  #   foldername_tmp <- paste0("Tables/agents/")
  #   
  #   aftparams_df <- sapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/AftParams_", x, ".csv"))) %>% t
  #   
  #   aftparams_df <- data.frame(aftparams_df)
  #   
  #   aftparams_df$productionCsvFile <- NULL
  #   
  #   DT::datatable(aftparams_df, options= list(paging = FALSE),  editable = F) 
  #   
  # })
  
  ### Production table -----
   # output$Tab1_ProductionTablePane <- renderDataTable({
   #   
   #   print("draw production pane")
   #   
   #   
   #   foldername_tmp <- ("Tables/production/Baseline")
   #   foldername_tmp <- paste0("Tables/production/", input$scenario)
   #   
   #   productionparams_l <- lapply(aft_shortnames_fromzero[-length(aft_shortnames_fromzero)], FUN = function(x) read.csv(paste0(foldername_tmp, "/", x, ".csv"))) 
   #   
   #   a_idx <- 1 
   #   x <- productionparams_l[[a_idx]]
   #   
   #   colnames(x)[1] = "Service"
   #   
   #   DT::datatable(x, options= list(paging = F),  editable = F, rownames = F, caption = aft_names_fromzero[a_idx]) 
   #   
   # })
   
   ### Timeseries plot -----
     output$Tab2_TimeseriesPlotPane <- renderPlot(height = "auto", width = 1000, res = 96, {
     
     print("draw timeseries pane")
     
     
     #scenario_tmp = "Baseline"
     # scenario_tmp = "RCP4_5-SSP4"
     # scenario_tmp = "Baseline"
     #production_tmp = "V2_June21"
     
     scenario_tmp = input$scenario
     #production_tmp = input$production
     
     # aft composition
     #aft_csvname_changed_v <- fs::path_expand(paste0(version_suffix[match(input$world, world_names)], "/", production_tmp, "/", scenario_tmp, "/",  scenario_tmp, "-", runid, "-99-", version_suffix, "-AggregateAFTComposition.csv"))
     #aft_csvname_changed_v <- fs::path_expand(paste0(version_suffix[match(input$world, world_names)], "/", scenario_tmp, "/",  scenario_tmp, "-", runid, "-99-Scotland_", version_suffix, "-AggregateAFTComposition.csv"))
     aft_csvname_changed_v <- fs::path_expand(paste0(input$world,"/",scenario_tmp, "/",  scenario_tmp, "-", runid, "-99-Scotland_", version_suffix, "-AggregateAFTComposition.csv"))
     #test
     #aft_csvname_changed_v <- fs::path_expand(paste0("Thresholds/Green_Gold/Green_Gold-",runid, "-99-Scotland_", version_suffix, "-AggregateAFTComposition.csv"))
     
     aftcomp_dt_l <- lapply(aft_csvname_changed_v, FUN = function(x) getCSV(x, location = location_UK))

     aftcomp_dt <- cbind(aftcomp_dt_l[[1]][,c("Tick", "Region")],  Reduce("+", lapply(aftcomp_dt_l, FUN = function(x) x[,-c(1:2)])))
     
     # supply and demand files
     #demand_csvname_changed_v = fs::path_expand(paste0(version_suffix[match(input$world, world_names)], "/", production_tmp, "/", scenario_tmp, "/", scenario_tmp, "-", runid, "-99-", version_suffix, "-AggregateServiceDemand.csv"))
     #demand_csvname_changed_v = fs::path_expand(paste0(version_suffix[match(input$world, world_names)], "/", scenario_tmp, "/", scenario_tmp, "-", runid, "-99-Scotland_", version_suffix, "-AggregateServiceDemand.csv"))
     demand_csvname_changed_v = fs::path_expand(paste0(input$world,"/",scenario_tmp, "/", scenario_tmp, "-", runid, "-99-Scotland_", version_suffix, "-AggregateServiceDemand.csv"))
     #test
     #demand_csvname_changed_v = fs::path_expand(paste0("Thresholds/Green_Gold/Green_Gold-",runid, "-99-Scotland_", version_suffix, "-AggregateServiceDemand.csv"))
     
     demand_dt_l = lapply(demand_csvname_changed_v, FUN = function(x) getCSV(x, location = location_UK))
     
     rem_col_idx = match(c("Tick", "Region"), colnames(demand_dt_l[[1]]))
     
     demand_dt = cbind( Reduce("+", lapply(demand_dt_l, FUN = function(x) x[,-rem_col_idx ])), Region= "UK", Tick = demand_dt_l[[1]][,c("Tick")])
     
     
     #}
     
     # mean capital level
     #capital_csvname_changed <- fs::path_expand(paste0(scenario_tmp, "-", runid, "-", seedid, "-UK-AggregateCapital.csv"))
     
     #capital_scene_tmp <- read.csv(paste0("Tables/Summary/", capital_csvname_changed))
     
     aftcomp_dt_org = aftcomp_dt
     
     aftcomp_m <- t(as.matrix(sapply(aftcomp_dt[, -c(1,2)] , FUN = function(x) as.numeric(as.character(x)))))
     #test
     #aftcomp_m <- t(as.matrix(aftcomp_dt[, -c(1,2)]))
     
     
     # process csv files
     demand_m <- t(as.matrix(sapply(demand_dt[, -c(ncol(demand_dt) - 1:0)] , FUN = function(x) as.numeric(as.character(x)))))
     #test
     #demand_m <- t(as.matrix(demand_dt[, -c(ncol(demand_dt) - 1:0)]))
     
     
     str(demand_m)
     ncold = nrow(demand_m)
     idx_dem_st = ((ncold/2)+1) : ncold
     idx_sup_st = 1:(ncold/2) 
     
     shortfall_m = ((demand_m[idx_dem_st,] - demand_m[idx_sup_st,]) / demand_m[idx_dem_st,] ) * 100
     shortfall_m[!is.finite(shortfall_m)] <- NA
     
     par(mfrow=c(4,2), mar = c(5.1, 5.1, 2, 0)  + c(0,0,0,10), oma=c(0,0,0,0))
     
     # par(mfrow=c(4,2), xpd = T, mar = par()$mar + c(0,0,0,7))
     # par( mar = c(5.1, 4.1, 4, 0)  + c(0,0,0,8))
     
     
     # AFT changes
     n_cell_unmanaged = n_cell_total - colSums(aftcomp_m) 
     
     aftcomp_m = rbind(aftcomp_m, n_cell_unmanaged)
     
     rownames(aftcomp_m)
     
     aftcomp_perc_m =  aftcomp_m/n_cell_total * 100
     
     
     plot(aftcomp_dt$Tick, aftcomp_perc_m[1,], type="l", xlab= "Year", ylab="Proportion (%)", col = aft_group_colors[1], ylim=c(0, max(aftcomp_perc_m, na.rm = T) * 1.1), main = "AFT composition changes", xaxt="n", lty= aft_lty_ts)
     axis(side=1, at = target_years_other, labels = target_years_other)
     
     for (a.idx in 2:nrow(aftcomp_perc_m)) {
       lines(aftcomp_dt$Tick, aftcomp_perc_m[a.idx,], col = aft_group_colors[a.idx], lty=aft_lty_ts[a.idx])
     }
     
     legend("topright", aft_group_shortnames, col = aft_group_colors, lty=aft_lty_ts, cex=LEGEND_CEX, bty="n", xpd = TRUE, inset=c(LEGEND_MAR,0), lwd=1.5)
     
     # Supply and demand
     
     supply_m_norm <- (demand_m[idx_sup_st,] / demand_m[idx_sup_st,1] - 1) * 100
     demand_m_norm <- (demand_m[idx_dem_st,] / demand_m[idx_dem_st,1] - 1) * 100
     
     supdem_range <- range(cbind(supply_m_norm))
     print(supdem_range)
     
     y_lim_max <- max(3, max(abs(supdem_range)) * 1.2)
     y_lim_v <- c(-y_lim_max, y_lim_max)
     # barplot(height = supply_m_norm, beside=T, ylab= "Relative to 2020's supply (%)", ylim= y_lim_v, col = serviceColours, main = "Service Supply", names= demand_dt$Tick, border=NA)
     # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
     
     #test
     #demand_dt <- data.frame(demand_dt)
     
     plot(demand_dt$Tick, supply_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2015 supply (%)",  main = "Service Supply", las=1, xaxt="n" )
     plot(demand_dt$Tick, supply_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2015 supply (%)",  main = "Service Supply", las=1, xaxt="n" )
     axis(side=1, at = target_years_other, labels = target_years_other)
     # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
     abline(h = 0, lty=2)
     
     for (a.idx in c(1:nrow(supply_m_norm))) {
       lines(demand_dt$Tick, supply_m_norm[a.idx,],   col = serviceColours[a.idx])
     }
     
     legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
     
     dem_range = range(cbind(demand_m_norm))
     print(dem_range)
     
     y_lim_max = max(5, max(abs(dem_range)) * 1.2)
     y_lim_v = c(max(-100, -y_lim_max), y_lim_max)
     
     # barplot(height = demand_m_norm, beside=T, ylab="Relative to 2020's supply (%)", col = serviceColours, main = "Service Demand", names= demand_dt$Tick, ylim=y_lim_v, border=NA)
     
     plot(demand_dt$Tick, demand_m_norm[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Relative to 2015 demand (%)",  main = "Service Demand", las=1, xaxt="n" )
     axis(side=1, at = target_years_other, labels = target_years_other)
     # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
     abline(h = 0, lty=2)
     abline(h = -100, lty=2)
     
     for (a.idx in c(1:nrow(demand_m_norm))) {
       lines(demand_dt$Tick, demand_m_norm[a.idx,],   col = serviceColours[a.idx])
     }
     
     # legend("topright", legend = serviceNames, fill=serviceColours, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), border=NA)
     legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
     
     
     # SDGAP
     
     sdgap  = (demand_m[idx_dem_st,] - demand_m[idx_sup_st,])
     # sdgap = (sdgap /demand_m[idx_sup_st,1]  ) * 100
     
     sdgap_range = range(sdgap, na.rm=T)
     y_lim_max = max(1, max(abs(sdgap_range)) * 1.2,  as.numeric(demand_m[idx_sup_st,1])*0.3) 
     
     y_lim_v = c(-y_lim_max, y_lim_max)
     
     plot(demand_dt$Tick, sdgap[1,], type="l", col = serviceColours[1], ylim=y_lim_v, xlab="Year", ylab="Demand - Supply (original unit)",  main = "S/D gap (=D-S)", las=1, xaxt="n", mgp=c(4,1,0))
     axis(side=1, at = target_years_other, labels = target_years_other)
     # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
     abline(h = 0, lty=2)
     
     for (a.idx in c(1:nrow(sdgap))) {
       lines(demand_dt$Tick, sdgap[a.idx,],   col = serviceColours[a.idx])
     }
     legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
     
     
     # Production shortfall
     
     shortfall_range = range(shortfall_m[,], na.rm = T)
     range(shortfall_m)
     shortfall_max = max(5, max(abs(shortfall_range)) * 1.2)
     shortfall_intv = floor(shortfall_max / 100) * 10
     shortfall_intv = max(1, shortfall_intv)
     
     
     # print(shortfall_intv)
     plot(demand_dt$Tick, shortfall_m[1,], type="l", col = serviceColours[1], ylim=c(-shortfall_max,shortfall_max), xlab="Year", ylab="",  main = "Production shortfall", las=1, xaxt="n" )
     # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
     mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
     
     axis(side=1, at = target_years_other, labels = target_years_other)
     # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
     abline(h = 0, lty=2)
     
     for (a.idx in c(1:nrow(shortfall_m))) {
       lines(demand_dt$Tick, shortfall_m[a.idx,],   col = serviceColours[a.idx])
     }
     
     legend("topright", legend = serviceNames[], col=serviceColours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
     
     # Mean capital levels
     
     # if (nrow(capital_scene_tmp) > 1) { 
     #   capital_scene_tmp[,-1] = sapply(1:length(baseline_capital_tmp[-1]), FUN = function(x) capital_scene_tmp[,x+1] /  baseline_capital_tmp[x+1])
     #   
     #   
     #   ylim_cap = max(sapply(capital_scene_tmp[,-1], max, na.rm=T))
     #   
     #   plot(capital_scene_tmp$Tick, capital_scene_tmp[,2] * 100, type="l", col = capital_colours[1], ylim=c(0, ylim_cap * 100), xlab="Year", ylab="Relative to 2015 (%)",  main = "Mean input capitals changes", las=1, xaxt="n", lwd=1.5)
     #   # title(ylab="Production shortfall (%)", mgp=c(3, 1, 0))
     #   # mtext(side = 2, text ="Production shortfall (%)", line = 4, cex=0.8)
     #   
     #   axis(side=1, at = target_years_other, labels = target_years_other)
     #   # axis(side=2, at = seq(floor(-shortfall_max), ceiling(shortfall_max), shortfall_intv))
     #   abline(h = 0, lty=2)
     #   
     #   for (a.idx in c(3:ncol(capital_scene_tmp))) {
     #     lines(capital_scene_tmp$Tick, capital_scene_tmp[,a.idx] * 100,   col = capital_colours[a.idx-1])
     #   }
     #   
     #   legend("topright", legend = capital_names$Capital[], col=capital_colours[], lty = 1, cex=LEGEND_CEX, bty="n", xpd = TRUE,  inset=c(LEGEND_MAR,0), lwd=2)
     #   
     #   
     # } else {   # baseline 
     #   plot.new()
     #   capital_scene_tmp$X = 2015
     #   colnames(capital_scene_tmp) = c("Tick", capital_names$Capital)
     #   capital_scene_tmp[-1] = capital_scene_tmp[-1] / capital_scene_tmp[-1] #all 1 
     # } 
     
   })
  
  ### Transition plot -----
  
    output$Tab3_TransitionPlotPane <- renderPlot(height = PLOT_HEIGHT, res = 96, {

    w_from_idx <- which(input$world_from == world_shortnames)
    w_to_idx <- which(input$world_to == world_shortnames)

    fname_from =  getFname(world_names[w_from_idx], input$scenario_from, year =  input$year_from)
    fname_to   =  getFname(world_names[w_to_idx], input$scenario_to, year =  input$year_to)

    #test
    #fname_from =  getFname("BehaviouralBaseline", "Green_Gold", year =  2015)
    #fname_to   =  getFname("BehaviouralBaseline", "Green_Gold", year =  2100)
    
    # Transition matrix

    csv_from = getCSV(fname_from, location = location_UK)
    csv_to = getCSV(fname_to, location = location_UK)

    # already 1 km grid in projected space (for the UK model)
    aft_old = csv_from$LandUseIndex
    aft_new = csv_to$LandUseIndex
    
    # deal with -1 (not needed for Scot)
    # aft_old[aft_old==-1] = 16
    # aft_new[aft_new==-1] = 16

    # deal with zero
    aft_old = aft_old + 1
    aft_new = aft_new + 1
    
    aft_names <- csv_from$Agent
    csv_from$LandUseIndex <- csv_from$LandUseIndex + 1
    unique(csv_from[,c(37:38)])

    # reclassify
    aft_old[aft_old==6] = 5
    aft_old[aft_old %in% c(8, 10:13)] = 8

    aft_new[aft_new==6] = 5
    aft_new[aft_new %in% c(8, 10:13)] = 8


    aft_tr.df = cbind(aft_old, aft_new)

    aft_tr.df = aft_tr.df[!is.na(rowSums(aft_tr.df)),]
    aft_tr.df = data.frame(cbind(1:nrow(aft_tr.df), aft_tr.df))
    colnames(aft_tr.df)[1] = "rowid"

    # Create the transition matrix that
    # is the basis for the transition plot

    aft_old_f = factor(aft_old)#, levels = c(1:5, 7:9, 14:17))
    aft_new_f = factor(aft_new)#, levels = c(1:5, 7:9, 14:17))

    aft_tb_oldandnew = table(aft_old_f, aft_new_f)

    trn_mtrx <- with(aft_tr.df, aft_tb_oldandnew)
    str(aft_tb_oldandnew)

    # reduce
    tr.colors = aft_colors_fromzero_17 #aft_group_colors
    tr_names =  aft_names_fromzero#aft_group_names


    aft_old_tb = rowSums(trn_mtrx)
    aft_new_tb = colSums(trn_mtrx)


    aft_old_prop = paste0(round(aft_old_tb / sum(aft_old_tb, na.rm = T) * 100, 3  ), "%")
    aft_new_prop = paste0(round(aft_new_tb / sum(aft_new_tb, na.rm = T)* 100, 3   ), "%")

    # Setup proportions
    box_prop <- cbind(aft_old_prop, aft_new_prop)
    par(mfrow=c(1,1))
    plot.new()

    transitionPlot(trn_mtrx,new_page=T, fill_start_box =  tr.colors, arrow_clr =tr.colors, cex=1, color_bar = T, txt_start_clr = "black", txt_end_clr = "black", type_of_arrow = "simple", box_txt = box_prop, overlap_add_width = 1, tot_spacing = 0.07, min_lwd = unit(0.005, "mm"), max_lwd = unit(10, "mm"),  box_label = c(input$year_from, input$year_to),)
    #transitionPlot(trn_mtrx,new_page=T, fill_start_box =  tr.colors, arrow_clr =tr.colors, cex=1, color_bar = T, txt_start_clr = "black", txt_end_clr = "black", type_of_arrow = "simple", box_txt = box_prop, overlap_add_width = 1, tot_spacing = 0.07, min_lwd = unit(0.005, "mm"), max_lwd = unit(10, "mm"),  box_label = c(2015, 2100),)
    
    legend("center", tr_names, col = tr.colors, pch=15, cex=0.9)

  })
  
  
  ### Map pane -----
  
  output$Tab1_MapPane <- renderLeaflet({
    
    print("draw mappane 1")
    
    leaflet() %>%
      clearImages() %>% clearControls() %>%
      #addTiles()
      addProviderTiles(providers$OpenStreetMap.Mapnik, # Esri.WorldImagery
                       options = providerTileOptions(noWrap = TRUE), group = "TileLayer"
      ) %>%   
      fitBounds(ext[1], ext[3], ext[2], ext[4] )
  })
  
  observe({
    
    print("draw tile layer")
    
    proxy <- leafletProxy("Tab1_MapPane", data = providernew())
    proxy %>% clearTiles() %>% addProviderTiles(input$background, options = providerTileOptions(noWrap = TRUE), group = "TileLayer")
    
  })
  
  rnew <- reactive( {
    print("Rnew called")
    
    # input$background # touch
    
    w_idx <- which(input$world == world_shortnames)
    
    #fname_changed =getFname(world_names[w_idx],input$production, input$scenario,input$year)   
    fname_changed =getFname(world_names[w_idx], input$scenario,input$year)   
    
    r_changed = getRaster(fname_changed, band.name = input$outputlayer, resolution = RESOLUTION_WEB, location = location_UK)
    
    return(r_changed)
    
  })
  
  
  # should be managed in its own observer.
  observe({
    print("redraw output layer")
    dt = rnew()
    # print(which (input$indicator == indicator_names))
    
    proxy <- leafletProxy("Tab1_MapPane", data =dt)
    proxy %>% clearImages() %>% clearControls()
    
    # touches
    input$background
    
    # Layers control
    proxy %>% addLayersControl(
      # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      baseGroups = c("ModelResult",  "Basemap"),
      # overlayGroups = c("TileLayer"),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    
    if (input$outputGroup == "print_out") {
      
      # Add output layer
      
      if (input$outputlayer == "LandUseIndex") {  # land use index
        
        
        pal_out = aft_pal
        col_out = aft_colors_fromzero
        
        proxy %>% addRasterImage(dt, project = FALSE, colors = pal_out, group = "ModelResult"
                                 , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        if (input$legend) {
          
          proxy %>% addLegend(colors = col2hex(as.character(col_out)), labels = aft_shortnames_fromzero, title = paste0("Output: ", input$outputlayer),group = "ModelResult", opacity = input$alpha)
        }
        
        
        
      } else {
        dt.v = getValues(dt)
        dt.rng = range(dt.v, na.rm = T)
        print(dt.rng)
        pal = colorNumeric(input$colors,reverse = input$InvertColour, domain = dt.rng,  na.color = "transparent")
        
        proxy %>%
          addRasterImage(dt, project = FALSE, colors =pal, group = "ModelResult", method = "bilinear"
                         , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
        if (input$legend) { proxy %>%
            addLegend(pal = pal, values = quantile(dt.v, probs=seq(1, 0, -0.05), na.rm=T),
                      title = paste0("Output ", input$outputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult", opacity=input$alpha)
        }
      }
    }
    
    if (input$outputGroup == "print_in") {
      # Add input layer
      dt_input = rnew_input()
      dt_input.v = getValues(dt_input)
      dt_input.rng = range(dt_input.v, na.rm = T)
      # print(dt_input.rng)
      
      pal_input = colorNumeric(input$colors, reverse = input$InvertColour, domain = dt_input.rng, na.color = "transparent")
      
      proxy %>%
        addRasterImage(dt_input, project = FALSE, colors =pal_input, method = "bilinear", group = "ModelResult"
                       , opacity = input$alpha, maxBytes = 4 * 1024 * 1024)
      if (input$legend) { proxy %>%
          addLegend(pal = pal_input, values = quantile(dt_input.v, probs=seq(1, 0, -0.05), na.rm=T),
                    title = paste0("Input ", input$inputlayer), labFormat = labelFormat(transform = function(x) sort(quantile(dt_input.v, probs=seq(0, 1, 0.33), na.rm=T), decreasing = FALSE)), group = "ModelResult")
      }
    }
    
    
    # add empty layer 
    proxy %>% addRasterImage(r_dummy, project = FALSE, group = "Basemap", opacity = 0) %>% addMiniMap(position = "bottomleft", zoomAnimation = T, toggleDisplay = TRUE)  %>% addMeasure()
  }) 
  
})