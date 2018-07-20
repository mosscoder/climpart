server <- shinyServer(function(input, output, session) {
  shinyalert(title = 'Welcome to the Climpart App!',
             text = HTML('App is initialized!<br><br>
                          For a detailed explanation of the underlying analyses, see
                         <a href="https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/eap.1505">Doherty et al. (2017)</a><br><br>
                         If you want to process a very large extent, it is best to run the app offline.<br>
                         <a href="https://storage.googleapis.com/seedmapper_dat/offlineInstructions.html">
                         Click here for more information!</a>'),
             type = 'success',
             closeOnClickOutside = TRUE,
             html = T)
  
  observeEvent(input$moreInfo, {
    showModal(modalDialog(HTML('This sampling tool is intended for use 
                               by USGS personnel, academia, the native seed industry, and the 
                               public. The analyses presented here are  
                               based upon the methods described in 
                               <a href="https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1002/eap.1505">Doherty et al. (2017)</a>.
                               Please send questions, 
                               comments, suggestions for improvements, and error reports via 
                               email to USGS - Southwest Biological Science Center c/o Kyle 
                               Doherty (<a href="mailto:kyledaviddoherty@gmail.com">kyledaviddoherty@gmail.com</a>). 
                               The current web location for this tool is temporary and it will be 
                               hosted on a USGS server as soon as a suitable one can be located.<br><br>
                               Written by Kyle Doherty, U.S. Geological 
                               Survey, Southwest Biological Science Center, Flagstaff, Arizona. Written in the programming 
                               language R (R Core Team (2015). R: A language and environment for 
                               statistical computing. R Foundation for Statistical Computing,
                               Vienna, Austria. URL http://www.R-project.org/).<br><br>
                               
                               Disclaimer: Although this program has been used by the USGS, no 
                               warranty, expressed or implied, is made by the USGS or the United 
                               States Government as to the accuracy and functioning of the 
                               program and related program material nor shall the fact of 
                               distribution constitute any such warranty, and no responsibility is 
                               assumed by the USGS in connection therewith.
                               '
    ),
    easyClose = TRUE,
    footer = HTML("<button type='button' class='btn btn-success' data-dismiss='modal'>OK</button>")
    ))
  })
  
  output$instruct <- renderText('The purpose of this app is to aid sampling efforts along climate gradients for a 
                                geographic region of interest.  Examples of potential uses of this tool include: 
                                sampling plant materials for common garden studies, establishing common garden 
                                arrays, establishing vegetation transects, or banking seed for native plant 
                                conservation. Analyses are conducted on the Bioclim 
                                (http://www.worldclim.org/bioclim) dataset for the extent of 15 to 60 degrees 
                                latitude and -135 to -45 degrees longitude. We chose to incorporate seven of these 
                                variables that together capture unique axes of multivariate climate space, including: 
                                mean annual temperature, diurnal range, temperature seasonality, temperature of 
                                warmest quarter, mean annual precipitation, precipitation seasonality, and 
                                precipitation of warmest quarter. 
                                To operate the app, the user may input a lat/long bounding box with the supplied 
                                slider bars or a spatial polygon. They then specify the number of partitions (as many as 50), click the 
                                "Generate Partitions" button, and the app then uses cluster analysis to group the user-
                                defined climate space into the desired number of partitions. Within each partition 
                                the app identifies the map cell that corresponds to the multivariate median, or 
                                medoid, which we refer to as a climate center. After calculating these points, the app 
                                then assigns each map cell to the climate center closest in climate space and maps 
                                these assignments as regions of differing colors. Also reported are the 
                                corresponding coordinates and Bioclim data for each of the climate centers, as well 
                                as the distributions of their assignments. 
                                The user can then download and explore the underlying rasters, climate center data, 
                                and within-assignment distributions for offline use. To reproduce the aesthetics of the 
                                mapping feature, set high values of the simval.tif as black and low values as white, 
                                then overlay the center.assignment.tif, setting it to ~50% transparency, with values 
                                as categorical, each value a contrasting color.')
  
  output$leaf <- renderLeaflet({
    leaflet() %>%
      setView(lat = 50, lng = -100, zoom = 3) %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Terrain") %>%
      addProviderTiles("CartoDB.Positron", group = "Light Basemap") 
  })
  
  observe({
    if(input$boundSelect == "slider"){ 
    leafletProxy('leaf') %>%
      clearShapes() %>%
      addRectangles(lng1 = input$lon.range[1],
                    lng2 = input$lon.range[2],
                    lat1 = input$lat.range[1],
                    lat2 = input$lat.range[2],
                    color = '#317873',
                    weight = 2)
    }
    
    if(!is.null(input$boundFile2) & input$boundSelect == "poly"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Processing user polygon", value = 0.5)
      
      file.remove(paste0(temp.folder,'/userPoly.shp'))
      inFile <- input$boundFile2
      unzip(zipfile = inFile$datapath, exdir = temp.folder) #specify user poly here
      shp.file.dir <- list.files(path = temp.folder, pattern = "\\.shp$")
      shp.layer <- strsplit(shp.file.dir, ".shp")
      
      poly <- readOGR(dsn = path.expand(paste0(temp.folder,"/",shp.file.dir)), layer=shp.layer[[1]])
      file.remove(paste0(temp.folder,"/",shp.file.dir))
      
      poly.trans <- gBuffer(spTransform(gUnionCascaded(poly), CRS("+init=epsg:3857")), width = 1)
      poly4map <- spTransform(gUnionCascaded(poly), CRS("+init=epsg:4326"))
      
      polyDF <- SpatialPolygonsDataFrame(poly.trans, data.frame(f=0), match.ID = F)
      
      temp.folder <- tempdir()
      writeOGR(polyDF, temp.folder, "userPoly", driver="ESRI Shapefile", overwrite_layer = T)
      
      withProgress(message = "Rendering user polygon",
                   value = 0.75,
                   leafletProxy('leaf') %>%
                     clearShapes() %>%
                     addPolygons(data = poly4map,
                                 weight = 2, 
                                 color = '#317873')
      )
    }
  })
  
  climClip <- eventReactive(input$goButton,{
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Extracting climate data for user extent", value = 0.1)
    
    if(input$boundSelect == "slider"){ 
      ext <- extent(input$lon.range[1],
                    input$lon.range[2],
                    input$lat.range[1],
                    input$lat.range[2])
      
      extPoly <- as(ext, "SpatialPolygons")
      sp::proj4string(extPoly) <- "+init=epsg:4326"
      extMercator <- spTransform(extPoly, CRS("+init=epsg:3857"))
      
      polyDF <- SpatialPolygonsDataFrame(extMercator, data.frame(f=0), match.ID = F)
    
      writeOGR(polyDF, temp.folder, "userPoly", driver="ESRI Shapefile", overwrite_layer = T)
      
      shpLoc <- path.expand(paste0(temp.folder,'/userPoly.shp'))
      rasLoc <- path.expand('./climateMerc.tif')
      getwd()
      gdalwarp(srcnodata=-9999, 
               dstnodata=-9999,
               overwrite = T,
               crop_to_cutline=T, 
               cutline = shpLoc,
               rasLoc,
               'clipped.tif')
      
    }
    
    if(input$boundSelect == "poly"){ #extent set by poly

      shpLoc <- path.expand(paste0(temp.folder,'/userPoly.shp'))
      rasLoc <- path.expand('./climateMerc.tif')
      getwd()
      gdalwarp(srcnodata=-9999, 
               dstnodata=-9999,
               overwrite = T,
               crop_to_cutline=T, 
               cutline = shpLoc,
               rasLoc,
               'clipped.tif')
    }
    
    stack('clipped.tif')
    
    })
  
  unscaled <- eventReactive(input$goButton,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Formatting selection", value = 0.15)
    
    clip <- climClip()
    
    clipMat <- as.data.frame(clip)
    cell <- 1:ncell(clip)
    
    colnames(clipMat) <- c("MAT","DiurnalRange","TSeasonality",
                           "TWettestQtr","MAP","PSeasonality","PWarmestQtr")
    
    xy <- xyFromCell(clip, cell)
    roiDF <- na.omit(data.frame(cell = cell, x = xy[,1], y = xy[,2], clipMat))
    
    if(nrow(roiDF) < input$cluster.num){
      shinyalert(title = 'Invalid region of interest!',
                 text = 'Selection contains insufficient land area. Limit selection to land masses
                 within the bounds of -168 to -52 degrees longitude and 7 to 83 degrees latitude. ',
                 type = 'error')
    }
    roiDF
    
  })
  
  map.crop <- eventReactive(input$goButton,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Scaling data", value = 0.25)
    
    unsc <- unscaled()
    
    cropped.stack <- data.frame(unsc[,1:3],scale(unsc[,4:10]))
    
  })
  
  max.find <- eventReactive(input$goButton,{
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating maximum climate distance", value = 0.3)
    
    cropped.stack <- map.crop()
    set.seed(123)
    while.maxxer <- function(){
      
      dists <- sqrt((cropped.stack[,4] - mean(cropped.stack[,4]))^2 + (cropped.stack[,5] - mean(cropped.stack[,5]))^2 + 
                      (cropped.stack[,6] - mean(cropped.stack[,6]))^2 + (cropped.stack[,7] - mean(cropped.stack[,7]))^2 +
                      (cropped.stack[,8] - mean(cropped.stack[,8]))^2 + (cropped.stack[,9] - mean(cropped.stack[,9]))^2 +
                      (cropped.stack[,10] - mean(cropped.stack[,10]))^2)
      
      max.dists <- max(dists)
      
      repeat{
        w.max <- cropped.stack[which.max(dists),]
        
        test.dists <- sqrt((cropped.stack[,4] - w.max[,4])^2 + (cropped.stack[,5] - w.max[,5])^2 + 
                             (cropped.stack[,6] - w.max[,6])^2 + (cropped.stack[,7] - w.max[,7])^2 +
                             (cropped.stack[,8] - w.max[,8])^2 + (cropped.stack[,9] - w.max[,9])^2 +
                             (cropped.stack[,10] - w.max[,10])^2)
        
        new.max <- max(test.dists)
        if(new.max > max.dists){
          max.dists <- new.max
          w.max <- cropped.stack[which.max(test.dists),]
        } else{
          break
        }
      }
      return(max.dists)
    }
    
    extent.max <- while.maxxer()
    extent.max
  }) 
  
  medoids <- eventReactive(input$goButton,{
    set.seed(123)
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Clustering user extent", value = 0.4)
    
    cropped.stack <- map.crop()
    extent.max <- max.find()
    
    kmeans.medoid.reps <- function(cluster, rep){
      kmeans.medoids.solutions <- function(cluster){
        species.kmeans <- kmeans(x=cropped.stack[,4:10], centers=cluster, iter.max = 100000000)
        
        species.centers <- species.kmeans$centers
        
        euc.dist <- function(full.df, med.df, med){
          
          euc <- sqrt((full.df[,4] - med.df[med,1])^2 + (full.df[,5] - med.df[med,2])^2 + 
                        (full.df[,6] - med.df[med,3])^2 + (full.df[,7] - med.df[med,4])^2 +
                        (full.df[,8] - med.df[med,5])^2 + (full.df[,9] - med.df[med,6])^2 +
                        (full.df[,10] - med.df[med,7])^2)
          min.euc <- which.min(euc)
          out <- full.df[min.euc,]
          return(out)
        }
        
        kmeans.medoids <- do.call(rbind, lapply(FUN=euc.dist, full.df=cropped.stack, med.df=species.centers, X=1:cluster))
        
        #Calculate euclidean distance between climate centers and all other cells
        mean.cell <- function(kmeans.medoids, cropped.stack){
          euc.dist <- function(med){
            euc <- sqrt((cropped.stack[,4] - kmeans.medoids[med,4])^2 + (cropped.stack[,5] - kmeans.medoids[med,5])^2 + 
                          (cropped.stack[,6] - kmeans.medoids[med,6])^2 + (cropped.stack[,7] - kmeans.medoids[med,7])^2 +
                          (cropped.stack[,8] - kmeans.medoids[med,8])^2 + (cropped.stack[,9] - kmeans.medoids[med,9])^2 +
                          (cropped.stack[,10] - kmeans.medoids[med,10])^2)
            return(euc)
          }
          
          euc.df <- as.data.frame(do.call(cbind, lapply(FUN=euc.dist, X=1:nrow(kmeans.medoids))))
          #Calculate mean euclidean distance values for each climate center and covert to climate similarity
          mean.val <- 1 - mean(do.call(pmin, data.table(euc.df)))/extent.max
          
          return(mean.val)
        }
        
        mean.cell.value <- mean.cell(kmeans.medoids = kmeans.medoids, cropped.stack = cropped.stack)
        out <- cbind(cluster, mean.cell.value, kmeans.medoids)
        return(out)
      }
      
      medoids.solutions <- do.call(rbind, lapply(FUN=kmeans.medoids.solutions, X=cluster))
      
      out <- cbind(rep, medoids.solutions)
      
      return(out)
    }
    
    kmeans.medoids.reps <- data.frame(do.call(rbind, lapply(FUN=kmeans.medoid.reps, X=1:5, cluster=input$cluster.num))) #input cluster number here
    
    best.medoids <- subset(kmeans.medoids.reps, kmeans.medoids.reps$mean.cell.value == max(kmeans.medoids.reps$mean.cell.value ))
    rep.screen <- unique(best.medoids$rep)[1]
    best.medoids <- subset(best.medoids, rep == rep.screen)
    best.medoids <- best.medoids[order(best.medoids$cell),]
    best.medoids
  }) 
  
  medprint <- eventReactive(input$goButton,{
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying best climate centers", value = 0.5)
    
    cropped.stack <- map.crop()
    extent.max <- max.find()
    best.medoids <- medoids()
    
    medoid.print <- data.frame(paste("Center", 1:nrow(best.medoids)),subset(unscaled(), cell %in% best.medoids$cell))
    colnames(medoid.print)[1] <- "Climate Center"
    medoid.print$MAT <- medoid.print$MAT/10
    medoid.print$DiurnalRange <- medoid.print$DiurnalRange/10
    medoid.print$TWettestQtr <- medoid.print$TWettestQtr/10
    
    medoid.sp <- SpatialPointsDataFrame(subset(medoid.print, select =c(x,y)), medoid.print)
    proj4string(medoid.sp) <- '+init=epsg:3857'
    medoid.trans <- spTransform(medoid.sp, '+init=epsg:4326') 
    medoid.xy <- coordinates(medoid.trans)
    medoid.print$x <- medoid.xy[,1]
    medoid.print$y <- medoid.xy[,2]
    
    medoid.print
  }) 
  
  sim.calcs <- eventReactive(input$goButton,{
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Mapping climate similarity", value = 0.6)

    cropped.stack <- map.crop()
    extent.max <- max.find()
    best.medoids <- medoids()
    medoid.print <- medprint()
    
    maps.clust.fun <- function(clim.vals){
      col.dat <- clim.vals 
      
      clim.dist.df <- function(col){  #Euclidean distance function
        
        euc <- sqrt((cropped.stack[,4] - col.dat[col,3])^2 + (cropped.stack[,5] - col.dat[col,4])^2 + 
                      (cropped.stack[,6] - col.dat[col,5])^2 + (cropped.stack[,7] - col.dat[col,6])^2 +
                      (cropped.stack[,8] - col.dat[col,7])^2 + (cropped.stack[,9] - col.dat[col,8])^2 +
                      (cropped.stack[,10] - col.dat[col,9])^2)
        return(euc)
      }
      
      euc.out <- as.data.frame(do.call(cbind, lapply(FUN=clim.dist.df, X = 1:nrow(col.dat)))) #Applying the distance function over the accessions
      colnames(euc.out) <- paste(1:nrow(col.dat))
      zone <- cbind(apply( euc.out, 1, which.min)) #Determining which accession is closest in climate space for a given cell
      zone.namer <- function(x){ # a function to rename the indices from zone to the corresponding accession id
        return(colnames(euc.out)[x])
      }
      accession <- do.call(rbind, lapply(FUN=zone.namer, X=zone)) # applying zone.namer across all accessions
      clim.sim <- 1- as.data.frame(do.call(pmin, euc.out))/extent.max #identifying minimum climate distance by cell, converting to climate similarity
      
      
      out <- as.data.frame(cbind(accession, clim.sim, cropped.stack)) #georeferencing the closest accession and climate similarity data
      colnames(out)[2] <- "clim.sim"
      return(out) #returning data frame of relevant data
    }
    
    map.vals <- maps.clust.fun(clim.vals = best.medoids[,5:13])
    levels(map.vals$accession) <- levels(medoid.print$`Climate Center`)
    map.vals$accession <- factor(map.vals$accession, levels= paste("Center",1:nrow(medoid.print)))
    map.vals
  }) 
  
  click.list <- reactive({
    
    map.vals <- sim.calcs()
    cropped.stack <- map.crop()
    
    if(is.null(input$leaf_click$lng) |
       input$leaf_click$lng < -168 |
       input$leaf_click$lng > -52 |
       input$leaf_click$lat < 7 |
       input$leaf_click$lat > 83
    ){return()}else{
      
      click.xy <- SpatialPoints(coords = data.frame(input$leaf_click$lng, input$leaf_click$lat),
                                proj4string=CRS('+init=epsg:4326'))
      click.trans <- spTransform(click.xy, '+init=epsg:3857') 
      
      map.cell <- cellFromXY(climClip(), click.trans)
    }
    
    center <- subset(map.vals, cell == map.cell, select=c("accession","clim.sim"))
    vals <- subset(unscaled(), cell == map.cell, select=c("MAT","DiurnalRange","TSeasonality",
                                                         "TWettestQtr","MAP","PSeasonality","PWarmestQtr"))
    
    if(nrow(center) > 0){
      sub <- data.frame(center,vals)
      
      sub$clim.sim <- round(sub$clim.sim*100)
      sub$MAT <- sub$MAT/10
      sub$DiurnalRange <- sub$DiurnalRange/10
      sub$TWettestQtr <- sub$TWettestQtr/10
      
      colnames(sub)[1:2] <- c("Assignment","Climate Similarity")
      labeler <- function(x){
        out <- paste("<b>", colnames(sub)[x], "</b>", ":", sub[,x], "<br>")
        return(out)
      }
      
      out <- do.call(rbind, lapply(FUN=labeler, X=1:ncol(sub)))
    }else{out <- NULL}
    out
  })
  
  box.react <- eventReactive(input$goButton,{
    
    map.vals <- sim.calcs()
    medoid.print <- medprint()
    
    sub.vals <- withProgress(message="Extracting data for center assignments", value=0.91, 
                             filter(unscaled(), cell %in% map.vals$cell))
    forPlot <- cbind(map.vals$accession,sub.vals[,4:10])
    colnames(forPlot) <- c("accession", colnames(unscaled()[,4:10]))
    forPlot$MAT <- forPlot$MAT/10
    forPlot$DiurnalRange <- forPlot$DiurnalRange/10
    forPlot$TWettestQtr <- forPlot$TWettestQtr/10
    melt <- withProgress(message="Formatting for box and whisker plots", value=0.93,
                         melt(forPlot, id.vars = "accession", measure.vars = c(colnames(forPlot[2:8]))))
    
    if(nrow(medoid.print) <= 50){
      palette.full <- c("#8B1117",
                        "#29D32A",
                        "#F743FB",
                        "#03ADC3",
                        "#F0A733",
                        "#6F1D68",
                        "#7B8BFA",
                        "#188D57",
                        "#1F3D46",
                        "#FA5B93",
                        "#C498C4",
                        "#F8651F",
                        "#4E5705",
                        "#B7755E",
                        "#283F85",
                        "#E028B0",
                        "#92C015",
                        "#0196DB",
                        "#A1AB60",
                        "#DC9AF8",
                        "#66BE9F",
                        "#317313",
                        "#B95D18",
                        "#A27811",
                        "#61410C",
                        "#107585",
                        "#9B005F",
                        "#E92725",
                        "#B62DC2",
                        "#6ECC4B",
                        "#F3606F",
                        "#CCB437",
                        "#622515",
                        "#7A2C8F",
                        "#98A008",
                        "#4CCBC4",
                        "#FF70B8",
                        "#7C9FF7",
                        "#393364",
                        "#C64704",
                        "#4F673D",
                        "#A96AD3",
                        "#CB533C",
                        "#8ED14A",
                        "#9FAEC1",
                        "#C9A551",
                        "#85D1A2",
                        "#C6548A",
                        "#524874",
                        "#653A35")
      
      palette <- palette.full[1:nrow(medoid.print)]
    }
    
    if(nrow(medoid.print) > 25){
      cols <- 2
    }else{
      cols <- 1
    }
    
    box <- ggplot(data=melt, aes(x=accession, y=value, fill=accession))+
      theme_bw()+
      geom_boxplot()+
      scale_fill_manual(values=palette)+
      guides(fill= guide_legend(title="Climate\nCenter\nAssignment", ncol= cols))+
      facet_wrap(~variable, scales= "free_y", ncol=1)+
      xlab("Climate Center Assignments")+
      ylab("")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size = 18, face = 'bold'))
    
    withProgress(message = "Saving box plot data", value = 0.95, ggsave(filename=paste(temp.folder,"/assignments.boxplot.pdf",sep=""), plot=box, width=8.5,height=11, dpi=300))
    withProgress(message="Generating box and whisker plots", value=0.97, box)
  })
  
  rasStack <- eventReactive(input$goButton,{

    cropped.stack <- map.crop()
    extent.max <- max.find()
    best.medoids <- medoids()
    map.vals <- sim.calcs()
    medoid.print <- medprint()
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Projecting rasters", value = 0.7)
    
    leaf.template <- raster(nrow=nrow(climClip()), ncol = ncol(climClip()),
                            xmn=xmin(climClip()), xmx= xmax(climClip()),
                            ymn=ymin(climClip()), ymx=ymax(climClip()),
                            resolution=c(927.6624, 927.6624),
                            crs="+init=epsg:3857")
    
    clim.ras <- leaf.template
    bound.ras <- leaf.template
    
    values(clim.ras)[map.vals$cell] <- round(map.vals$clim.sim*100)
    values(bound.ras)[map.vals$cell] <- as.integer(map.vals$accession)
    
    dataType(clim.ras) <- "INT1U"
    dataType(bound.ras) <- "INT1U"
    
    stack(clim.ras,bound.ras)
    
  })
  
  palettes <- eventReactive(input$goButton,{
    cropped.stack <- map.crop()
    extent.max <- max.find()
    best.medoids <- medoids()
    map.vals <- sim.calcs()
    medoid.print <- medprint()
    clim.ras <- rasStack()[[1]]
    bound.ras <- rasStack()[[2]]
    clim.ras <- rasStack()[[1]]
    bound.ras <- rasStack()[[2]]
    
    if(nrow(medoid.print) <= 50){
      palette.full <- c("#8B1117",
                        "#29D32A",
                        "#F743FB",
                        "#03ADC3",
                        "#F0A733",
                        "#6F1D68",
                        "#7B8BFA",
                        "#188D57",
                        "#1F3D46",
                        "#FA5B93",
                        "#C498C4",
                        "#F8651F",
                        "#4E5705",
                        "#B7755E",
                        "#283F85",
                        "#E028B0",
                        "#92C015",
                        "#0196DB",
                        "#A1AB60",
                        "#DC9AF8",
                        "#66BE9F",
                        "#317313",
                        "#B95D18",
                        "#A27811",
                        "#61410C",
                        "#107585",
                        "#9B005F",
                        "#E92725",
                        "#B62DC2",
                        "#6ECC4B",
                        "#F3606F",
                        "#CCB437",
                        "#622515",
                        "#7A2C8F",
                        "#98A008",
                        "#4CCBC4",
                        "#FF70B8",
                        "#7C9FF7",
                        "#393364",
                        "#C64704",
                        "#4F673D",
                        "#A96AD3",
                        "#CB533C",
                        "#8ED14A",
                        "#9FAEC1",
                        "#C9A551",
                        "#85D1A2",
                        "#C6548A",
                        "#524874",
                        "#653A35")
      
      palette <- palette.full[1:nrow(medoid.print)]
    }
    
    grays <- withProgress(value=0.8, message="Assigning Aesthetics",gray.colors(n=10, start = 1, end = 0, alpha = NULL))
    
    # sim.pal <- withProgress(value=0.90, message="Assigning Aesthetics",colorNumeric(grays, values(clim.ras), 
    #                                                                                 na.color = 'transparent'))
    
    sim.pal <- withProgress(value=0.90, message="Assigning Aesthetics",colorNumeric(grays, minValue(clim.ras):maxValue(clim.ras), 
                                                                                    na.color = 'transparent'))
    
    
    ras.zone.pal <- withProgress(value=0.95, message="Assigning Aesthetics",colorFactor(palette, 
                                                                                        domain=factor(1:nrow(medoid.print)),
                                                                                        na.color = 'transparent'))
    c(sim.pal, ras.zone.pal)
  })
  
  observeEvent(input$goButton,{
    input$goButton
    
    if(input$goButton[1]==0){
      return()
    }
    
    cropped.stack <- isolate(map.crop())
    extent.max <- isolate(max.find())
    best.medoids <- isolate(medoids())
    map.vals <- isolate(sim.calcs())
    medoid.print <- isolate(medprint())
    clim.ras <- isolate(rasStack()[[1]])
    bound.ras <- isolate(rasStack()[[2]])
    sim.pal <- isolate(palettes()[[1]])
    ras.zone.pal <- isolate(palettes()[[2]])
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Rendering map", value = 0.99)
    
    ext <- extent(clim.ras)
    extPoly <- as(ext, "SpatialPolygons")
    sp::proj4string(extPoly) <- "+init=epsg:3857"
    extLatLon <- extent(spTransform(extPoly, CRS("+init=epsg:4326")))
    
    leafletProxy("leaf")  %>%
      flyToBounds(lng1 = extLatLon[1], lng2 = extLatLon[2], lat1 = extLatLon[3], lat2 = extLatLon[4]) %>%
      clearMarkers() %>%
      clearImages() %>%
      clearControls() %>%
      clearShapes() %>%
      addCircleMarkers(data=medoid.print, lng= ~x, lat =~y, radius=4, color="black", group="Overalys") %>%
      addCircleMarkers(data=medoid.print, lng= ~x, lat =~y, radius=3, color="white", 
                       fillOpacity = 1,  group="Overalys", label=~medoid.print[,1]) %>%
      addRasterImage(clim.ras, colors = sim.pal, opacity = 0.9, 
                     project=FALSE, maxBytes = 8 * 1024 * 1024, group="Overalys") %>%
      addRasterImage(bound.ras, colors = ras.zone.pal, opacity = 0.45,
                     project=FALSE, maxBytes = 8 * 1024 * 1024, group="Overalys") %>%
      addLayersControl(baseGroups = c("Light Basemap", "Terrain"),
                       overlayGroups = c("Overalys"),
                       options = layersControlOptions(collapsed = F),
                       position=ifelse(nrow(medoid.print) > 30, "topleft", "topright")) %>%
      addLegend("topright",pal = ras.zone.pal,
                values = factor(1:nrow(medoid.print)),
                title ="Assignment",
                layerId = 'assignL') %>%
      addLegend(ifelse(nrow(medoid.print) > 20, "bottomleft", "topright"),
                pal = sim.pal, values = minValue(clim.ras):maxValue(clim.ras),
                title ="Climate<br>Similarity", bins = 5,
                layerId = 'simL')
    
  })
  
  observe({
    if(!is.null(input$leaf_click$lng) && !is.null(click.list())){
      leafletProxy("leaf")  %>%
        clearPopups() %>%
        addPopups(lng=input$leaf_click$lng, lat=input$leaf_click$lat,
                  popup=paste(
                    "<b>", "Long:", "</b>", round(input$leaf_click$lng, 5),  "<br>",
                    "<b>", "Lat:","</b>", round(input$leaf_click$lat, 5),  "<br>",
                    paste(click.list(), collapse="")))}
  })
  
  output$boxPlot <- renderPlot({
    box.react()
  })
  
  output$centerTable <- renderDataTable({
    raw <- medprint() %>% dplyr::select(-cell) %>% dplyr::rename(Latitude = y, Longitude = x)
    raw
  })
  
  output$downloadData <- downloadHandler(
    
    filename = paste("climate_partitioning_data_",Sys.Date(),".zip", sep=""),
    content = function(fname) {
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      old.wd <- getwd()
      setwd(temp.folder)
      print(temp.folder)
      
      cropped.stack <- isolate(map.crop())
      extent.max <- isolate(max.find())
      best.medoids <- isolate(medoids())
      map.vals <- isolate(sim.calcs())
      medoid.print <- isolate(medprint())
      box.react <- isolate(box.react())
      
      fs <- c("simval.tif", "center.assignment.tif", "center.data.csv","metadata.txt","assignments.boxplot.pdf")
      
      progress$set(message = "Rasterizing Data", value = 0.25)
      
      clim.ras <- rasStack()[[1]]
      bound.ras <- rasStack()[[2]]
      
      projClim.ras <- projectRaster(clim.ras, crs = '+init=epsg:4326')
      projBound.ras <- projectRaster(bound.ras, crs = '+init=epsg:4326', method = 'ngb')
      NAvalue(projClim.ras) <- 0
      NAvalue(projBound.ras) <- 0
      
      progress$set(message = "Writing Raster Files", value = 0.5)
      
      writeRaster(projClim.ras, "simval.tif", format="GTiff", overwrite=TRUE, datatype = "INT1U", NAflag = 0)
      writeRaster(projBound.ras,"center.assignment.tif", format="GTiff", overwrite=TRUE, datatype = "INT1U", NAflag = 0)
      
      write.csv(medoid.print, file = "center.data.csv", row.names = FALSE)
      
      cat("Explanation of contents:",
          "",
          "simval.tif: this raster contains the climate similarity values for the extent of interest. It reports how climatically close each cell is to its climate center assignment",
          "",
          "center.assignment.tif: this raster contains values for climate center assignments for all cells within the extent of interest", 
          "",
          "center.data.csv: this file contains coordinates and climate variate values for all climate centers generated in these analyses", 
          "",
          "assignments.boxplot.pdf: this plot displays the distribution of climate data for the cells within each center assignment ", 
          "",
          "Symbology instructions:",
          "",
          "Set high values of the simval.tif as black and low values as white. Overlay center.assignment.tif and set it to 50% Opacity with values as categorical, each a unique color"
          , file="metadata.txt",sep="\n")
      
      print (fs)
      
      progress$set(message = "Compressing Files", value = 0.9)
      zip(zipfile=fname, files=fs)
      setwd(old.wd)
    },
    contentType = "application/zip"
  )
  
  })