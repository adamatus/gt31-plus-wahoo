# TODOS
# - Add in bike info, stride info
# - Could probably go more precise by disecting X.1/X3 for full Lat/Lon
# - Accept lists for arguments
# - Default input files to be globbed  *.sbn and *.pwx
# - Deal with time zones
# - Read GPX/TPX files, without having to go through PWX to flatten
# - Check cross-correlations to see if things are off by a second or two between devices
# - Add plotting options
  
library(XML)
  
# gpsbabel -t -i sbn -f FILE1 -o openoffice -F snb.tab
# gpsbabel -t -i gpx -f FILE2 -o openoffice -F wahoo.tab
  
combine.with.gt31 <- function(sbnFile='',pwxFile='',outFile='combined-out.gpx')
{
  
  # Import the GT-31 data
  #sbn <- read.table('sbn.tab',sep='\t',header=T,fileEncoding="ISO-8859-1",quote="")
  sbn <- system(paste(Sys.which("gpsbabel"), ' -t -i sbn -f "',sbnFile,'" -o openoffice -F -',sep=''),
                intern=T)
  sbn <- read.table(textConnection(sbn,encoding="UTF-8"),sep='\t',header=T,quote="")
  
  # Function for converting excel formatted times to POSIXct
  convert.excel.times <- function(x) {
    thedate <- as.Date(floor(x),origin=as.Date("1899-12-30"))
    frac.hours <- x - floor(x)
    y <- as.POSIXct(thedate) + 3600*24*frac.hours
    y
  }
  
  # Convert times to POSIXct
  converted.times <- lapply(sbn[,'Time'],convert.excel.times)
  sbn$FixedTime <- as.POSIXct(unlist(converted.times),origin='1969-12-31 18:00.00 UTC')
  
  # Null out a bunch of data we don't currently need
  sbn$X.1 <- NULL
  sbn$X.3 <- NULL
  sbn$Icon <- NULL
  sbn$Name <- NULL
  sbn$Description <- NULL
  sbn$Notes <- NULL
  sbn$URL <- NULL
  sbn$Link.Text <- NULL
  
  # Import the Wahoo data, as pwx file (flattest of the formats)
  doc <- xmlParse(pwxFile)
  wahoo <- xmlToDataFrame(nodes=getNodeSet(doc,"//ns:sample","ns"),stringsAsFactors=F)
  startTime <- xmlValue(getNodeSet(doc,"//ns:time","ns")[[1]])
  startTime <- as.POSIXct(startTime,format='%Y-%m-%dT%H:%M:%S')
  
  # Make sure all of the columns are numeric
  for (j in 1:ncol(wahoo))
  {
    wahoo[,j] <- as.numeric(wahoo[,j])
  }
  
  # Convert times to specific POSIXct times, rather than second offsets from beginning
  wahoo$FixedTime <- wahoo$timeoffset+startTime
  
  # Find overlapping time points
  sbn.start <- min(sbn$FixedTime)
  sbn.end <- max(sbn$FixedTime)
  
  wahoo.start <- min(wahoo$FixedTime)
  wahoo.end <- max(wahoo$FixedTime)
  
  overlap.start <- max(sbn.start,wahoo.start)
  overlap.end <- min(sbn.end,wahoo.end)
  
  sbn.overlap <- subset(sbn,FixedTime >= overlap.start & FixedTime <= overlap.end)
  wahoo.overlap <- subset(wahoo,FixedTime >= overlap.start & FixedTime <= overlap.end)
  
  # Compute distances from start of overlap (and convert to km)
  sbn.overlap$Distance..km. <- sbn.overlap$Distance..km.-sbn.overlap$Distance..km.[1]
  wahoo.overlap$dist <- (wahoo.overlap$dist - wahoo.overlap$dist[1])/1000
  
  sbn.usage <- round(nrow(sbn.overlap)/nrow(sbn)*100,4)
  wahoo.usage <- round(nrow(wahoo.overlap)/nrow(wahoo)*100,4)
  
  cat('Computing overlap\n\tUsing ',sbn.usage,'% of SBN, ',wahoo.usage,'% of Wahoo',sep='')
  
  # Draw some plots to see how similar the measurements are...
  measures <- list(c('Latitude','Lat','lat'),
                   c('Longitude','Lon','lon'),
                   c('Altitude','Altitude..m.','alt'),
                   c('Distance','Distance..km.','dist'))
  
  par(mfrow=c(length(measures),1),mar=c(4,7,4,2))
  for(m in 1:length(measures))
  {
    plot(sbn.overlap[,measures[[m]][2]],type='l',col='red',las=1,xlab='Time',ylab='',main=measures[[m]][1])
    mtext(measures[[m]][1],2,5,cex=.75)
    lines(wahoo.overlap[,measures[[m]][3]],col='blue',lty=2)
    legend('topright',legend=c('GT-31','Wahoo'),col=c('red','blue'),lty=c(1,2),bty='n')
  }
  
  # Combine the data
  sbn.overlap[,paste(names(wahoo.overlap),'.wahoo',sep='')] <- 0
  
  hits <- 0
  misses <- 0
  multiples <- 0
  
  for(i in 1:nrow(sbn.overlap))
  {
    target <- sbn.overlap[i,'FixedTime']
    match <- subset(wahoo.overlap,FixedTime > (target-.5) & FixedTime < (target+.5))
    if (nrow(match) == 1)
    {
      sbn.overlap[i,paste(names(wahoo.overlap),'.wahoo',sep='')] <- match[1,]
      hits <- hits+1
    } else if(nrow(match) == 0) {
      misses <- misses+1
    } else {
      multiples <- multiples + 1
      sbn.overlap[i,paste(names(wahoo.overlap),'.wahoo',sep='')] <- match[1,]
    }
  }
  
  hits.usage <- round(hits/nrow(sbn.overlap)*100,4)
  misses.usage <- round(misses/nrow(sbn.overlap)*100,4)
  multiples.usage <- round(multiples/nrow(sbn.overlap)*100,4)
  
  cat('Combining time points...\n\t',hits.usage,'% Hit, ',misses.usage,'% Missed, ',multiples.usage,'% Matched Multiple',sep='')
  
  # Pull out the header from the GPX file so we can use it
  header.len <- system(paste('grep -n "<trkseg>"',sub('pwx','gpx',pwxFile),'|cut -f1 -d:'),intern=T)
  system(paste('head -n',header.len,sub('pwx','gpx',pwxFile),'>',outFile))

  # Construct template for output trackpoints
  out <- '     <trkpt lon="%f" lat="%f">
         <ele>%f</ele>
         <time>%s</time>
         <extensions>
           <gpxtpx:TrackPointExtension>
             <gpxtpx:hr>%i</gpxtpx:hr>
           </gpxtpx:TrackPointExtension>
         </extensions>
       </trkpt>'
  
  # Open output file
  f <- file(outFile,open='at',)
  
  do.hr <- 'hr.wahoo' %in% names(sbn.overlap)
  do.cad <- 'cad.wahoo' %in% names(sbn.overlap)
  
  # Loop through output row, write to file
  for (i in 1:nrow(sbn.overlap))
  {
    lon <- sbn.overlap[i,'Lon']
    lat <- sbn.overlap[i,'Lat']
    out <- sprintf('\n    <trkpt lon="%f" lat="%f">\n',lon,lat)
    
    ele <- sbn.overlap[i,'Altitude..m.']
    if (is.numeric(ele))
    {
      out <- sprintf('%s      <ele>%f</ele>\n',out,ele)
    }
    
    time <- format(sbn.overlap[i,'FixedTime'],'%Y-%m-%dT%H:%M:%SZ')
    out <- sprintf('%s      <time>%s</time>\n',out,time)
    
    if (do.hr | do.cad)
    {
      out <- sprintf('%s      <extensions>
        <gpxtpx:TrackPointExtension>\n',out)
      
      if (do.hr)
      {
        hr <- sbn.overlap[i,'hr.wahoo']
        if (is.numeric(hr) & !is.na(hr))
        {
          out <- sprintf('%s          <gpxtpx:hr>%i</gpxtpx:hr>\n',out,hr)
        }
      }
      
      if (do.cad)
      {
        cad <- sbn.overlap[i,'cad.wahoo']
        if (is.numeric(cad) & !is.na(cad))
        {
          out <- sprintf('%s         <gpxtpx:cad>%i</gpxtpx:cad>\n',out,cad)
        }
      }
      
      out <- sprintf('%s        </gpxtpx:TrackPointExtension>
      </extensions>\n',out)
    }
      
    cat(sprintf('%s    </trkpt>',out),file=f)
  }
  
  # Add footer and close file
  end <- "  </trkseg>
 </trk>
</gpx>"
  cat(end,file=f)
  close(f)
}
