###################################################################
###################################################################
#Series of functions to analyze the Microwave Telemetry X-tags, this code can be adapted to other tag types, but was designed for the X-tags
#These functions are adapted from the analyzepsat package written by Ben Galuardi, updated to run on R>2.15
#also changed the method for importing data from R, which greatly decreased processing time
###################################################################
#
#Updated 1/13/2016 by C. Tribuzio
#
###################################################################

#########################
#required libraries
#libs<-c("readxl","animation","date","MASS","maps","mapdata","sp","fields","ellipse","maptools","rgdal","raster")
#if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
#lapply(libs,library,character.only=T)

###################################################################
#FUNCTION FOR DOWNLOADING BATHYMETRIC COVERAGE
###################################################################
##############
#get bathymetric data
#' Dowload bathymetric coverage
#' 
#' @param longlow bounding longitude
#' @param lonhigh bounding longitude
#' @param latlow bounding latitude
#' @param lathigh bounding latitude
#' @param folder directory, defaults to tempdir()
#' @param seaonly gets rid of land topography 
#' @param res resolution of output data, here either 1 or 0.5, larger number is smaller file size, low resolution
get.bath.data<-function (lonlow, lonhigh, latlow, lathigh, folder = tempdir(), 
                         seaonly = T, res = c(0.5, 1)) 
{
  require(ncdf)
  rot90 <- function(A) {
    n <- dim(A)[2]
    A <- t(A)
    A[n:1, ]
  }
  fliplr <- function(A) {
    A = (A)[(ncol(A)):1, ]
    A
  }
  fname = paste(folder, "request.nc", sep = "/")
  if (res == 1) {
    cat("ERDDAP downloading: Topography, Smith & Sandwell v11.1, 1/60-degree \n UCSD   (Dataset ID: usgsCeSS111)")
    opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSS111.htmlTable?topo[(LATHIGH):1:(LATLOW)][(LONLOW):1:(LONHIGH)]"
    bathid = "topo"
  }
  if (res == 0.5) {
    cat("ERDDAP downloading: Topography, SRTM30+ Version 1.0, 30 arc second, Global \n \tScripps   (Dataset ID: usgsCeSrtm30v1)")
    opt = "http://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeSrtm30v1.nc?topo[(LATHIGH):(LATLOW)][(LONLOW):(LONHIGH)]&.draw=surface&.vars=longitude|latitude|topo&.colorBar=|||||"
    bathid = "topo"
  }
  opt <- sub("LATLOW", latlow, opt)
  opt <- sub("LATHIGH", lathigh, opt)
  opt <- sub("LONLOW", lonlow, opt)
  opt <- sub("LONHIGH", lonhigh, opt)
  download.file(opt, fname, mode = "wb")
  nc <- open.ncdf(fname)
  lon <- as.numeric(get.var.ncdf(nc, varid = "longitude"))
  lat <- as.numeric(get.var.ncdf(nc, varid = "latitude"))
  bdata = get.var.ncdf(nc, varid = bathid)
  if (res == 1) 
    bdata = rot90(bdata)
  if (res == 0.5) 
    bdata = rot90(bdata)
  lat = lat[order(lat)]
  if (seaonly == T) 
    bdata[bdata >= 0] = 1
  bathy = list(lon = lon, lat = lat, data = bdata)
  bathy
  saveRDS(bathy,file="bathy.rds")
}

###################################################################
#FUNCTIONS FOR IMPORTING DATA FROM THE EXCEL FILES PROVIDED BY MICROWAVE TELEMETRY
###################################################################

#ARCHIVED DATA, THOSE IN WHICH THE HIGH RESOULTION DATA IS DOWNLOADED, 
#THESE ARE IN A DIFFERENT FORMAT FROM TRANSMITTED DATA

#datT and xT are last day and last location of argos transmissions, use these if no transmissions
#in the for loop it's easier to either make a list of last dates/locs, or to just add a fake one to the ARGOS Data sheet in the excel file

#' Get tagging location
#' 
#' @param tagLocfile Path to the .txt file listing all tags release information
#' @param tagID ARGOS ID of the tag being run
#' @param tyear Year which the tag was deployed, for use in looping through tags
.getTaggingLoc<-function (tagLocfile, tagID, tyear) {
  tab = read.table(tagLocfile, header = F)
  i = tab[(tab[, 1] == tagID) & (tab[, 2] == tyear), ]
  i = as.numeric(i)
  day0 = mdy.date(i[3], i[4], i[2])
  x0 = rev(i[5:6])
  return(x0)
}

#' Get tag release date
#' 
#' @param tagLocfile Path to the .txt file listing all tags release information
#' @param tagID ARGOS ID of the tag being run
#' @param tyear Year which the tag was deployed, for use in looping through tags
.getTaggingDay<-function (TagLocfile, tagID, tyear) {
  tab = read.table(TagLocfile, header = F)
  i = tab[(tab[, 1] == tagID) & (tab[, 2] == tyear), ]
  i = as.numeric(i)
  day0 = mdy.date(i[3], i[4], i[2])
  return(day0)
}

#' Get ARGOS transmission dates and locations
#' 
#' @param xlsfile Path to the excell workboot for tagID
.getArgosPSAT<-function (xlsfile) {
  res = read_excel(xlsfile, "Argos Data", skip = 1)
  adata = res[, 1:4]
  names(adata) = c("Date-Time", "LC", "Lat", "Lon")
  res = res[!is.na(res[, 1]), ]
  res = res[!is.na(res[, 2]), ]
  d1 = dim(res)[1]
  d2 = dim(res)[2]
  adata
}

#' Get last day tag transmitted
#' 
#' @param arfos ARGOS transmission data output from .getArgosPSAT
.getLastDay<-function (argos) {
  adates = argos[, 1]
  a1 = min(adates, na.rm = T)
  a1 = as.POSIXlt(a1)
  aa = unclass(unlist(a1))
  dayT = mdy.date(aa[5] + 1, aa[4], aa[6] + 1900)
  dayT
}

#' Reformats date of last transmission for use in function
#' 
#' @param dayT date of last transmission
.getDayTb<-function (dayT) {
  dayTb = ISOdatetime(date.mdy(dayT)[[3]], date.mdy(dayT)[[1]], 
                      date.mdy(dayT)[[2]], 0, 0, 0)
  dayTb
}

#' Get location of last transmission
#' 
#' @param arfos ARGOS transmission data output from .getArgosPSAT
.getLastLoc<-function (argos) {
  res = argos
  i = 1
  while ((res[i, 2] == "A") || (res[i, 2] == "B") || (res[i, 
                                                          2] == "Z")) {
    i = i + 1
  }
  xT = c(as.numeric(res[i, 3:4]))
  xT[2] = -1 * xT[2]
  if (i > 5) {
    print("Pop-off location may be incorrect")
  }
  xT
}

#' Get estimated daily location data
#' 
#' @param xlsfile Path to the excell workboot for tagID
#' @param x0 Tagging location from .getTaggingLoc
#' @param xT Location of last transmission from .getLastLoc
#' @param day0 Taggind date from .getTaggingDay
#' @param dayT Date of last transmission from .getLastDay
.getMWTxy<-function (xlsfile, x0, xT, day0, dayT) {
  MWTxy = read_excel(xlsfile, sheet = "Lat&Long", skip = 1)
  MWTxy = MWTxy[!is.na(MWTxy[, 1]), 1:3]
  names(MWTxy) = c("Date", "Lat", "Long")
  fulldates = seq(day0, dayT)
  len = length(fulldates)
  MWTdata = as.data.frame(array(NA, c(len, 5)))
  Years = as.numeric(format.Date(MWTxy$Date, "%Y"))
  Months = as.numeric(format.Date(MWTxy$Date, "%m"))
  Days = as.numeric(format.Date(MWTxy$Date, "%d"))
  MWTdates = mdy.date(Months, Days, Years)
  didx = match(MWTdates, fulldates)
  didx = didx[!is.na(didx)]
  MWTdata[, 1:3] = cbind(date.mdy(fulldates)[[3]], date.mdy(fulldates)[[1]],
                         date.mdy(fulldates)[[2]])
  MWTdata[didx, 4:5] = MWTxy[, 2:3]
  MWTdata[, 5] = -1 * (MWTdata[, 5])
  MWTdata[1, 4:5] = (x0)
  MWTdata[len, 4:5] = xT
  names(MWTdata) = c("Year", "Month", "Day", "Lat", "Lon")
  MWTdata
}

#' Get estimated daily location data
#' 
#' @param xlsfile Path to the excell workboot for tagID
#' @param day0b Reformatted tagging date, created during run of getMWTarch
#' @param dayTb Reformatted date of last transmission from .getLastDay
.getSRSS<-function (xlsfile, day0b, dayTb){
  res = read_excel(xlsfile, sheet = "Sunrise and Sunset Times", 
                   skip = 1)
  res = res[!is.na(res[, 1]), c(1, 2, 4)]
  len = length(res[, 1])
  srssdates = as.POSIXct(strptime(res[, 1], "%b %d, %Y"))
  SR = strptime(paste(res[, 1], res[, 2], sep = " "), "%b %d, %Y %H:%M:%S")
  SR = SR$hour * 60 + SR$min
  SS = strptime(paste(res[, 1], res[, 3], sep = " "), "%b %d, %Y %H:%M:%S")
  SS = SS$hour * 60 + SS$min
  tidx = srssdates >= day0b & srssdates <= dayTb
  SRSS = data.frame(Date = srssdates[tidx], SR = SR[tidx],
                    SS = SS[tidx])
  SRSS[which(SRSS[, 3] < 500), 3] = SRSS[which(SRSS[, 3] < 
                                                 500), 3] + 1400
  SRSS[which(SRSS[, 2] > 1400), 3] = SRSS[which(SRSS[, 
                                                     2] > 1400), 2] - 500
  SRSS
}

#' Function to extract MWT archived tag data from excel workbooks, formats data for 
#' running models
#' 
#' @param xlsfile Path to the excell workboot for tagID
#' @param tagID ARGOS ID of the tag being run
#' @param tyear Year which the tag was deployed, for use in looping through tags
#' @param tagLocfile Path to the .txt file listing all tags release information
#' @param xT Location of last transmission from .getLastLoc
#' @param dayT Date of last transmission from .getLastDay
getMWTarch<-function (xlsfile, tagID, tyear, taglocfile, dayT = NULL, xT = NULL) { 
  tabs = excel_sheets(xlsfile)
  atabs<-tabs[grep("Archival",tabs)]
  adat = NULL
  print("Retrieving Archival Light, Temp and Depth ")
  for (i in 1:length(atabs)) {
    print(paste("retrieving archival record ", i, sep = ""))
    temp = read_excel(xlsfile, sheet = atabs[i], skip = 1)
    temp = temp[2:nrow(temp), ]
    names(temp) = c("Date", "Tval", "Pval", "light", "extT","depth")
    adat = rbind(adat, temp)
  }
  names(adat) = c("Date", "Tval", "Pval", "light", "extT","depth")
  print("Converting date")
  adat$uday = as.POSIXct(trunc(as.POSIXct(adat$Date,format="%m/%d/%Y %H:%M",tz="UTC"), "days")) #maybe add a column for local date/time, will probably require changing fields later in code
  print("Calculating max temp/depth")
  maxz = tapply(adat$depth, adat$uday, min)
  maxt = tapply(adat$extT, adat$uday, max)
  print("Retrieving tag release info")
  x0 = .getTaggingLoc(taglocfile, tagID, tyear)
  x0 = rev(x0)
  x0[1]<-x0[1]*-1
  day0 = .getTaggingDay(taglocfile, tagID, tyear)
  day0b = ISOdatetime(date.mdy(day0)[[3]], date.mdy(day0)[[1]], date.mdy(day0)[[2]], 0, 0, 0)
  if (!is.null(dayT)) {
    dayTb = .GetDayTb(dayT)
  }
  else {
    print("Retrieving tag pop off info")
    argos = .getArgosPSAT(xlsfile)
    dayT = .getLastDay(argos)
    dayTb = .getDayTb(dayT)
  }
  if (is.null(xT)) {
    #argos = .GetArgosPSAT(xlsfile, odbc = F) #this needs to be active if dayT!=NULL, but all our data is set up to use dayT==NULL at this time
    xT = .getLastLoc(argos)
  }
  print("Reading MWT produced locations")
  MWTxy = .getMWTxy(xlsfile, x0, xT, day0, dayT)
  fulldates = seq(day0, dayT)
  print("Reading sunset/sunrise times")
  SRSS = .getSRSS(xlsfile, day0b, dayTb)
  dataout = list(tagID = tagID, x0 = x0, day0 = day0, day0b = day0b, 
                 xT = xT, dayT = dayT, dayTb = dayTb, fulldates = fulldates, 
                 SRSS = SRSS, MWTxy = MWTxy, LTD = adat, maxt = maxt, 
                 maxz = maxz)
  dataout$LTD = na.omit(dataout$LTD)
  mmtdates = as.POSIXct(trunc(as.POSIXct(unique(dataout$LTD$uday)), 
                              "day"), tz = "GMT")
  dataout$mmt = data.frame(date = mmtdates, minz = tapply(dataout$LTD$extT, 
                                                          dataout$LTD$uday, min), maxz = tapply(dataout$LTD$extT, 
                                                                                                dataout$LTD$uday, max))
  dataout$mmz = data.frame(date = mmtdates, minz = tapply(dataout$LTD$depth, 
                                                          dataout$LTD$uday, max), maxz = tapply(dataout$LTD$depth, 
                                                                                                dataout$LTD$uday, min))
  dataout$maxz = dataout$mmz[, 3]
  dataout$maxt = dataout$mmt[, 3]
  class(dataout) = c("MWTpsat", "list")
  rm(adat, tagID, x0, day0, day0b, xT, dayT, dayTb, fulldates, 
     SRSS, MWTxy, maxt, maxz, atabs, tabs, i)
  dataout
}

###################################################################
#FUNCTIONS FOR PREPARING DATA FOR kftrack
###################################################################
#for archived data files only

#' Function cleans up tag data to prepare it to run in kftrack
#' 
#' @param tag output file from getMWTarch
#' @param xmin minimum bounding longitude
#' @param xmax maximum bounding longitude
#' @param ymin minimum bounding latitude
#' @param ymax maximum bounding latitude
#' @param keepall logical to keep values outside of bounds
#' @param sst.depth not sure what this does, we aren't using SST
#' @param use.minmax logical to use x/y min/max
prepf_arch<-function (tag, xmin = -100, xmax = 0, ymin = 10, ymax = 55, keepall = F, 
                      sst.depth = NULL, use.minmax = F) {
  dim1 = dim(tag$LTD)[1]
  dim2 = dim(tag$LTD)[2]
  Temp = as.data.frame(tag$LTD[1:(dim1),5])
  Z = as.data.frame(tag$LTD[1:(dim1),6])
  loc.init = c(date.mdy(tag$day0)$year, date.mdy(tag$day0)$month, 
               date.mdy(tag$day0)$day, (tag$x0))
  loc.last = c(date.mdy(tag$dayT)$year, date.mdy(tag$dayT)$month, 
               date.mdy(tag$dayT)$day, (tag$xT))
  locs = tag$MWTxy
  len = length(locs[, 4])
  locs[1, 1:5] = loc.init
  locs[len, 1:5] = loc.last
  locs[, 4:5] = rev(locs[, 4:5])
  dates = rev(locs[, 1:3])
  if (is.null(sst.depth) == F) 
    Temp[Z <= (sst.depth)] = NaN
  maxz = apply(Z, 1, min, na.rm = T)
  maxz[!is.finite(maxz)] = 0
  maxt = apply(Temp, 1, max, na.rm = T)
  if (use.minmax) {
    mdates = seq(tag$day0b, tag$dayTb, "day")
    attributes(mdates)$tzone = "GMT"
    mdates = as.POSIXct(trunc(mdates, "day"))
    tdates = tag$mmt[, 1]
    pdates = tag$mmz[, 1]
    tidx = na.omit(match(tdates, mdates))
    pidx = na.omit(match(pdates, mdates))
    maxz = maxt = numeric(nrow(tag$MWTxy))
    maxz[pidx] = tag$mmz[, 3]
    maxt[tidx] = tag$mmt[, 3]
    maxz2 = apply(Z, 1, min, na.rm = T)
    maxz2[!is.finite(maxz2)] = 0
    maxt2 = apply(Temp, 1, max, na.rm = T)
    maxz[is.na(maxz)] = maxz2[is.na(maxz)]
    maxt[is.na(maxt)] = maxt2[is.na(maxt)]
  }
  dat = as.data.frame(cbind(dates, (locs[, 4:5]), maxt[1:len], 
                            maxz[1:len]))
  names(dat)[4:7] = c("Lon", "Lat", "SST", "maxD")
  if (keepall) {
    dat = dat[!is.na(dat[, 5]), ]
    dat = dat[!is.na(dat[, 4]), ]
  }
  else {
    dat[is.nan(dat[, 5]), 5] = NA
    dat[is.nan(dat[, 4]), 4] = NA
    dat[!is.na(dat[, 5]) & dat[, 5] < ymin, 5] = NA
    dat[!is.na(dat[, 5]) & dat[, 5] > ymax, 5] = NA
    dat[!is.na(dat[, 4]) & dat[, 4] > xmax, 4] = NA
    dat[!is.na(dat[, 4]) & dat[, 4] < (xmin), 4] = NA
    dat = dat[!is.na(dat[, 5]), ]
    dat = dat[!is.na(dat[, 4]), ]
    dat[is.na(dat[, 6]), 6] = -1
  }
  rm(Temp, maxz, dates, locs, Z, dim1, dim2, tag)
  attr(dat, "Header") = "#Input data frame for kftrack/kfsst/ukfsst"
  dat
}

###################################################################
#RUNNING THE kftrack MODEL, right now, need to have the kftrack folder copied to correct directory
#this is so it can point to the /admb folder and run the models, need to clean this up with Pete's help?
###################################################################
#.generate.dat.file function needed for kftrack
#' Background function that works inside kftrack
#' 
#' @param data from the output of prepf_arch
#' @param file admb .dat file
#' @param fix.first logical if the model should leave the first (i.e., tagging location) location fixed
#' @param fix.last logical if the model should leave the last (i.e., recovery or last transmission) location fixed
#' @param theta.active admb inputs
#' @param theta.init admb inputs
.generate.dat.file<-function(data, file="kftrack.dat", fix.first=TRUE, 
                             fix.last=TRUE, 
                             theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
                                            sx.active, sy.active, a0.active, b0.active, vscale.active), 
                             theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
                                          b0.init, vscale.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
                             by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, 
                             vscale.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, 
                             sy.init=1.5, a0.init=0.001, b0.init=0, vscale.init=1, var.struct="solstice", dev.pen=0.0){
  
  "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  tostr<-function(x)paste(as.numeric(x), collapse="\t")
  var.flags<-switch(var.struct, "uniform"=c(0,0,0,0), "solstice"=c(1,0,0,0), 
                    "daily"=c(0,1,dev.pen,0), "specified"=c(0,0,0,1), 
                    warning("No matching variance structure found"))
  
  header<-"#Auto generated data file from R-KFtrack \n#"%+%date()%+%"\n#\n"%+%
    "# Number of data points\n  "%+%
    tostr(nrow(data))%+%"\n#\n"%+%
    "# 1 if first point is true release position; 0 otherwise\n  "%+%
    tostr(fix.first)%+%"\n#\n"%+%
    "# 1 if last point is true recapture position; 0 otherwise\n  "%+%
    tostr(fix.last)%+%"\n#\n"%+%
    "# active parameters \n# u\tv\tD\tbx\tby\tsx\tsy\ta0\tb0\tvscale\n  "%+%
    tostr(theta.active)%+%"\n#\n"%+%
    "# initial values \n# u\tv\tD\tbx\tby\tsx\tsy\ta0\tb0\tvscale\n  "%+%
    tostr(theta.init)%+%"\n#\n"%+%
    "# latitude errors \n# cos\tdev\tdeviation penalty weight\tspecified flag\n  "%+%
    tostr(var.flags)%+%"\n#\n"%+%
    "# Positions \n# day\tmonth\tyear\tlong\tlati\tvlon\tvlat\tvlonlat (last three optional)\n  "
  cat(header, file=file)
  write.table(data, file=file, row.names=FALSE, col.names=FALSE, 
              sep="\t", eol="\n  ", append=TRUE)
  cat("\n", file=file, append=TRUE)
  class(header)<-"kfhead"
  return(header)
}

#####
#read.output necessary for kftrack to run
#' Background function that works inside kftrack
#' 
#' @param data from the output of prepf_arch
#' @param fix.first logical if the model should leave the first (i.e., tagging location) location fixed
#' @param fix.last logical if the model should leave the last (i.e., recovery or last transmission) location fixed
#' @param theta.active admb inputs
#' @param theta.init admb inputs
.read.output<-function(data, fix.first=TRUE, fix.last=TRUE, 
                       theta.active=c(u.active, v.active, D.active, bx.active, by.active, 
                                      sx.active, sy.active, a0.active, b0.active, vscale.active), 
                       theta.init=c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
                                    b0.init, vscale.init), u.active=TRUE, v.active=TRUE, D.active=TRUE, bx.active=TRUE, 
                       by.active=TRUE, sx.active=TRUE, sy.active=TRUE, a0.active=TRUE, b0.active=TRUE, 
                       vscale.active=TRUE, u.init=0, v.init=0, D.init=100, bx.init=0, by.init=0, sx.init=.5, 
                       sy.init=1.5, a0.init=0.001, b0.init=0, vscale.init=1, var.struct="solstice", dev.pen=0.0){
  getpar<-function(what, file){
    txt<-readLines(file)
    return(as.numeric(strsplit(txt[grep(what, txt)+1], split=" ")[[1]]))
  }
  getrep<-function(what, file){
    txt<-readLines(file)
    return(as.numeric(strsplit(txt[grep(what, txt)], split=" ")[[1]][3]))
  }
  theta.names<-c("u","v","D","bx","by","sx","sy")
  if(var.struct=="solstice"){theta.names<-c(theta.names, "a0", "b0")}
  if(var.struct=="specified"){theta.names<-c(theta.names, "vscale")}
  if(file.access("kftrack.par")!=0){
    warning("File \"kftrack.par\" not found (possibly because there was no solution to minimization problem)", call.=FALSE)
    npar<-NA; nlogL<-NA; max.grad.comp<-NA; estimates<-NA
  }else{
    tmp<-as.numeric(scan("kftrack.par", what="character", 16, quiet=TRUE)[c(6,11,16)])
    npar<-tmp[1]; nlogL<-tmp[2]; max.grad.comp<-tmp[3]
    estimates<-sapply(c("uu","vv","D","bx","by","vx","vy"), getpar, file="kftrack.par")
    if(var.struct=="solstice"){estimates<-c(estimates,sapply(c("a0", "b0"), getpar, file="kftrack.par"))}
    if(var.struct=="specified"){estimates<-c(estimates,sapply(c("vscale"), getpar, file="kftrack.par"))}
    names(estimates)<-theta.names
  }
  if(file.access("kftrack.rep")!=0){
    warning("File \"kftrack.rep\" not found", call.=FALSE)
    spd<-NA; hdg<-NA
  }else{
    spd<-getrep("spd", "kftrack.rep")
    hdg<-getrep("hdg", "kftrack.rep")
  }
  if(file.access("kftrack.std")!=0){
    warning("File \"kftrack.std\" not found (possibly the hessian was not estimated)", call.=FALSE)
    std.dev<-NA    
  }else{
    dat<-read.table("kftrack.std", skip=1)
    tmp<-dat[dat[,2]%in%c("sduu","sdvv","sdD","sdbx","sdby","sdvx","sdvy"), 3:4]
    if(var.struct=="solstice"){
      if(theta.active[8]){tmp<-rbind(tmp,dat[dat[,2]=="a0",3:4])}else{tmp<-rbind(tmp,c(0, 0))}
      if(theta.active[9]){tmp<-rbind(tmp,dat[dat[,2]=="b0",3:4])}else{tmp<-rbind(tmp,c(0, 0))}
    }
    if(var.struct=="specified"){
      if(theta.active[10]){tmp<-rbind(tmp,dat[dat[,2]=="vscale",3:4])}else{tmp<-rbind(tmp,c(1, 0))}
    }
    std.dev<-tmp[,2]
    names(std.dev)<-paste("sd.",theta.names, sep="")
  }
  ta<-theta.active[1:7]
  if(var.struct=="specified"){
    ta[6:7]<-0
    estimates<-estimates[!names(estimates)%in%c('sx','sy')]  
    std.dev<-std.dev[!names(std.dev)%in%c('sd.sx','sd.sy')]  
  }
  #mptfilename<-"mpt.dat"
  mptfilename<-paste("mpt_", paste(as.numeric(
    c(ta,var.struct=="solstice",var.struct=="daily")), collapse=""),".dat",sep="")
  if(file.access(mptfilename)!=0){warning("File not found")}else{
    tmp<-read.table(mptfilename,skip=3, header=FALSE)
    name<-strsplit(readLines(mptfilename, 3)[3], split=" ")[[1]]  
    colnames(tmp)<-name[!name%in%c("","#")]
    nominal.track<-cbind(x=tmp$ox, y=tmp$oy)
    pred.track<-cbind(x=tmp$px,y=tmp$py)
    most.prob.track<-cbind(x=tmp$smoothX, y=tmp$smoothY)
    days.at.liberty<-cumsum(tmp$dt)
    date<-matrix(as.numeric(unlist(strsplit(as.character(tmp$date), "/"))), ncol=3, byrow=TRUE) 
    colnames(date)<-c("year", "month", "day")
    var.most.prob.track<-cbind(tmp$Psmooth11,tmp$Psmooth12,tmp$Psmooth21,tmp$Psmooth22)
  }
  return(list(npar=npar, nlogL=nlogL, max.grad.comp=max.grad.comp, estimates=estimates, 
              std.dev=std.dev, nominal.track=nominal.track, pred.track=pred.track, 
              most.prob.track=most.prob.track, var.most.prob.track=var.most.prob.track, 
              days.at.liberty=days.at.liberty, date=date, spd=spd, hdg=hdg))
}

####
#' Background function that works inside kftrack
#' 
#' @param cmd gives platform to admb code

.sys <-
  function (cmd) 
    if (.Platform$OS.type == "windows") {
      shell(cmd, invisible = TRUE)
    } else {
      system(cmd)
    }

######
#' Kalman filter model estimation of geolocation, wrapper for admb function
#' 
#' @param kfdata from the output of prepf_arch
#' @param file admb .dat file
#' @param fix.first logical if the model should leave the first (i.e., tagging location) location fixed
#' @param fix.last logical if the model should leave the last (i.e., recovery or last transmission) location fixed
#' @param theta.active admb inputs
#' @param theta.init admb inputs
#' @param kf.dir Path to location of admb folder
kftrack <-function (kfdata, fix.first = TRUE, fix.last = TRUE, theta.active = c(u.active, v.active, D.active, bx.active, by.active, sx.active, sy.active, 
                                                                                a0.active, b0.active, vscale.active), 
                    theta.init = c(u.init, v.init, D.init, bx.init, by.init, sx.init, sy.init, a0.init, 
                                   b0.init, vscale.init), u.active = TRUE, v.active = TRUE, 
                    D.active = TRUE, bx.active = TRUE, by.active = TRUE, sx.active = TRUE, 
                    sy.active = TRUE, a0.active = TRUE, b0.active = TRUE, vscale.active = TRUE, 
                    u.init = 0, v.init = 0, D.init = 100, bx.init = 0, by.init = 0, 
                    sx.init = 0.5, sy.init = 1.5, a0.init = 0.001, b0.init = 0, 
                    vscale.init = 1, var.struct = "solstice", dev.pen = 0, save.dir = NULL, 
                    admb.string = "",kf.dir) 
{
  olddir <- getwd()
  dirname <- ifelse(is.null(save.dir), "_kftrack_temp_", save.dir)
  dir.create(dirname)
  setwd(dirname)
  #if (!is.null(save.dir)) {
  #  mptfilename <- paste("mpt_", paste(as.numeric(c(theta.active[1:7], 
  #                                                  var.struct == "solstice", var.struct == "daily")), 
  #                                     collapse = ""), ".dat", sep = "")
  #  unlink(c(mptfilename, "kftrack.par", "kftrack.rep", "kftrack.std"))
  #}
  header <- .generate.dat.file(kfdata, "kftrack.dat", fix.first, 
                               fix.last, theta.active, theta.init, var.struct = var.struct, 
                               dev.pen = dev.pen)
  #filename <- dir(kf.dir., pattern = "kf")
  #if (.Platform$OS.type == "windows") {
  file.copy(paste(kf.dir, "/kftrack.exe",sep = ""), "kftrack.exe", TRUE)
  error.code <- .sys(paste("kftrack.exe", " ", admb.string, sep = ""))
  #}
  #else {
  #  system(paste("cp ", kf.dir., filename, " ", filename, sep = ""))
  #  .sys("chmod +x kftrack")
  #  error.code <- system(paste("./", filename, " ", admb.string, 
  #                             sep = ""))
  #}
  kf.o <- .read.output(kfdata, fix.first, fix.last, theta.active, 
                       theta.init, var.struct = var.struct, dev.pen = dev.pen)
  kf.o$nobs <- nrow(kfdata)
  kf.o$header <- header
  kf.o$fix.first <- fix.first
  kf.o$fix.last <- fix.last
  kf.o$theta.active <- theta.active
  kf.o$theta.init <- theta.init
  kf.o$var.struct <- var.struct
  kf.o$dev.pen <- dev.pen
  kf.o$call <- match.call()
  kf.o$data.name <- deparse(substitute(kfdata))
  kf.o$error.code <- error.code
  setwd(olddir)
  if (is.null(save.dir)) {
    unlink(dirname, recursive = TRUE)
  }
  class(kf.o) <- "kftrack"
  return(kf.o)
}

###################################################################
#FUNCTIONS FOR PREPARING DATA FOR btrack
###################################################################
#' Background function that works inside prepb
#' 
#' @param vec vector of values resulting from prepf[,6]
#' @param span not sure what this means
.fill.vals<-function (vec, span = 0.25) {
  len = length(vec)
  vlen = 1:len
  fidx1 = is.infinite(vec)
  fidx2 = is.na(vec)
  fidx3 = is.nan(vec)
  fidx = as.logical(fidx1 + fidx2 + fidx3)
  vec[fidx] = NA
  if (any(fidx == T)) {
    ltmp = loess(vec ~ vlen, span = span)
    vec[fidx] = predict(ltmp, newdata = vlen[fidx])
  }
  vec
}

#' Prepares kftrack results for the btrack function
#' 
#' @param kfit kftrack results
#' @param prepf prepf_arch results
#' @param span passed to internal function
prepb<-function (kfit, prepf, span = NULL) {
  if (any(prepf[, 7] > 0)) 
    print("You have positive Depth values! Please correct and re-run this function")
  if (any(prepf[, 4] > 180)) 
    print("You have Longitude values greater than 180. Please manipulate your longitude values to be -180 to 180.")
  fmat = as.data.frame(cbind(prepf[, rev(1:3)], kfit$var.most.prob.track, 
                             kfit$most.prob.track, prepf[, c(7, 6)]))
  names(fmat) = c("Year", "Month", "Day", "V11", "V12", "V21", 
                  "V22", "Lon_E", "Lat_N", "max_depth", "SST")
  fmat
}

###################################################################
#RUNNING THE btrack CORRECTION
###################################################################
#' Background function that operates within make.btrack
#' 
#' @param vec results of prepb
#' @param npoints number of points to resample
#' @param ci confidence interval
.get.samp<-function (vec, npoints, ci = 0.95) {
  vec = as.numeric(vec)
  Sigma <- matrix(vec[1:4], 2, 2) * ci
  mu <- c(vec[5:6])
  if (sum(Sigma) > 0) {
    ndata <- mvrnorm(npoints, mu, Sigma)
    return(ndata)
  }
}

#' Background function that operates within make.btrack and used to pull bathy info at other times
#' 
#' @param lon longitude
#' @param lat latitude
#' @param BATH loaded bathymetric coverage
.get.bath<-function (lon, lat, BATH) {
  X = as.vector(BATH$lon)
  Y = as.vector(BATH$lat)
  xidx = which.min((lon - X)^2)
  yidx = which.min((lat - Y)^2)
  BATH$data[yidx, xidx]
}

#' Background function that operates within make.btrack
#' 
#' @param lon1 longitude
#' @param lat1 latitude
#' @param lon2 logitude
#' @param lat2 latitude
#' @param samp result of .get.samp
.get.min2<-function (lon1, lat1, lon2, lat2, samp) {
  idx <- which.min((lon1 - samp[, 1])^2 + (lat1 - samp[, 2])^2 + 
                     (lon2 - samp[, 1])^2 + (lat2 - samp[, 2])^2)
  c(samp[idx, 1], samp[idx, 2])
}

#' Background function that operates within make.btrack
#' 
#' @param samp result of .get.samp
.denselect<-function (samp) {
  samp = samp[!is.na(samp[, 1]), ]
  samp = samp[!is.na(samp[, 2]), ]
  dd1 = density(samp[, 1])
  idx1 = dd1$y == max(dd1$y)
  xout = dd1$x[idx1]
  dd2 = density(samp[, 2])
  idx2 = dd2$y == max(dd2$y)
  yout = dd2$x[idx2]
  cbind(xout, yout)
}

#' Background function that operates within make.btrack
#' 
#' @param fmat results of prepb
#' @param bathy loaded bathymetric coverage
#' @param save.samp Should the sampled points be saved, makes for much larger file
#' @param mintype not exactly sure about this, it's on my list to investigate further
#' @param ci confidence interval
#' @param npoints number of points to resample
#' @param fulldist should the full distribution of points be used in variance calcs
make.btrack<-function (fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, 
                       npoints = 300, fulldist = T) {
  len = length(fmat[, 1])
  ntrack = as.data.frame(matrix(0, len, 6))
  ntrack[1, ] = c(0, 0, 0, 0, fmat[1, 8:9])
  ntrack[len, ] = c(0, 0, 0, 0, fmat[len, 8:9])
  sptmp = NULL
  for (i in 2:(length(fmat[, 1]) - 1)) {
    print(paste("Bathymetric point ", i, sep = ""))
    point = fmat[i, ]
    samp = .get.samp(point[4:9], npoints, ci = ci)
    samp.bath = sapply(1:length(samp[, 1]), function(j) .get.bath(samp[j, 
                                                                       1], samp[j, 2], bathy))
    sidx = samp.bath <= as.numeric(point[10])
    samp = samp[sidx, ]
    if (length(samp[sidx]) < 3) {
      samp = sptmp[[i - 1]]
      samp[, 1] = jitter(samp[, 1])
      samp[, 2] = jitter(samp[, 2])
    }
    if (mintype == 2) 
      ntrack[i, 5:6] = .get.min2(ntrack[i - 1, 5], ntrack[i - 
                                                            1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                                 samp)
    if (mintype == 3) 
      ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                                                            1, 6], ntrack[i - 1, 5], ntrack[i - 1, 6], .denselect(samp)[1], 
                                 .denselect(samp)[2], samp)
    if (mintype == 4) 
      ntrack[i, 5:6] = .get.min3(ntrack[i + 1, 5], ntrack[i + 
                                                            1, 6], .denselect(samp)[1], .denselect(samp)[2], 
                                 samp)
    sptmp[[i]] = samp
    b.init = .get.bath(as.numeric(point[8]), as.numeric(point[9]), 
                       bathy)
    print(c(b.init - as.numeric(point[10])))
    if (b.init <= as.numeric(point[10]) & fulldist == F) {
      ntrack[i, ] = fmat[i, 4:9]
    }
    else {
      tcov = sqrt(cov(samp))
      tcov[is.nan(tcov)] = 0
      ntrack[i, 1:4] = as.vector(tcov)
    }
  }
  btrack = cbind(fmat[, 1:3], ntrack, fmat[, 10:11])
  names(btrack) = c("Year", "Month", "Day", "V11", "V12", "V21", 
                    "V22", "Lon_E", "Lat_N", "maxz", "maxt")
  attr(btrack, "Header") = "#Bathymetric corrected track"
  if (save.samp) {
    list(btrack, sptmp)
  }
  else {
    btrack
  }
}

#######################################
#runs the kftrack and btrack through iterations, summarizes outputs
#' Function that repeats kftrack and btrack through multiple iterattions and summarizes output
#' 
#' @param iter Number of iterations
#' @param xtrack Output from prepf_arch function
#' @param kf.dir Path to location of admb folder
itersf<-function(iter,xtrack,kf.dir){
  newmat<-as.data.frame(matrix(data = NA,ncol = 12))
  colnames(newmat)<-c("Year","Month","Day","V11","V12","V21","V22","Lon_E","Lat_N","maxz","maxt","i")
  kfmat<-as.data.frame(matrix(data = NA,ncol = 12))
  colnames(kfmat)<-c("Year","Month","Day","V11","V12","V21","V22","Lon_E","Lat_N","max_depth","SST","i")
  for (i in 1:iter){
    fit = kftrack(xtrack[,1:5],kf.dir=kfdir)
    fmat = prepb(fit,xtrack) 
    kfmodelout = cbind(fmat,i)
    btrack = make.btrack(fmat, bathy)
    tmodelout<-cbind(btrack,i)
    newmat<-rbind(newmat,tmodelout)
    kfmat<-rbind(kfmat,kfmodelout)
  }
  newmat<-na.omit(newmat) #removes days with no locations
  kfmat<-na.omit(kfmat)
  colnames(kfmat)[10:11]<-c("maxz","maxt")
  bathysumm<-ddply(newmat,c("Year","Month","Day"),summarize,Lat_m=mean(Lat_N),Lat_LL=quantile(Lat_N,c(0.025,0.975))[1],Lat_UL=quantile(Lat_N,c(0.025,0.975))[2],
              Lon_m=mean(Lon_E),Lon_LL=quantile(Lon_E,c(0.025,0.975))[1],Lon_UL=quantile(Lon_E,c(0.025,0.975))[2],
              V11_m=mean(V11),V11_LL=quantile(V11,c(0.025,0.975))[1],V11_UL=quantile(V11,c(0.025,0.975))[2],
              V12_m=mean(V12),V12_LL=quantile(V12,c(0.025,0.975))[1],V12_UL=quantile(V12,c(0.025,0.975))[2],
              V21_m=mean(V21),V21_LL=quantile(V21,c(0.025,0.975))[1],V21_UL=quantile(V21,c(0.025,0.975))[2],
              V22_m=mean(V22),V22_LL=quantile(V22,c(0.025,0.975))[1],V22_UL=quantile(V22,c(0.025,0.975))[2])
  colnames(bathysumm)<-c("Year","Month","Day","Lat_N","LL_Lat","UL_Lat","Lon_E","LL_Lon","UL_Lon","V11","V11LL","V11UL","V12","V12LL","V12UL",
                       "V21","V21LL","V21UL","V22","V22LL","V22UL")
  bathysumm$model<-"Bathy"
  kfsumm<-ddply(kfmat,c("Year","Month","Day"),summarize,Lat_m=mean(Lat_N),Lat_LL=quantile(Lat_N,c(0.025,0.975))[1],Lat_UL=quantile(Lat_N,c(0.025,0.975))[2],
                   Lon_m=mean(Lon_E),Lon_LL=quantile(Lon_E,c(0.025,0.975))[1],Lon_UL=quantile(Lon_E,c(0.025,0.975))[2],
                   V11_m=mean(V11),V11_LL=quantile(V11,c(0.025,0.975))[1],V11_UL=quantile(V11,c(0.025,0.975))[2],
                   V12_m=mean(V12),V12_LL=quantile(V12,c(0.025,0.975))[1],V12_UL=quantile(V12,c(0.025,0.975))[2],
                   V21_m=mean(V21),V21_LL=quantile(V21,c(0.025,0.975))[1],V21_UL=quantile(V21,c(0.025,0.975))[2],
                   V22_m=mean(V22),V22_LL=quantile(V22,c(0.025,0.975))[1],V22_UL=quantile(V22,c(0.025,0.975))[2])
  colnames(kfsumm)<-c("Year","Month","Day","Lat_N","LL_Lat","UL_Lat","Lon_E","LL_Lon","UL_Lon","V11","V11LL","V11UL","V12","V12LL","V12UL",
                       "V21","V21LL","V21UL","V22","V22LL","V22UL")
  kfsumm$model<-"KF"
  newmat2<-rbind(bathysumm,kfsumm)
  write.table(newmat,paste("modeliters_bathy",tagID,".csv",sep=""), sep=',',row.names=F)
  write.table(kfmat,paste("modeliters_kf",tagID,".csv",sep=""), sep=',',row.names=F)
  write.table(newmat2,paste("modeliters_summary",tagID,".csv",sep=""), sep=',',row.names=F)
  list("bathy_iters"= newmat, "kf_iters"=kfmat, "iter_summary" = newmat2)
}

###################################################################
#PLOTTING FUNCTIONS
###################################################################
#' Plots the bathymetric corrected estimated locations
#' 
#' @param btrack results of make.btrack
#' @param cex adjusts size of points
#' @param add logical to add points
#' @param xlims bounds for the longitude, in the form c(xlow,xhigh)
#' @param ylims bounds for the latitude
#' @param alpha graphical parameter
#' @param bymonth logical if the points should be colored by month
#' @param pch point type
#' @param bg device background color
#' @param legend logical if legend should be included
plot.btrack<-function (btrack,cex = 1.5, ci = F,  xlims=xlims,ylims=ylims,
                       alpha = 0.15, bymonth = T, pch = 21, bg = 4, legend = F) 
{
  if (legend == T) {
    oldpar = unlist(par()["usr"])
    small = c(oldpar[2] - (oldpar[2] - oldpar[1])/10, oldpar[2] - 
                (oldpar[2] - oldpar[1])/15, oldpar[3] + (oldpar[4] - 
                                                           oldpar[3])/5, oldpar[4] - (oldpar[4] - oldpar[3])/8)
    data(month.colors)
    par(mar = c(5.1, 4.1, 4.1, 4.1))
  }
  len = length(btrack[, 1])
  btrack = btrack[!is.na(btrack[, c("Lon_E")]), ]
  btrack = btrack[!is.na(btrack[, c("Lat_N")]), ]
  #move the x/y lims to outside the function for use in for loops, can activate either of the below to have it built into the code
  #xlims = c(min(btrack[, 8], na.rm = T) - 5, max(btrack[, 8], na.rm = T) + 5) #use this for flexible code
  #ylims = c(min(btrack[, 9], na.rm = T) - 3, max(btrack[, 9],na.rm = T) + 3)
  #xlims<-c(-140,-120) #use this to hard code
  #ylims<-c(30,64.999)
  map("worldHires",xlim=xlims, ylim=ylims,border=1,fill=TRUE,col='gray')
  map.axes(cex.axis=2)
  S = SpatialPointsDataFrame(btrack[, c("Lon_E","Lat_N")], btrack)
  if (ci) {
    sapply(1:len, function(i) .makeCI(as.numeric(btrack[i,4:9]), col = rgb(0.1, 0.7, 0.5, alpha = alpha), border = 0))
  }
  points(S, pch = pch, bg = bg)
  if (bymonth==T) {
    .plot.by.month(S@data, cex = 2, pch = pch)
  }
  lines(btrack[1, c("Lon_E")], btrack[1, c("Lat_N")], typ = "p", pch = 21, col = 1, bg = 3, cex = 1.8)
  lines(btrack[len, c("Lon_E")], btrack[len, c("Lat_N")], typ = "p", pch = 24,col = 1, bg = 2, cex = 1.8)
  box(lwd = 2)
  if (legend == T) {
    .add.month.scale(small)
  }
}

#' Background function for plot.btrack, if bymonth=T
#' 
#' @param ttrack created by SpatialPointsDataFrame in plot.btrack
#' @param cex adjusts size of points
#' @param pch point type
.plot.by.month<-function (ttrack, cex = 2, pch = 21) 
{
  mons = unique(ttrack$Month)
  for (i in 1:length(mons)) {
    lines(ttrack[ttrack$Month == mons[i], c("Lon_E","Lat_N")], typ = "o", cex = cex, 
          pch = pch, bg = month.colors[month.colors[, 1] == mons[i], 2],lty="dashed")
  }
}

#' Plots the raw geolocation locations
#' 
#' @param rawlocs_dat data.frame of geolocation points, columns Year, Month, Day, Lat, Lon
#' @param cex adjusts size of points
#' @param bymonth logical if the points should be colored by month
#' @param pch point type
#' @param bg device background color
plot.rawlocs<-function (rawlocs_dat,cex = 1.5, bymonth = T, pch = 21, bg = 4) 
{
  rawlocs_dat = rawlocs_dat[!is.na(rawlocs_dat[, 5]), ]
  rawlocs_dat = rawlocs_dat[!is.na(rawlocs_dat[, 4]), ]
  len = length(rawlocs_dat[, 1])
  ylims = c(min(rawlocs_dat[, 4]) - 5, max(rawlocs_dat[, 4]) + 5) #use this for flexible code
  xlims = c(min(rawlocs_dat[, 5]) - 3, max(rawlocs_dat[, 5]) + 3)
  #xlims<-c(-172,-120) #use this to hard code
  #ylims<-c(40,62)
  map("worldHires",xlim=xlims, ylim=ylims,border=1,fill=TRUE,col='gray')
  map.axes()
  S = SpatialPointsDataFrame(cbind(rawlocs_dat[,5],rawlocs_dat[,4]), rawlocs_dat)
  points(S, pch = pch, bg = bg)
  if (bymonth) 
    .plot.by.month(S@data, cex = cex, pch = pch)
  lines(rawlocs_dat[1, 4], rawlocs_dat[1, 5], typ = "p", pch = 21, col = 1, 
        bg = 3, cex = 1.8)
  lines(rawlocs_dat[len, 4], rawlocs_dat[len, 5], typ = "p", pch = 24, 
        col = 1, bg = 2, cex = 1.8)
  box(lwd = 2)
  if (legend == T) {
    .add.month.scale(small)
  }
}

#' Object with monthly colors, can be adapted as desired
#' 
month.colors=cbind(c(8:12,1:7),
                   c(rgb(115/255,0,76/255),
                     rgb(0,76/255,115/255),
                     rgb(0,92/255,230/255),
                     rgb(115/255,223/255,1),
                     rgb(190/255,232/255,1),
                     rgb(1,1,190/255),
                     rgb(230/255,230/255,0),
                     rgb(1,170/255,0),
                     rgb(1,120/255,0),
                     rgb(1,0,197/255),
                     rgb(1,0,0),
                     rgb(168/255,0,0)
                   )
)

#' Background function for plot.btrack, if bymonth=T, creates legend
#' 
.add.month.scale<-function (...) {
  ticks <- c(1:12) + 0.5
  par(cex = 2)
  image.plot(-matrix(1:13), horizontal = F, col = rev(month.colors[,2]),
             axis.args = list(at = -ticks, labels = month.abb[as.numeric(month.colors[,1])]), 
             legend.args = list(text = "", cex = 0.75, side = 3, line = 1), legend.mar = 3.6, legend.only = T, ...)
  par(cex = 1)
}

#' Background function for plot.btrack, creates confidence ellipses
#' 
#' @param x results of btrack
#' @param level confidence level
#' @param col RGB color combination for ellipses, set at alpha transparency level
#' @param border size of border around ellipse
#' @param density only if saveobj=T
#' @param lwd width of border around ellipse
#' @param saveobj logical if the ellipses should be saved
.makeCI<-function (x, level = 0.95, npoints = 100, col = rgb(0.7, 0.7, 0.7, alpha = 0.9), border = 1, density = 20, lwd = 0.1 * 
                     par("lwd"), saveobj = F, ...) {
  t.quan <- sqrt(qchisq(level, 2))
  centre <- x[5:6]
  x <- matrix(x[1:4], 2, 2)
  r <- x[1, 2]
  scale <- sqrt(diag(x))
  if (scale[1] > 0) {
    r <- r/scale[1]
  }
  if (scale[2] > 0) {
    r <- r/scale[2]
  }
  r <- min(max(r, -1), 1)
  d <- acos(r)
  a <- seq(0, 2 * pi, len = npoints)
  polymat = (matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], 
                      t.quan * scale[2] * cos(a - d/2) + centre[2]), npoints, 
                    2))
  if (saveobj == T) {
    return(polymat)
  }
  else {
    polygon(polymat, col = col, border = border, density = NA, 
            lwd = lwd, ...)
  }
}


#outputs an animated gif of model iterations
#' Fucntion to animate the results of the itersf function
#' 
#' @param anidat iterout$modeliterations (pulls the list item modeliterations from the itersf output, here called "iterout")
#' @param iter Number of iterations
animap<-function(anidat,iter){
  xlims = c(min(anidat[, 8], na.rm = T) - 5, max(anidat[, 8], na.rm = T) + 5) #use this for flexible code
  ylims = c(min(anidat[, 9], na.rm = T) - 3, max(anidat[, 9],na.rm = T) + 3) #it's outside the loop so the map bounds don't change between iterations
  saveGIF({
    for (j in 1:length(seq(1:iter))){
      matdat<-anidat[anidat$i==j,]
      len = length(matdat[, 1])
      par(mfrow=c(1,1),oma=c(0,0,0,0))
      map("worldHires",xlim=xlims, ylim=ylims,border=1,fill=TRUE,col='gray') #NOTE: the xlim and ylim are hard coded here, need to fix to be adaptable
      map.axes()
      lines(matdat[1:length(matdat[,1]),]$Lon_E,matdat[1:length(matdat[,1]),]$Lat_N,col="gray40",lwd=2,lty="dashed")
      S = SpatialPointsDataFrame(matdat[, 8:9], matdat)
      .plot.by.month(S@data)
      lines(matdat[1, 8], matdat[1, 9], typ = "p", pch = 21, col = 1, 
            bg = 3, cex = 1.8)
      lines(matdat[len, 8], matdat[len, 9], typ = "p", pch = 24, 
            col = 1, bg = 2, cex = 1.8)
      points((((xlims[2]-xlims[1])/iter)*j)+xlims[1],ylims[1],pch=21,bg="red")  #this is hard coded, needs to be adaptable
    }
  }, movie.name = paste("modeliters",tagID,".gif",sep=""), interval = 0.1, nmax = 50, ani.width = 625, ani.height = 625)
  
}

#makes CI polygons based on bootstrap output, currenly uses the model output variance to analytically calculate CI at each iter
#in future, maybe add option to use the percentile of the bootstrap iters around the point estimate
#' Creates confidence interval polygons based on results of bootstrap iterations
#' 
#' @param track data.frame of $iter_summary from the result of itersf
#' @param fname output file name, stored in workspace, not in folder
#' @param level Confidence level
#' @param npoints Number of points to include in polygon
#' @param proj4string Projection for polygon
CI2shp_boot<-function (track, fname = "testshp", level = 0.95, npoints = 100, 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) 
{
  my.ellipse <- function(track, irow = 1, level = 0.95, npoints = 100) {
    print(paste("making polygon #", irow))
    errval<-track[,c("V11","V12","V21","V22")]
    point = as.numeric(errval[irow, ])
    x = matrix(point, 2, 2)
    centre = c(track[irow,8],track[irow,4])
    e1 = ellipse(x, scale = c(1, 1), centre = centre, level = level, 
                 t = sqrt(qchisq(level, 2)), which = c(1, 2), npoints = npoints)
    e1 = rbind(e1, e1[1, ])
    e1
  }
  ndays = dim(track)[1]
  all.ps = lapply(1:ndays, function(i) Polygons(list(Polygon(my.ellipse(track,i, level = level, npoints = npoints))), i))
  row.names(track) = names(all.ps) = 1:ndays
  all.sp = SpatialPolygons(all.ps, proj4string = proj4string)
  all.spdf = SpatialPolygonsDataFrame(all.sp, data = track, 
                                      match.ID = T)
  writePolyShape(all.spdf, fname, factor2char = TRUE, max_nchar = 254)
}

#Function to pull the temperature and depth data only
#
#' Function to pull the temperature and depth data only, creates a list containing two data.frames: all data, daily summaries
#' 
#' @param xlsfile Path to the excell workboot for tagID
getMWTdepths<-function (xlsfile,tagID) { 
  tabs = excel_sheets(xlsfile)
  atabs<-tabs[grep("Archival",tabs)]
  adat = NULL
  print("Retrieving Archival Light, Temp and Depth ")
  for (i in 1:length(atabs)) {
    print(paste("retrieving archival record ", i, sep = ""))
    temp = read_excel(xlsfile, sheet = atabs[i], skip = 1)
    temp = temp[2:nrow(temp), ]
    names(temp) = c("Date", "Tval", "Pval", "light", "extT","depth")
    adat = rbind(adat, temp)
  }
  names(adat) = c("Date", "Tval", "Pval", "light", "extT","depth")
  print("Converting date")
  adat$local<-as.POSIXct(adat$Date,tz="UTC")
  attributes(adat$local)$tzone<-"America/Los_Angeles"
  adat$lday = as.POSIXct(trunc(as.POSIXct(adat$local,format="%m/%d/%Y %H:%M",tz="America/Los_Angeles"), "days")) #maybe add a column for local date/time, will probably require changing fields later in code
  adat$tagID = tagID
  print("Calculating daily summary max temp/depth")
  summarymat<-data.frame(unique(adat$lday))
  summarymat$maxD = tapply(adat$depth, adat$lday, min)
  summarymat$minD = tapply(adat$depth, adat$lday, max)
  summarymat$maxT= tapply(adat$extT, adat$lday, max)
  summarymat$minT = tapply(adat$extT, adat$lday, min)
  summarymat$tagID = tagID
  list("TDdat"= adat, "Daily_summary" = summarymat)
}
