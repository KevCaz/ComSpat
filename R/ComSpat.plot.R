##' Within-Community Spatial Organization Plot
##'
##' Function \code{ComSpat.plot} makes use of core R graphics systems to
##' display several Information Theory metrics. The \code{ComSpat.plot} does
##' not calculate the actual Information Theory metrics, but it accepts results
##' from the \code{\link{ComSpat}} function.
##'
##' \code{ComSpat.plot} is used to construct the initial plot object. It has
##' the functionality to return single or multiple outputs. When multiple
##' outputs are returned data must be supplied as a list; a single measure for
##' each of the data frames will be added to the same plot. This function makes
##' use of core R graphics systems.
##'
##' @param data Data frame or list of data frames returned from \code{ComSpat}.
##' @param params Data frame providing the secondary sampling information.
##' @param type Character. Supply either \code{"Grid"} or \code{"Transect"}.
##' @param measure Character. Supply one of \code{"CD", "NRC", "AS" or
##' "AS.Rel"}.
##' @param su.size Numeric. Surface area of the smallest sampling unit (mm sq.).
##' @param ymin Numeric. Y axis lower limit.
##' @param ymax Numeric. Y axis upper limit.
##' @param p.col Character. Colour used for the measure. The value must be
##' either single or a vector corresponding to the length of data if class
##' list.
##' @param intervals TRUE or FALSE.
##' @param xmin Numeric. Minimum x axis value (i.e. lower range).
##' @param xmax Numeric. Maximum x axis value (i.e. upper range).
##' @param p.cex Numeric.
##' @param cex.axis Numeric.
##' @param xaxt NULL or 'n'. To control if x-axis text is displayed.
##' @param yaxt NULL or 'n'. To control if y-axis text is displayed.
##' @author James L. Tsakalos
##' @seealso \code{\link{ComSpat}}, \code{\link{data}}
##' @examples
##'
##' # Load the training data and paramater files
##' data("grid.random")
##' data("param.grid")
##'
##' # Perform ComSpat calculations
##' temp.rand<-ComSpat(data=grid.random, params=param.grid[1:5,], dim_max = 64, type="Grid")
##'
##' # Plot ComSpat results
##' ComSpat.plot(data = temp.rand, params = param.grid[1:5,], type = "Grid", measure = "NRC",
##'  su.size = 0.01, ymin = 0, ymax = 65, p.col = "red")
##'
##' # Hint - several measures can be combined using par() commands
##' @export

ComSpat.plot<-function(data = NULL, params = NULL, type = NULL, measure = NULL,
                       intervals = FALSE, su.size = NULL,
                       ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
                       p.col = NULL, p.cex = NULL, cex.axis = NULL,
                       xaxt = NULL, yaxt = NULL){

  if(is.null(data))
    stop("data must be of length one of class list")
  if(is.null(params))
    stop("paramater data is null")
  if(is.null(type) | is.na(match(type,c("Grid","Transect"))))
    stop("type must be one of Grid or Transect")
  if(is.list(measure) || is.null(measure) || is.na(match(measure,c("CD","NRC","AS","AS.rel")))
     || length(measure)>1)
    stop("measure must be of length one corresponding to CD, NRC, AS or AS.rel")
  if(is.null(su.size))
    stop("su.size must be a numeric value indicating the size of the smallest sampling unit")
  if(is.null(ymin))
    stop("ymin must be a numeric value inidicating the minimum y-axis extent")
  if(is.null(ymax))
    stop("ymax must be a numeric value inidicating the maximum y-axis extent")
  if(is.null(p.col))p.col<-"black"

  if(is.null(xmax)){xmax<-100}
  if(is.null(xmin)){xmin<-0.01}
  if(is.null(p.cex)){p.cex<-0.75}
  if(is.null(cex.axis)){cex.axis<-1}

  depth<-function(data) ifelse(is.list(data), 1L + max(sapply(data, depth)), 0L)

  if(!intervals == TRUE){

    if(is.list(data)==TRUE && depth(data)==2){

      if(is.list(p.col)==FALSE)
        stop("p.col must be a list of length matching data")

      for(i in 1:length(data)){

        temp.1<-apply(data[[i]][[measure]],2,max)

        if(type=="Grid")
          #temp.2<-(params[["Length.of.plots"]]*params[["Height.of.plots"]])*su.size
          temp.2<-params[["Length.of.plots"]]*su.size
        if(type=="Transect")
          #temp.2<-(params[["Length.of.plots"]]*params[["Length.of.plots"]])*su.size
          temp.2<-params[["Length.of.plots"]]*su.size

        if(measure=="AS" || measure=="AS.rel"){
          as.las<-0
        }else{as.las<-2}

        if(i==1){
          plot(temp.1~temp.2, xaxt="none", xlim=c(xmin,xmax), ylim =c(ymin,ymax),
               type='o', col=p.col[[i]][1], cex.axis=0.75, xlab='',ylab='', las=as.las, pch=19,
               cex = p.cex,log = 'x')
          axis(1, at=round(c(0.01,seq(0.02,0.09,0.01),0.1,1,10,100),2), labels=c("","","","","","","","","","","","",""), cex.axis=0.75)
          axis(1, at=round(c(0.01,0.1,1,10,100),2), labels=c(0.01, 0.1,1,10,100), cex.axis=0.75)
        }else{
          par(new=TRUE)
          plot(temp.1~temp.2, xaxt="none", xlim=c(xmin,xmax), ylim =c(ymin,ymax),
               type='o', col=p.col[[i]][1], cex.axis=0.75, xlab='',ylab='', las=as.las, pch=19,
               cex = p.cex, log = 'x')
        }

      }

      if(measure=="CD")
        mtext(text="Compositional Diversity (bits)", side=2, line=2, cex=0.8, font=2)
      if(measure=="NRC")
        mtext(text="Realised Combinations (nr.)", side=2, line=2, cex=0.8, font=2)
      if(measure=="AS")
        mtext(text="Associatum (bits)", side=2, line=2, cex=0.8, font=2)
      if(measure=="AS.rel")
        mtext(text="Rel. Associatum (bits)", side=2, line=2, cex=0.8, font=2)

    }else{

      if(is.list(p.col)==TRUE)
        stop("p.col must be of length one")

      temp.1<-apply(data[[measure]],2,max)
      temp.2<-(params[[2]]*params[[3]])*su.size

      if(measure=="AS" || measure=="AS.rel"){
        as.las<-0
      }else{as.las<-2}

      plot(temp.1~temp.2, xaxt="none", xlim=c(xmin,xmax), ylim =c(ymin,ymax),
           type='o', col=p.col, cex.axis=0.75, xlab='',ylab='', las=as.las, pch=19,
           cex = p.cex, log = 'x')

      #axis(side = 1, at = x, labels = FALSE, tck = -0.01)
      #axis(side = 1, at = x, labels = FALSE, tck = -0.01)

      axis(1, at=round(c(0.01,0.1,1,10,100),2), labels=c(0.01,0.1,1,10,100), cex.axis=0.75)

      if(measure=="CD")
        mtext(text="Compositional Diversity (bits)", side=2, line=2, cex=0.8, font=2)
      if(measure=="NRC")
        mtext(text="Realised Combinations (nr.)", side=2, line=2, cex=0.8, font=2)
      if(measure=="AS")
        mtext(text="Associatum (bits)", side=2, line=2, cex=0.8, font=2)
      if(measure=="AS.rel")
        mtext(text="Rel. Associatum (bits)", side=2, line=2, cex=0.8, font=2)
    }

    mtext(text="Length of sampling units (m)", side=1, line=2, cex=0.8, font=2)

  }

  if(intervals == TRUE){

    for(i in 1:length(data)){
      for(j in measure){

        temp.1<-data[[i]][[j]][1,] # Original

        if(type=="Grid")
          temp.2<-(params[["Length.of.plots"]]*params[["Height.of.plots"]])*su.size
        if(type=="Transect")
          temp.2<-(params[["Length.of.plots"]]*params[["Length.of.plots"]])*su.size

        if(measure=="AS" || measure=="AS.rel"){
          as.las<-0
        }else{as.las<-2}


        if(i==1){
          plot(temp.1~temp.2, xaxt="none", xlim=c(xmin,xmax), ylim =c(ymin,ymax),
               type='o', col='black', cex.axis=cex.axis, xlab='',ylab='', las=as.las, pch=19,
               cex = p.cex, log = 'x', xaxt = xaxt, yaxt = yaxt)
          if(is.null(xaxt)){axis(1, at=round(c(0.01,0.1,1,10,100),2), labels=c(0.01,0.1,1,10,100), cex.axis=0.75)}

        }

        i
        j

        data[["Random Shift"]][[j]]
        data$`Random Shift`

        temp.up<-data[[i]][[j]][11,]
        temp.do<-data[[i]][[j]][12,]

        if(names(data)[i] == "Random Shift"){
          lines(temp.2,temp.up, col = 'red')
          lines(temp.2,temp.do, col = 'red')
        }

        if(names(data)[i] == "CSR"){
          lines(temp.2,temp.up, col = 'blue')
          lines(temp.2,temp.do, col = 'blue')
        }




      }



    }



  }



}

# NOT RUN {Rdpack::reprompt("ComSpat.plot")}
