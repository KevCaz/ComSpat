.reflector<-function(data = data, control = NULL){
  if(is.null(control)==TRUE){
    fliprow<-sample(c("TRUE", "FALSE"),1)
    flipcol<-sample(c("TRUE", "FALSE"),1)
    if(fliprow==TRUE){
      data<-matrix(data[nrow(data):1,], ncol = ncol(data))
    }
    if(flipcol==TRUE){
      data<-matrix(data[,ncol(data):1], ncol = ncol(data))
    }
  }
  if(is.null(control)==FALSE){
    if(control == "None"){
      data = data
    }
    if(control == "colz"){
      data<-matrix(data[,ncol(data):1], ncol = ncol(data))
    }
    if(control == "rowz"){
      data<-matrix(data[nrow(data):1,], ncol = ncol(data))
    }
    if(control == "colz&rowz"){
      data<-matrix(data[,ncol(data):1], ncol = ncol(data))
      data<-matrix(data[nrow(data):1,], ncol = ncol(data))
    }
  }
  return(data)
}

.rotator<-function(data = data, control = NULL){
  nr_rotations<-sample(c(1,2,3,4),1)
  if(is.null(control)==TRUE){
    if(nr_rotations==1){data<-data} #No reflection
    if(nr_rotations==2){data<-t(data)[,ncol(data):1];colnames(data)<-c(1:ncol(data))} #90 degrees
    if(nr_rotations==3){data<-data[nrow(data):1,ncol(data):1];row.names(data)<-c(1:nrow(data));colnames(data)<-c(1:ncol(data))} #180 degrees
    if(nr_rotations==4){data<-t(data)[ncol(data):1,];row.names(data)<-c(1:nrow(data))} #270 degrees
  }

  if(is.null(control)==FALSE){
    if(control==1){data<-data} #No reflection
    if(control==2){data<-t(data)[,ncol(data):1];colnames(data)<-c(1:ncol(data))} #90 degrees
    if(control==3){data<-data[nrow(data):1,ncol(data):1];row.names(data)<-c(1:nrow(data));colnames(data)<-c(1:ncol(data))} #180 degrees
    if(control==4){data<-t(data)[ncol(data):1,];row.names(data)<-c(1:nrow(data))} #270 degrees
  }
  return(data)
}

.completerandomization<-function(temp.index=temp.index){
  out<-temp.index
  iterate<-0
  while(iterate<=1000){
    iterate<-iterate+1
    out<-out[sample(1:nrow(out)),sample(1:ncol(out))]
  }
  return(out)
}

.shift<-function(temp.index = NULL, rowz = NULL, colz = NULL){
  t.new<-temp.index
  if(rowz>=0){
    countdown<-rowz
    while(!countdown==0){
      t.new<-rbind(t.new[nrow(t.new),],t.new[1:(nrow(t.new)-1),])
      countdown<-countdown-1
    }
  }
  if(rowz<0){
    countdown<-rowz
    while(!countdown==0){
      t.new<-rbind(t.new[2:nrow(t.new),], t.new[1,])
      countdown<-countdown+1
    }
  }
  if(colz>=0){
    countdown<-colz
    while(!countdown==0){
      t.new<-cbind(t.new[,ncol(t.new)], t.new[,1:(ncol(t.new)-1)])
      countdown<-countdown-1
    }
  }
  if(colz<0){
    countdown<-colz
    while(!countdown==0){
      t.new<-cbind(t.new[,2:ncol(t.new)], t.new[,1])
      countdown<-countdown+1
    }
  }
  return(t.new)
}

.add_index<-function(data, dim_max, temp.index){
  for(across in 1:dim_max){
    for(down in 1:dim_max){
      if(!dim(data[data[,"X"]==across & data[,"Y"]==down,])[1]==0){
        data[data[,"X"]==across & data[,"Y"]==down,"index"]<-temp.index[down,across]
      }
      temp.index[down,across]
    }
  }
  return(data)
}

.randomiser<-function(temp.index = NULL, data = NULL, dim_max = NULL, type = NULL, randomization_type = NULL){

  if(type=="Grid"){

    ############################################################################
    # Random Shift
    ############################################################################

    if(randomization_type == "RS"){

      data.r<-NULL

      for(sp in unique(data[,"Species"])){

        ##########################################################################
        # Reflect the data

        temp.index.r<-.reflector(data = temp.index)

        ##########################################################################
        # Rotate the data

        temp.index.r<-.rotator(temp.index.r)

        ##########################################################################
        # Perform the random shift

        x1<-sample(1:dim_max, 1)
        y1<-sample(1:dim_max, 1)

        x2<-sample(1:dim_max, 1)
        y2<-sample(1:dim_max, 1)

        move_y<-y2-y1
        move_x<-x2-x1

        temp.index.r<-.shift(temp.index = temp.index.r, move_y, move_x)

        ##########################################################################
        # link to the original data

        dimnames(temp.index.r)<-list(c(1:dim_max),c(1:dim_max))
        temp.index.r<-as.data.frame(as.data.frame(as.table(temp.index.r)))
        colnames(temp.index.r)<-c("X", "Y", "index")
        temp.index.r[,"index"]<-as.numeric(as.character(temp.index.r[,"index"]))

        temp.index.r<-merge(data,temp.index.r, by = 'index')
        temp.index.r<-temp.index.r[,c(2,5,6,1)]
        colnames(temp.index.r)<-colnames(data)
        temp.index.r<-temp.index.r[,c(1,2,3)]
        temp.index.r$X<-as.numeric(as.character(temp.index.r$X))
        temp.index.r$Y<-as.numeric(as.character(temp.index.r$Y))

        data.r<-rbind(data.r,temp.index.r[temp.index.r[,"Species"]==sp,])

      }

      temp.index.r<-data.r

    }

    ############################################################################
    ############################################################################
    # Complete Randomization
    ############################################################################
    ############################################################################

    if(randomization_type == "CSR"){

      temp.index.r<-data
      temp.index.r[,"X"]<-sample(temp.index.r[,"X"])
      temp.index.r[,"Y"]<-sample(temp.index.r[,"Y"])

    }

  }

  if(type=="Transect"){

    ############################################################################
    # Random Shift
    ############################################################################

    if(randomization_type == "RS"){

      dat.r<-NULL

      #data<-tran.grass.t
      data[,"X"]<-as.numeric(as.character(data[,"X"]))
      #dim_max<-500
      data<-rbind(data,data.frame("X"=c(1:dim_max)[!c(1:dim_max)%in%unique(data$X)],"Species"=""))
      data[,"X"]<-as.numeric(as.character(data[,"X"]))
      data<-data[order(data[,"X"]),]
      data[,"Xn"]<-as.factor(data[,"X"])
      data[,"Xr"]<-length(unique(na.omit(data[,"Xn"]))) + 1 - as.numeric(data[,"Xn"])
      data[,"X"]<-as.numeric(data[,"X"])
      data[,"Xn"]<-as.numeric(data[,"Xn"])
      data[,"Xr"]<-as.numeric(data[,"Xr"])

      for(sp in unique(data[,"Species"])){

        data.r<-data

        ##########################################################################
        # Reflect the data

        flip<-sample(c("Xn","Xr"),1)
        data.r[,"X"]<-data.r[,flip]
        data.r<-data.r[,c("X","Species")]

        ##########################################################################
        # Perform the random shift

        y<-sample(c(1:dim_max), 1)
        A<-data.r[1:(which(data.r[,"X"]==y)[1]-1),]
        B<-data.r[which(data.r[,"X"]==y)[1]:nrow(data.r),]

        data.r<-rbind(B,A)

        dat.r<-rbind(dat.r,data.r[data.r[,"Species"]==sp,])

      }

      dat.r$value<-NULL
      row.names(dat.r)<-NULL
      data.r<-dat.r[!dat.r$Species=="",]

    }

    ############################################################################
    ############################################################################
    # Complete Randomization
    ############################################################################
    ############################################################################

    if(randomization_type == "CSR"){

      data.r<-data
      #data.r$X<-as.numeric(as.character(data.r$X))
      for(sp in unique(data.r[,"Species"])){
        data.r[data.r$Species==sp,'X']<-sample(c(1:dim_max), size = length(data.r[data.r$Species==sp,'X']))
      }
      data.r<-data.r[order(data.r$X),]
      data.r$value<-NULL
      row.names(data.r)<-NULL

    }

    temp.index.r<-data.r

  }

  return(temp.index.r)

}

.random_data<-function(data=NULL, params=NULL, dim_max = NULL, type = NULL,
                       randomization_type = NULL, iterations = 999){

  if(is.null(data))
    stop("data matrix is null")
  if(is.null(type) | is.na(match(type,c("Grid","Transect"))))
    stop("type must be one of Grid or Transect")
  if(type=="Grid")
    if(sum(!is.na(match(colnames(data),c("Species","X","Y"))))<3)
      stop("data must have Species, X and Y column names")
  if(type=="Transect")
    if(sum(!is.na(match(colnames(data),c("Species","X"))))<2)
      stop("data must have Species, X and Y column names")

  if(type=="Grid")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X","Y"))))]
  if(type=="Transect")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X"))))]
  if(!is.character(data[,"Species"]) | is.factor(data[,"Species"]))
    data[,"Species"]<-as.character(data[,"Species"])
  if(!is.integer(data[,"X"]) | is.numeric(data[,"X"]))
    data[,"X"]<-as.numeric(as.character(data[,"X"]))
  if(type=="Grid")
    if(!is.integer(data[,"Y"]) | is.numeric(data[,"Y"]))
      data[,"Y"]<-as.numeric(as.character(data[,"Y"]))

  # This part checks that the paramater input meets criteria for calculation
  if(is.null(params))
    stop("paramater data is null")
  if("Height.of.plots"%in%colnames(params)&type=="Transect")
    stop("Height of plots not a valid option for paramater data")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))<3&type=="Grid")
    stop("paramater data must have Steps.of.scaling, Length.of.plots, Height.of.plots column names")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))<2&type=="Transect")
    stop("paramater data must have Steps.of.scaling and Length.of.plots")

  if(type=="Transect")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))]
  if(type=="Grid")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))]

  params<-as.matrix(params)

  if(!is.integer(params[,"Steps.of.scaling"])|is.numeric(params[,"Steps.of.scaling"]))
    params[,"Steps.of.scaling"]<-as.numeric(as.character(params[,"Steps.of.scaling"]))
  if(!is.integer(params[,"Length.of.plots"])|is.numeric(params[,"Length.of.plots"]))
    params[,"Length.of.plots"]<-as.numeric(as.character(params[,"Length.of.plots"]))
  if(type=="Grid")
    if(!is.integer(params[,"Height.of.plots"])|is.numeric(params[,"Height.of.plots"]))
      params[,"Height.of.plots"]<-as.numeric(as.character(params[,"Height.of.plots"]))

  # Check the attributes

  if(is.null(dim_max))
    stop("dim_max must be the number of plots in one row of grid or the total number of plots in a transect")

  nsp <- length(unique(data[,"Species"])) # Get the number of species
  steps <- nrow(params)

  if(type == "Grid"){

    temp.index<-t(matrix(data=1:(dim_max*dim_max), nrow=dim_max, ncol=dim_max));colnames(temp.index)<-1:dim_max
    params<-cbind(params,(params[,"Length.of.plots"]-rep(1,nrow(params))))
    colnames(params)[4]<-"SkipLN"

    data<-.add_index(data=data, dim_max=dim_max, temp.index = temp.index)

    if(randomization_type == "RS"){

      index.r<-list()
      index.r[["Original"]]<-data

      if(is.null(randomization_type)==FALSE){
        randomisations <- 0
        while(randomisations<iterations){
          randomisations<-randomisations+1

          temp<-.randomiser(temp.index = temp.index, data = data, dim_max = dim_max, type = type,
                                                        randomization_type = randomization_type)

          temp<-.add_index(data=temp, dim_max=dim_max, temp.index = temp.index)

          index.r[[paste(randomisations)]]<-temp

        }
      }
      out<-index.r
    }

    if(randomization_type == "CSR"){

      data.r<-list()
      data.r[["Original"]]<-data

      if(is.null(randomization_type)==FALSE){
        randomisations <- 0
        while(randomisations<iterations){
          randomisations<-randomisations+1

          temp<-.randomiser(temp.index = NULL, data = data,
                                                       dim_max = dim_max, type = type,
                                                       randomization_type = randomization_type)

          for(across in 1:dim_max){
            for(down in 1:dim_max){

              if(!dim(temp[temp[,"X"]==across & temp[,"Y"]==down,])[1]==0){

                temp[temp[,"X"]==across & temp[,"Y"]==down,"index"]<-temp.index[down,across]

              }
              temp.index[down,across]
            }
          }

          data.r[[paste(randomisations)]]<-temp

        }
      }
      out<-data.r

    }

  }

  if(type == "Transect"){
    #data[,"value"]<-1
    data.r<-list()
    data.r[["Original"]]<-data

    if(is.null(randomization_type)==FALSE){
      randomisations <- 0
      while(randomisations<iterations){
        randomisations<-randomisations+1

        data.r[[paste(randomisations)]]<-.randomiser(temp.index = NULL, data = data,
                                                     dim_max = dim_max, type = type,
                                                     randomization_type = randomization_type)
      }
    }
    out<-data.r
  }
  return(out)
}

.ComSpat_rand<-function(data=NULL, params=NULL, dim_max = NULL, type = NULL,
                        measures=c("CD","NRC","AS"),
                        random.index = NULL, randomization_type = NULL){

  if(is.null(data))
    stop("data matrix is null")
  if(is.null(type) | is.na(match(type,c("Grid","Transect"))))
    stop("type must be one of Grid or Transect")
  if(type=="Grid")
    if(sum(!is.na(match(colnames(data),c("Species","X","Y"))))<3)
      stop("data must have Species, X and Y column names")
  if(type=="Transect")
    if(sum(!is.na(match(colnames(data),c("Species","X"))))<2)
      stop("data must have Species, X and Y column names")

  if(type=="Grid")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X","Y"))))]
  if(type=="Transect")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X"))))]
  if(!is.character(data[,"Species"]) | is.factor(data[,"Species"]))
    data[,"Species"]<-as.character(data[,"Species"])
  if(!is.integer(data[,"X"]) | is.numeric(data[,"X"]) | is.factor(data[,"X"])){
    data[,"X"]<-as.numeric(as.character(data[,"X"]))
    data<-data[order(data$X),]
  }

  if(!is.integer(random.index[,"X"]) | is.numeric(random.index[,"X"]) | is.factor(random.index[,"X"])){
    random.index[,"X"]<-as.numeric(as.character(random.index[,"X"]))
    random.index<-random.index[order(random.index$X),]
  }


  if(type=="Grid")
    if(!is.integer(data[,"Y"]) | is.numeric(data[,"Y"]))
      data[,"Y"]<-as.numeric(as.character(data[,"Y"]))

  # This part checks that the paramater input meets criteria for calculation
  if(is.null(params))
    stop("paramater data is null")
  if("Height.of.plots"%in%colnames(params)&type=="Transect")
    stop("Height of plots not a valid option for paramater data")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))<3&type=="Grid")
    stop("paramater data must have Steps.of.scaling, Length.of.plots, Height.of.plots column names")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))<2&type=="Transect")
    stop("paramater data must have Steps.of.scaling and Length.of.plots")

  if(type=="Transect")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))]
  if(type=="Grid")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))]

  params<-as.matrix(params)

  if(!is.integer(params[,"Steps.of.scaling"])|is.numeric(params[,"Steps.of.scaling"]))
    params[,"Steps.of.scaling"]<-as.numeric(as.character(params[,"Steps.of.scaling"]))
  if(!is.integer(params[,"Length.of.plots"])|is.numeric(params[,"Length.of.plots"]))
    params[,"Length.of.plots"]<-as.numeric(as.character(params[,"Length.of.plots"]))
  if(type=="Grid")
    if(!is.integer(params[,"Height.of.plots"])|is.numeric(params[,"Height.of.plots"]))
      params[,"Height.of.plots"]<-as.numeric(as.character(params[,"Height.of.plots"]))

  # Check the attributes

  if(is.null(dim_max))
    stop("dim_max must be the number of plots in one row of grid or the total number of plots in a transect")

  nsp <- length(unique(data[,"Species"])) # Get the number of species
  steps <- nrow(params)

  CD.rand <- matrix(data = 0, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  CD.rand <- CD.rand[order(row.names(CD.rand)),]
  CD.rand <- as.matrix(CD.rand)
  NRC.rand <- matrix(data = 1, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  LD.rand <- as.matrix(CD.rand)
  AS.rand <- as.matrix(CD.rand)
  AS.rel <- as.matrix(CD.rand)

  ##############################################################################
  ##############################################################################
  # Grid Analysis

  if(type == "Grid"){

    temp.index<-t(matrix(data=1:(dim_max*dim_max), nrow=dim_max, ncol=dim_max));colnames(temp.index)<-1:dim_max
    data$index<-NULL
    params<-cbind(params,(params[,"Length.of.plots"]-rep(1,nrow(params))))
    colnames(params)[4]<-"SkipLN"

    for(across in 1:dim_max){
      for(down in 1:dim_max){

        if(!dim(data[data[,"X"]==across & data[,"Y"]==down,])[1]==0){

          data[data[,"X"]==across & data[,"Y"]==down,"index"]<-temp.index[down,across]

        }
        temp.index[down,across]
      }
    }

    index.r<-temp.index
    data0<-data

    if("CSR" == randomization_type){data0<-random.index}

    if("RS" == randomization_type){data0<-random.index}

    for(step in 1:steps){

      temp.2<-matrix(data = 0, nrow = nsp, ncol = dim_max*dim_max);row.names(temp.2)<-unique(data0[,"Species"])
      #print(paste0("step ",step))
      posit<-0

      for(across in 1:dim_max){
        for(down in 1:dim_max){

          posit<-posit+1

          if(down<=(dim_max-(params[step,"SkipLN"]))){
            down_cond<-c(down:(down+params[step,"Height.of.plots"]-1))
          }else{
            down_cond<-c(down:dim_max,(1:(params[step,"Height.of.plots"]-length(down:dim_max))))
          }

          if(across<=(dim_max-params[step,"SkipLN"])){
            across_cond<-c(across:(across+params[step,"Length.of.plots"]-1))
          }else{
            across_cond<-c(across:dim_max,(1:(params[step,"Length.of.plots"]-length(across:dim_max))))
          }

          indice<-matrix(index.r[down_cond,across_cond])
          temp.2[,posit]<-row.names(temp.2)%in%data0[data0[["index"]]%in%as.vector(indice),"Species"]
        }
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,step]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,step] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY
            p1<-freq/sum(freq)
            CD.rand[q,step]=sum(abs((p1)*log2((p1))))}else{
              CD.rand[q,step]=0}

          if("AS"%in%measures){
            AS.rand[q,step]=LD.rand[q,step]-CD.rand[q,step]
          }
        }
      }
    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  }

  ##############################################################################
  ##############################################################################
  # Transect Analysis

  if(type == "Transect"){

    #data[,"value"]<-1
    #if(is.null(random.index)==TRUE){data0<-data}
    #if(is.null(random.index)==FALSE){data0<-random.index}
    data0<-random.index

    for(stepz in 1:steps){

      temp.1<-matrix(data = 0, nrow = nsp, ncol = length(0:(params[stepz,'Length.of.plots']-1)));row.names(temp.1)<-unique(data0[,"Species"])
      temp.2<-matrix(data = 0, nrow = nsp, ncol = max(data0[,"X"]));row.names(temp.2)<-unique(data0[,"Species"])

      for(i in 1:dim_max){

        if(max(c(i:((i+ncol(temp.1))-1)))>dim_max){

          check<-data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

          if(dim(check)[1]==0){
            temp.2<-temp.2
          }else{
            data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

            temp.3<-aggregate(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),],
                              by = list(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"]),
                              FUN = length)

            temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"],i]<-temp.3[,"value"]
          }
        }

        check<-data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),]

        if(dim(check)[1]==0){
          temp.2<-temp.2
        }else{
          temp.3<-aggregate(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),],
                            by = list(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"]),
                            FUN = length)

          temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"],i]<-temp.3[,"value"]
        }
        temp.2<-temp.2[order(row.names(temp.2)),]
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,stepz]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,stepz] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY
            CD.rand[q,stepz]=sum(abs((freq/sum(freq))*log2((freq/sum(freq)))))}else{CD.rand[q,stepz]=0}

          if("AS"%in%measures){
            AS.rand[q,stepz]=LD.rand[q,stepz]-CD.rand[q,stepz]
            AS.rand[is.nan(AS.rand)]<-0
          }
        }
      }
    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  } # end transect

  if("AS"%in%measures){
    measures<-c(measures,"AS.rel")
  }

  results.final <- list("CD" = CD.rand,
                        "NRC" = NRC.rand,
                        "AS" = AS.rand,
                        "AS.rel" = AS.rel)

  names(results.final)=c("CD","NRC","AS","AS.rel")
  results.final<-results.final[names(results.final)%in%measures]
  return(results.final)

}

.ComSpat_rand2<-function(data=NULL, params=NULL, dim_max = NULL, type = NULL,
                        measures=c("CD","NRC","AS"), randomization_type = NULL){

  #print(colnames(data))
  if(!is.data.frame(data))
    stop("data must be a data.frame object")
  if(is.null(data))
    stop("data matrix is null")
  if(is.null(type) | is.na(match(type,c("Grid","Transect"))))
    stop("type must be one of Grid or Transect")
  if(type=="Grid")
    if(sum(!is.na(match(colnames(data),c("Species","X","Y"))))<3)
      stop("data must have Species, X and Y column names")
  if(type=="Transect")
    if(sum(!is.na(match(colnames(data),c("Species","X"))))<2)
      stop("data must have Species, X and Y column names")

  if(type=="Grid")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X","Y"))))]
  if(type=="Transect")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X"))))]
  if(!is.character(data[,"Species"]) | is.factor(data[,"Species"]))
    data[,"Species"]<-as.character(data[,"Species"])
  if(!is.integer(data[,"X"]) | is.numeric(data[,"X"]) | is.factor(data[,"X"])){
    data[,"X"]<-as.numeric(as.character(data[,"X"]))
    data<-data[order(data$X),]
  }

  if(type=="Grid")
    if(!is.integer(data[,"Y"]) | is.numeric(data[,"Y"]))
      data[,"Y"]<-as.numeric(as.character(data[,"Y"]))

  # This part checks that the paramater input meets criteria for calculation
  if(is.null(params))
    stop("paramater data is null")
  if("Height.of.plots"%in%colnames(params)&type=="Transect")
    stop("Height of plots not a valid option for paramater data")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))<3&type=="Grid")
    stop("paramater data must have Steps.of.scaling, Length.of.plots, Height.of.plots column names")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))<2&type=="Transect")
    stop("paramater data must have Steps.of.scaling and Length.of.plots")

  if(type=="Transect")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))]
  if(type=="Grid")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))]

  params<-as.matrix(params)

  if(!is.integer(params[,"Steps.of.scaling"])|is.numeric(params[,"Steps.of.scaling"]))
    params[,"Steps.of.scaling"]<-as.numeric(as.character(params[,"Steps.of.scaling"]))
  if(!is.integer(params[,"Length.of.plots"])|is.numeric(params[,"Length.of.plots"]))
    params[,"Length.of.plots"]<-as.numeric(as.character(params[,"Length.of.plots"]))
  if(type=="Grid")
    if(!is.integer(params[,"Height.of.plots"])|is.numeric(params[,"Height.of.plots"]))
      params[,"Height.of.plots"]<-as.numeric(as.character(params[,"Height.of.plots"]))

  # Check the attributes

  if(is.null(dim_max))
    stop("dim_max must be the number of plots in one row of grid or the total number of plots in a transect")

  nsp <- length(unique(data[,"Species"])) # Get the number of species
  steps <- nrow(params)

  CD.rand <- matrix(data = 0, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  CD.rand <- CD.rand[order(row.names(CD.rand)),]
  CD.rand <- as.matrix(CD.rand)
  NRC.rand <- matrix(data = 1, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  LD.rand <- as.matrix(CD.rand)
  AS.rand <- as.matrix(CD.rand)
  AS.rel <- as.matrix(CD.rand)

  ##############################################################################
  ##############################################################################
  # Grid Analysis

  if(type == "Grid"){

    temp.index<-t(matrix(data=1:(dim_max*dim_max), nrow=dim_max, ncol=dim_max));colnames(temp.index)<-1:dim_max
    data$index<-NULL
    params<-cbind(params,(params[,"Length.of.plots"]-rep(1,nrow(params))))
    colnames(params)[4]<-"SkipLN"

    for(across in 1:dim_max){
      for(down in 1:dim_max){

        if(!dim(data[data[,"X"]==across & data[,"Y"]==down,])[1]==0){

          data[data[,"X"]==across & data[,"Y"]==down,"index"]<-temp.index[down,across]

        }
        temp.index[down,across]
      }
    }

    index.r<-temp.index
    data0<-data

    for(step in 1:steps){

      temp.2<-matrix(data = 0, nrow = nsp, ncol = dim_max*dim_max);row.names(temp.2)<-unique(data0[,"Species"])
      #print(paste0("step ",step))
      posit<-0

      for(across in 1:dim_max){
        for(down in 1:dim_max){

          posit<-posit+1

          if(down<=(dim_max-(params[step,"SkipLN"]))){
            down_cond<-c(down:(down+params[step,"Height.of.plots"]-1))
          }else{
            down_cond<-c(down:dim_max,(1:(params[step,"Height.of.plots"]-length(down:dim_max))))
          }

          if(across<=(dim_max-params[step,"SkipLN"])){
            across_cond<-c(across:(across+params[step,"Length.of.plots"]-1))
          }else{
            across_cond<-c(across:dim_max,(1:(params[step,"Length.of.plots"]-length(across:dim_max))))
          }

          indice<-matrix(index.r[down_cond,across_cond])
          temp.2[,posit]<-row.names(temp.2)%in%data0[data0[["index"]]%in%as.vector(indice),"Species"]
        }
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,step]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,step] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY
            p1<-freq/sum(freq)
            CD.rand[q,step]=sum(abs((p1)*log2((p1))))}else{
              CD.rand[q,step]=0}

          if("AS"%in%measures){
            AS.rand[q,step]=LD.rand[q,step]-CD.rand[q,step]
          }
        }
      }
    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  }

  ##############################################################################
  ##############################################################################
  # Transect Analysis

  if(type == "Transect"){
    data[,"value"]<-1
    data0<-data

    for(stepz in 1:steps){

      temp.1<-matrix(data = 0, nrow = nsp, ncol = length(0:(params[stepz,'Length.of.plots']-1)));row.names(temp.1)<-unique(data0[,"Species"])
      temp.2<-matrix(data = 0, nrow = nsp, ncol = dim_max);row.names(temp.2)<-unique(data0[,"Species"])

      for(i in 1:dim_max){

        if(max(c(i:((i+ncol(temp.1))-1)))>dim_max){

          check<-data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

          if(dim(check)[1]==0){
            temp.2<-temp.2
          }else{
            data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

            temp.3<-aggregate(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),],
                              by = list(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"]),
                              FUN = length)

            temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"],i]<-temp.3[,"X"]
          }
        }

        check<-data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),]

        if(dim(check)[1]==0){
          temp.2<-temp.2
        }else{
          temp.3<-aggregate(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),],
                            by = list(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"]),
                            FUN = length)

          temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"],i]<-temp.3[,"X"] #temp.3[,"value"]
        }
        temp.2<-temp.2[order(row.names(temp.2)),]
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,stepz]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,stepz] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY
            CD.rand[q,stepz]=sum(abs((freq/sum(freq))*log2((freq/sum(freq)))))}else{CD.rand[q,stepz]=0}

          if("AS"%in%measures){
            AS.rand[q,stepz]=LD.rand[q,stepz]-CD.rand[q,stepz]
            AS.rand[is.nan(AS.rand)]<-0
          }
        }
      }
    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  } # end transect

  if("AS"%in%measures){
    measures<-c(measures,"AS.rel")
  }

  results.final <- list("CD" = CD.rand,
                        "NRC" = NRC.rand,
                        "AS" = AS.rand,
                        "AS.rel" = AS.rel)

  names(results.final)=c("CD","NRC","AS","AS.rel")
  results.final<-results.final[names(results.final)%in%measures]
  return(results.final)

}


.ComSpat_orig<-function(data=NULL, params=NULL, dim_max = NULL, type = NULL,
                        measures=NULL){

  random.index = NULL # This part is kept for ComSpat.par

  if(is.null(data))
    stop("data matrix is null")
  if(is.null(type) | is.na(match(type,c("Grid","Transect"))))
    stop("type must be one of Grid or Transect")
  if(type=="Grid")
    if(sum(!is.na(match(colnames(data),c("Species","X","Y"))))<3)
      stop("data must have Species, X and Y column names")
  if(type=="Transect")
    if(sum(!is.na(match(colnames(data),c("Species","X"))))<2)
      stop("data must have Species, X and Y column names")

  if(type=="Grid")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X","Y"))))]
  if(type=="Transect")
    data<-data[,which(!is.na(match(colnames(data),c("Species","X"))))]
  if(!is.character(data[,"Species"]) | is.factor(data[,"Species"]))
    data[,"Species"]<-as.character(data[,"Species"])
  if(!is.integer(data[,"X"]) | is.numeric(data[,"X"]))
    data[,"X"]<-as.numeric(as.character(data[,"X"]))
  if(type=="Grid")
    if(!is.integer(data[,"Y"]) | is.numeric(data[,"Y"]))
      data[,"Y"]<-as.numeric(as.character(data[,"Y"]))

  # This part checks that the paramater input meets criteria for calculation
  if(is.null(params))
    stop("paramater data is null")
  if("Height.of.plots"%in%colnames(params)&type=="Transect")
    stop("Height of plots not a valid option for paramater data")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))<3&type=="Grid")
    stop("paramater data must have Steps.of.scaling, Length.of.plots, Height.of.plots column names")
  if(sum(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))<2&type=="Transect")
    stop("paramater data must have Steps.of.scaling and Length.of.plots")

  if(type=="Transect")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots"))))]
  if(type=="Grid")
    params<-params[,which(!is.na(match(colnames(params),c("Steps.of.scaling", "Length.of.plots", "Height.of.plots"))))]

  params<-as.matrix(params)

  if(!is.integer(params[,"Steps.of.scaling"])|is.numeric(params[,"Steps.of.scaling"]))
    params[,"Steps.of.scaling"]<-as.numeric(as.character(params[,"Steps.of.scaling"]))
  if(!is.integer(params[,"Length.of.plots"])|is.numeric(params[,"Length.of.plots"]))
    params[,"Length.of.plots"]<-as.numeric(as.character(params[,"Length.of.plots"]))
  if(type=="Grid")
    if(!is.integer(params[,"Height.of.plots"])|is.numeric(params[,"Height.of.plots"]))
      params[,"Height.of.plots"]<-as.numeric(as.character(params[,"Height.of.plots"]))

  # Check the attributes

  if(is.null(dim_max))
    stop("dim_max must be the number of plots in one row of grid or the total number of plots in a transect")

  nsp <- length(unique(data[,"Species"])) # Get the number of species
  steps <- nrow(params)

  CD.rand <- matrix(data = 0, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  CD.rand <- CD.rand[order(row.names(CD.rand)),]
  CD.rand <- as.matrix(CD.rand)
  NRC.rand <- matrix(data = 1, ncol = steps, nrow = nsp, dimnames = list(c(as.character(unique(data[,"Species"]))),c(paste0(rep("Step_"),1:steps))))
  LD.rand <- as.matrix(CD.rand)
  AS.rand <- as.matrix(CD.rand)
  AS.rel <- as.matrix(CD.rand)

  ##############################################################################
  ##############################################################################
  # Grid Analysis

  if(type == "Grid"){

    temp.index<-t(matrix(data=1:(dim_max*dim_max), nrow=dim_max, ncol=dim_max));colnames(temp.index)<-1:dim_max
    data$index<-NULL
    params<-cbind(params,(params[,"Length.of.plots"]-rep(1,nrow(params))))
    colnames(params)[4]<-"SkipLN"

    for(across in 1:dim_max){
      for(down in 1:dim_max){

        if(!dim(data[data[,"X"]==across & data[,"Y"]==down,])[1]==0){

          data[data[,"X"]==across & data[,"Y"]==down,"index"]<-temp.index[down,across]

        }
        temp.index[down,across]
      }
    }

    if(is.null(random.index)==TRUE){index.r<-temp.index}
    if(is.null(random.index)==FALSE){index.r<-random.index}

    names(index.r)<-"Trial"

    for(step in 1:steps){

      temp.2<-matrix(data = 0, nrow = nsp, ncol = dim_max*dim_max);row.names(temp.2)<-unique(data[,"Species"])
      #print(paste0("step ",step))
      posit<-0

      for(across in 1:dim_max){
        for(down in 1:dim_max){

          posit<-posit+1

          if(down<=(dim_max-(params[step,"SkipLN"]))){
            down_cond<-c(down:(down+params[step,"Height.of.plots"]-1))
          }else{
            down_cond<-c(down:dim_max,(1:(params[step,"Height.of.plots"]-length(down:dim_max))))
          }

          if(across<=(dim_max-params[step,"SkipLN"])){
            across_cond<-c(across:(across+params[step,"Length.of.plots"]-1))
          }else{
            across_cond<-c(across:dim_max,(1:(params[step,"Length.of.plots"]-length(across:dim_max))))
          }

          indice<-matrix(index.r[down_cond,across_cond])
          temp.2[,posit]<-row.names(temp.2)%in%data[data[["index"]]%in%as.vector(indice),"Species"]
        }
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,step]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,step] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY

            p1<-freq/sum(freq)
            CD.rand[q,step]=sum(abs((p1)*log2((p1))))}else{CD.rand[q,step]=0}

          if("AS"%in%measures){
            AS.rand[q,step]=LD.rand[q,step]-CD.rand[q,step]
          }
        }
      }

    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  }

  ##############################################################################
  ##############################################################################
  # Transect Analysis

  if(type == "Transect"){

    data[,"value"]<-1
    data.r<-list()
    data.r[["Original"]]<-data

    if(is.null(random.index)==TRUE){data0<-data}
    if(is.null(random.index)==FALSE){data0<-random.index}

    for(step in 1:steps){

      temp.1<-matrix(data = 0, nrow = nsp, ncol = length(0:(params[step,'Length.of.plots']-1)));row.names(temp.1)<-unique(data0[,"Species"])
      temp.2<-matrix(data = 0, nrow = nsp, ncol = dim_max);row.names(temp.2)<-unique(data0[,"Species"])

      for(i in 1:dim_max){

        if(max(c(i:((i+ncol(temp.1))-1)))>dim_max){

          check<-data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

          if(dim(check)[1]==0){
            temp.2<-temp.2
          }else{
            data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),]

            temp.3<-aggregate(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),],
                              by = list(data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"]),
                              FUN = length)

            temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:dim_max,1:(max(c(i:((i+ncol(temp.1))-1)))-dim_max)),"Species"],i]<-temp.3[,"value"]
          }
        }

        check<-data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),]

        if(dim(check)[1]==0){
          temp.2<-temp.2
        }else{
          temp.3<-aggregate(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),],
                            by = list(data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"]),
                            FUN = length)

          temp.2[row.names(temp.2)%in%data0[data0[,"X"]%in%c(i:((i+ncol(temp.1))-1)),"Species"],i]<-temp.3[,"value"]
        }
        temp.2<-temp.2[order(row.names(temp.2)),]
      }

      ##############################################################################
      ######################### Functions go here ##################################
      ##############################################################################

      for(q in 1:nrow(temp.2)){

        prova2.temp<-temp.2[1:q,]
        prova2.temp[prova2.temp>0]<-1

        if(is.null(dim(prova2.temp))==TRUE){
          prova2.temp<-as.data.frame(t(prova2.temp))
          unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])}else{
            unique_comb<-as.matrix(prova2.temp[,!duplicated(t(prova2.temp))])
            #unique_comb<-as.matrix(unique_comb[,colSums(unique_comb)>0])
          }

        if("AS"%in%measures){
          # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
          LE.rand <- rep(0,nrow(prova2.temp[,]))
          for(i in 1:nrow(prova2.temp[,])){
            LE.rand[i] <- (ncol(prova2.temp[,])*log2(ncol(prova2.temp[,]))-sum(prova2.temp[i,])*log2(sum(prova2.temp[i,]))-(ncol(prova2.temp[,])-sum(prova2.temp[i,]))*log2((ncol(prova2.temp[,])-sum(prova2.temp[i,]))))/ncol(prova2.temp[,])
          }
          LE.rand[is.na(LE.rand)] <- 0
          LD.rand[q,step]=sum(LE.rand) # Used to calculate ASSOCIATUM
        }

        if("NRC"%in%measures|"CD"%in%measures|"AS"%in%measures){
          # NUMBER OF REALISED COMBINATIONS
          NRC.rand[q,step] <- ncol(unique_comb)
        }

        if(!is.null(unique_comb)){

          freq = rep(0, ncol(unique_comb))

          for(z in 1:ncol(unique_comb)){
            a=unique_comb[,z]
            hasCol=function(prova2.temp,a){colSums(a==prova2.temp)==nrow(prova2.temp)}
            freq[z]<-sum(hasCol(prova2.temp,a))
          }

          if("CD"%in%measures|"AS"%in%measures){
            # COMPOSITIONAL DIVERSITY
            CD.rand[q,step]=sum(abs((freq/sum(freq))*log2((freq/sum(freq)))))}else{CD.rand[q,step]=0}

          if("AS"%in%measures){
            AS.rand[q,step]=LD.rand[q,step]-CD.rand[q,step]
            AS.rand[is.nan(AS.rand)]<-0
          }
        }
      }
    }

    if("AS"%in%measures){
      AS.rel<-as.matrix(AS.rand/CD.rand)
      AS.rel[is.nan(AS.rel)]<-0
    }

  } # end transect

  if("AS"%in%measures){
    measures<-c(measures,"AS.rel")
  }

  results.final <- list("CD" = CD.rand,
                        "NRC" = NRC.rand,
                        "AS" = AS.rand,
                        "AS.rel" = AS.rel)

  names(results.final)=c("CD","NRC","AS","AS.rel")
  results.final<-results.final[names(results.final)%in%measures]
  return(results.final)

}

##' Within-Community Spatial Organisation
##'
##' The \code{ComSpat} function calculates Juhász-Nagy Information Theory models.
##'
##' The \code{ComSpat} function presents four measures from a family of
##' Information Theory models developed by Juhász-Nagy (1967, 1976, 1984a,
##' 1984b). The measures represent co-existence relationships in multispecies
##' communities. For additional information on the measures please see the
##' package vignette.
##'
##' @param data A matrix or data frame capturing the spatial coordinate(s) of
##' species sampled from a grid or transect. Each row captures the
##' coordinate(s) of a \code{Species}. If the \code{data} was sampled as a
##' Transect only the \code{X} coordinate is required.  If the \code{data} was
##' sampled as a Grid both \code{X} and \code{Y} coordinates are required.
##' @param params Data frame providing the secondary sampling information.
##' @param dim_max Numeric. Number of sampling units in one row of a
##' \code{"Grid"} or \code{"Transect"}.
##' @param type Character. Supply either \code{"Grid"} or \code{"Transect"}.
##' @param measures Vector. List the measures returned by \code{SpatCom()}. The
##' default option returns the compositional diversity (\code{"CD"}), number of
##' realised species combinations (\code{"NRC"}) and associatum (\code{"AS"}).
##' Relative associatum (\code{"AS.rel"}) is returned by default when
##' \code{"AS"} is called.
##' @param randomization_type Character. Supply either \code{"CSR"} or
##' \code{"RS"}. Activating randomization initiates parallel computing.
##' @param iterations Numeric. Number of randomizations. The default is 999.
##' @param alpha Numeric. If (\code{NULL}), p value returned. Else 1 or 0.
##'
##' @return The function returns an object of class list returning named data
##' frames specified by the \code{measures} argument. Each data frame contains
##' species as rows and the steps of scaling as columns.
##' @author James L. Tsakalos
##' @seealso \code{\link{ComSpat.plot}}
##' @references Juhász-Nagy, P. (1967). On some 'characteristic area' of plant
##' community stands. Proc. Colloq. Inf. Theor. 269-282.
##'
##' Juhász-Nagy, P. (1976). Spatial dependence of plant populations. Part 1.
##' Equivalence analysis (an outline for a new model). Acta Bot. Acad Sci.
##' Hung. 22: 61-78.
##'
##' Juhász-Nagy, P. (1984a). Notes on diversity. Part, I. Introduction. Abstr.
##' Bot. 8: 43-55.
##'
##' Juhász-Nagy, P. (1984b). Spatial dependence of plant populations. Part 2. A
##' family of new models. Acta Bot. Acad Sci. Hung. 30: 363-402.
##'
##' Tsakalos, J.L. et al. (XXXX). SpatCom: An R package to analyse community
##' organisation across spatial scales. XXXX.
##' @examples
##'
##' data("grid.random") #input data frame
##' data("param.grid") #input paramater data frame
##' temp<-ComSpat(data = grid.random, params = param.grid[1:5,], dim_max = 64, type = "Grid")
##' @export

ComSpat<-function(data = NULL, params = NULL, dim_max = NULL, type = NULL,
                      measures=NULL,
                      randomization_type = NULL, iterations = NULL, alpha = NULL){

  if(is.null(measures)==TRUE){measures=c("CD","NRC","AS")}
  if(is.null(iterations)==TRUE){iterations = 999}
  if(!is.null(randomization_type)){
    if(sum(!is.na(match(randomization_type,c("CSR", "RS"))))<1){
      stop("randomization_type must be CSR or RS")
    }
  }

  if(is.null(randomization_type)==TRUE){

    return(.ComSpat_orig(data=data,params=params,dim_max=dim_max,type=type,
                  measures=measures))

  }

  if(is.null(randomization_type)==FALSE){

    ##############################################################################
    # Prepare data for randomizations

    random.dat<-.random_data(data=data, params=params, dim_max = dim_max, type = type,
                             randomization_type = randomization_type,
                             iterations = iterations)

    ##############################################################################
    # Run randomizations

    future::plan(future::multisession)

    rand<-future.apply::future_lapply(random.dat, FUN = function(x){
      .ComSpat_rand2(data=data.frame(x), params=params, dim_max=dim_max, type=type,
                    randomization_type = randomization_type,
                    measures=measures)})

    future::plan(future::sequential)

    #.ComSpat_rand(data=data, params=params, dim_max=dim_max, type=type,
    #              randomization_type = randomization_type,
    #              measures=measures, random.index = x)})

    ##############################################################################
    # Clean the output

    steps <- nrow(params)

    CD<-matrix(data = 0, ncol = steps, nrow = iterations+1, dimnames = list(names(rand[]),c(paste0(rep("Step_"),1:steps))))
    NRC <- as.matrix(CD)
    AS <- as.matrix(CD)
    AS.rel <- as.matrix(CD)

    out<-list("CD"=CD,"NRC"=NRC,"AS"=AS,"AS.rel"=AS)

    for(i in names(rand[])){
      for(j in names(rand$Original)){
        out[[j]][i,]<-apply(matrix(unlist(rand[[i]][j]), ncol = steps),2,max)
      }
    }

    ##############################################################################
    # Calculate statistics

    vars<-c("Original","Mean", "Max", "Min", "Quartile 1",
            "Quartile 3", "std", "cv", "p o<r", "p o>r",
            "95%UL", "95%LL")

    CD<-matrix(data = 0, ncol = steps, nrow = length(vars), dimnames = list(c(vars),c(paste0(rep("Step_"),1:steps))))
    NRC <- as.matrix(CD)
    AS <- as.matrix(CD)
    AS.rel <- as.matrix(CD)

    statz<-list("CD"=CD,"NRC"=NRC,"AS"=AS,"AS.rel"=AS)

    for(i in names(out[])){

      for(j in vars){

        if(j=="Original"){statz[[i]][j,]<-out[[i]][j,]}

        if(j=="Mean"){statz[[i]][j,]<-apply(matrix(out[[i]][2:(iterations+1),], ncol = steps),2,mean)}

        if(j=="Max"){statz[[i]][j,]<-apply(matrix(out[[i]][2:(iterations+1),], ncol = steps),2,max)}

        if(j=="Min"){statz[[i]][j,]<-apply(matrix(out[[i]][2:(iterations+1),], ncol = steps),2,min)}

        if(j=="Quartile 1"){
          summy<-matrix(summary(matrix(out[[i]][2:(iterations+1),], ncol = steps)), ncol = steps)
          statz[[i]][j,]<-as.numeric(as.character(trimws(sapply(strsplit(summy[2,],":"), `[`, 2))))
        }

        if(j=="Quartile 3"){
          summy<-matrix(summary(matrix(out[[i]][2:(iterations+1),], ncol = steps)), ncol = steps)
          statz[[i]][j,]<-as.numeric(as.character(trimws(sapply(strsplit(summy[5,],":"), `[`, 2))))
        }

        if(j=="std"){statz[[i]][j,]<-apply(matrix(out[[i]][2:(iterations+1),], ncol = steps),2,sd)}

        if(j=="cv"){
          statz[[i]][j,]<-statz[[i]][7,]/statz[[i]][2,]
          statz[[i]][j,][is.nan(statz[[i]][j,])]<-0.0000
        }

        if(j=="p o<r"){
          for(k in 1:steps){
            statz[[i]][j,k]<-sum(matrix(out[[i]][2:(iterations+1),],ncol=steps)[,k]<matrix(out[[i]],ncol=steps)[1,k])/(iterations)

            if(is.null(alpha)==FALSE){
              if(statz[[i]][j,k]<=alpha){
                statz[[i]][j,k]<-1
              }else{
                statz[[i]][j,k]<-0
              }
            }
          }
        }

        if(j=="p o>r"){
          for(k in 1:steps){
            statz[[i]][j,k]<-sum(matrix(out[[i]][2:(iterations+1),],ncol=steps)[,k]>matrix(out[[i]],ncol=steps)[1,k])/(iterations)
            if(is.null(alpha)==FALSE){
              if(statz[[i]][j,k]<=alpha){
                statz[[i]][j,k]<-1
              }else{
                statz[[i]][j,k]<-0
              }
            }
          }
        }

        if(j=="95%UL"){
          statz[[i]][j,]<-statz[[i]][6,] #UL (upper limit)
        }

        if(j=="95%LL"){
          statz[[i]][j,]<-statz[[i]][5,] #LL (lower limit)
        }
      }
    }

    for(i in names(statz[])){
      statz[[i]]<-round(statz[[i]],4)
    }

    if("AS" %in% measures){
      measures<-c(measures, "AS.rel")
    }

    out<-out[measures]
    statz<-statz[measures]

    return(list("Raw data" = out, "Summary statistics" = statz))

    }
}



