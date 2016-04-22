library(shiny)
library(data.table)
library(ggplot2)
library(rgdal)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output functions defined 
  # below then all use the value computed from this expression
  
  data <- reactive({
    dist <- switch(input$kat,
                   jedna = 1,
                   dva = 2,
                   tri = 3,
                   ctyr = 4,
                   rnorm)
    
    dist(input$n)
  })
  
  data <- reactive({
    dist <- switch(input$rok,
                   leto = 11,
                   zima = 22,
                   rnorm)
    dist(input$n)
    
    })
  
#   data <- reactive({
#     dist <- switch(input$kat,
#                    norm = rnorm,
#                    unif = runif,
#                    lnorm = rlnorm,
#                    exp = rexp,
#                    rnorm)
#     
#     dist(input$n)
#   })
#   
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    kat <- input$kat
    X330 = q330 <- input$n
    obd = input$rok
    exponent= input$expo
    exponent1= input$expo2
    #exponent=0.85
    
    X355<- input$nn
    X364<- input$nnn
    
    #X355=0.02
    #X330=0.5
    #X364=4
    
    old=data.table(X355)
    
    old[X355<0.05, MZPold:=X330]
    old[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
    old[X355>0.5 & X355<5, MZPold:=X355]
    old[X355>5, MZPold:=(X355+X364)*0.5]
    
    
    if (q330<1) exponent1=1.09 else exponent1=1
    
    
    if (kat==1 & obd==11) mzp=((q330^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp=((q330^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp=((q330^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp=((q330^exponent)^exponent1)*.8
    if (kat==3 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp=((q330^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp=((q330^exponent)^exponent1)*1
    
    exponent=0.85
    if (kat==1 & obd==11) mzp3=((q330^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp3=((q330^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp3=((q330^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp3=((q330^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp3=((q330^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp3=((q330^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp3=((q330^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp3=((q330^exponent)^exponent1)*1
    
    rada=seq((q330-0.9*q330),(q330+0.9*q330),by=q330/50)
    #length(rada)
    
    mzp1=rep(0,91)
    mzp2=rep(0,91)
    
    for (i in 1:91)
    {
    
      exponent= input$expo
      #exponent=0.85
      
      #if (rada[i]<1) exponent1=1.09 else exponent1=1
      exponent1= input$expo2
      
    if (kat==1 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp1[i]=(((rada[i])^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp1[i]=(((rada[i])^exponent)^exponent1)*1
    
    if (rada[i]<1) exponent1=1.09 else exponent1=1
    exponent=0.85
    
    if (kat==1 & obd==11) mzp2[i]=(((rada[i])^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp2[i]=(((rada[i])^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp2[i]=(((rada[i])^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp2[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp2[i]=(((rada[i])^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp2[i]=(((rada[i])^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp2[i]=(((rada[i])^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp2[i]=(((rada[i])^exponent)^exponent1)*1
        
    }
    
    bod=old$MZPold
    
    plot(rada, mzp1, type='l', lty=2, lwd=2, xlab='Q330', ylab='MZP',ylim=c(0,max(mzp1)))
    abline(coef=c(mzp3,0), col='red',lwd=0.7, lty=3)
    abline(coef=c(bod,0), col='dark blue',lwd=0.7, lty=3)
    points(q330,mzp3, col='red',lwd=2)
    points(q330,bod,col='dark blue', lwd=2)
    lines(rada, mzp2, type='l', lty=3, lwd=1)
    
    grid()
    #hist(data(), 
      #   main=paste('r', dist, '(', n, ')', sep=''))
  })
  
output$plot2 <- renderPlot({
  #kat <- input$kat
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
  #setwd('/media//Windows7_OS_/vizina/')
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
#   newdta[K99>0.15, KAT:=2]
#   newdta[K99>0.18, KAT:=1]
#   newdta[K99>0.1 & K99<0.15, KAT:=3]
#   newdta[K99<0.1, KAT:=4]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  ggplot(newdta,aes(factor=KAT))+
    geom_boxplot(aes(x=KAT,y=rozdil_proc_hlav, group=KAT))+
    xlab('Kategorie')+
    ylab('Změna [%]')+
    theme(legend.position="none")
  
  
  
  #plot(newdta$X330,  type='l', lty=2, lwd=2, xlab='Q330', ylab='MZP')
  #abline(coef=c(mzp3,0), col='red',lwd=0.7, lty=3)
  #points(q330,mzp3, col='red',lwd=2)
  #lines(rada, mzp2, type='l', lty=3, lwd=1)
  
  #grid()
  #hist(data(), 
  #   main=paste('r', dist, '(', n, ')', sep=''))
})

output$plot3 <- renderPlot({
  #kat <- input$kat
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
  #newdta=as.data.table(read.table('/media//Windows7_OS_/vizina/MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
#   newdta[K99>0.15, KAT:=2]
#   newdta[K99>0.18, KAT:=1]
#   newdta[K99>0.1 & K99<0.15, KAT:=3]
#   newdta[K99<0.1, KAT:=4]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  kor1=0.85
  kor=1.09
  
  newdta[KAT==1, MZPleto1:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima1:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto1:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto1:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto1:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer1:=(MZPleto1/X50)*100]
  
  
  
  ggplot(newdta,aes(factor=KAT))+
    #  geom_boxplot(aes(x=KAT,y=X330, group=KAT))
    geom_point(aes(x=X330,y=pomer1, group=KAT,alpha=0.5), colour='dark blue')+
    geom_point(aes(x=X330,y=pomer, group=KAT), colour = 'red' ,alpha=0.5)+
    #geom_abline(1)+
    ylim(0,100)+
    facet_grid(.~KAT, scales='free_x')+
    xlab('Qa')+
    ylab('Poměr [%]')+
    theme(legend.position="none")
    
})

output$plot4 <- renderPlot({
  #kat <- input$kat
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
 # kor1=0.85
#  kor=1.09
  
  #newdta=as.data.table(read.table('/media//Windows7_OS_/vizina/MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
#   newdta[K99>0.15, KAT:=2]
#   newdta[K99>0.18, KAT:=1]
#   newdta[K99>0.1 & K99<0.15, KAT:=3]
#   newdta[K99<0.1, KAT:=4]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  kor1=0.85
  kor=1.09
  
  newdta[KAT==1, MZPleto1:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima1:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto1:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto1:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto1:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer1:=(MZPleto1/X50)*100]
  
  library('rgdal')
  library('maptools')
  
  #povo_bod = readOGR('/media//Windows7_OS_/vizina/gis/hydro data/dib_E04_Vodomerne_stanice/','E04_Vodomerne_stanice')
  povo_bod = readOGR('gis/','E04_Vodomerne_stanice')
  
  p=data.table(povo_bod@data)
  pokus=povo_bod@coords
  p$X_COORD=pokus[,1]
  p$Y_COORD=pokus[,2]
  
  p = data.table(fortify(p,region='DBCN'))
  p$DBCN=as.character(p$DBCN)
  
  dta = newdta
  dta$DBCN=dta$DBC
  
  for (k in 1:length(dta$DBC))
  {
  if (nchar(dta$DBC[k])==4) dta$DBCN[k]=paste0('00',dta$DBC[k]) 
  if (nchar(dta$DBC[k])==5) dta$DBCN[k]=paste0('0',dta$DBC[k]) 
  }
  
  setkey(p, DBCN)
  setkey(dta, DBCN)
  
  B = dta[p, allow.cartesian=TRUE]
  B=na.omit(B)
  B[, MP:=1]
  
  #  pokus=povo_kat[, allow.cartesian=TRUE]
  #setwd('/home//adam/Dropbox/R/alfa zprava/metodika/gis/')
  #voda=readShapePoly('vodni_plochy')
  #hranice=readShapePoly('/home//adam/Dropbox/R/alfa zprava/metodika/gis/hranice')
  #hranice=readShapePoly('gis/','hranice')
  
  hranice = readOGR('gis/','cr_sjtsk')
  povo_kat = readOGR('gis/','reg_adam2')
povo_kat=fortify(povo_kat)
#   povo_kat = krov2wgs(povo_kat)
#   povo_kat=spTransform(povo_kat,)
#   povo_kat=data.table(povo_kat@data)
#   povo_kat=data.table(fortify(povo_kat))
# source('/home//adam//Dropbox/R/!!functions/HMCR/krov2wgs.r')

#hranice=spTrans(hranice, from="s42", to="wgs")



#  plot(povo_kat)
#  plot(povo_bod,add=TRUE)
#  plot(hranice,add=TRUE,col="red")

  mp = B[, MP[1]]  
  d = B
  #rng = B[, range(z_dv_st)]
  #val = seq(0,1000, length=5)+mp
  
# povo_kat1=povo_kat[povo_kat$kategorie=='1',]
# plot(povo_kat1)

  ggplot(d) + 
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT)), colour='black', size=5.5,alpha=0.21)+
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT), colour=(pomer)), size=4,alpha=0.6)+
    
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name='Poměr')+
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') + 
    theme_minimal() +
    coord_fixed()+
    #theme(legend.position="none")+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
  
})

output$plot5 <- renderPlot({
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
#  kor1=0.85
#  kor=1.09
  
  #newdta=as.data.table(read.table('/media//Windows7_OS_/vizina/MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  newdta[,pomer_zima:=(MZPzima/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  kor1=0.85
  kor=1.09
  
  newdta[KAT==1, MZPleto1:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima1:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto1:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto1:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto1:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer1:=(MZPleto1/X50)*100]
  
  library('rgdal')
  library('maptools')
  
  #povo_bod = readOGR('/media//Windows7_OS_/vizina/gis/hydro data/dib_E04_Vodomerne_stanice/','E04_Vodomerne_stanice')
  povo_bod = readOGR('gis/','E04_Vodomerne_stanice')
  povo_kat = readOGR('gis/','reg_adam2')
  povo_kat=fortify(povo_kat)
  
  p=data.table(povo_bod@data)
  pokus=povo_bod@coords
  p$X_COORD=pokus[,1]
  p$Y_COORD=pokus[,2]
  
  p = data.table(fortify(p,region='DBCN'))
  p$DBCN=as.character(p$DBCN)
  
  dta = newdta
  dta$DBCN=dta$DBC
  
  for (k in 1:length(dta$DBC))
  {
    if (nchar(dta$DBC[k])==4) dta$DBCN[k]=paste0('00',dta$DBC[k]) 
    if (nchar(dta$DBC[k])==5) dta$DBCN[k]=paste0('0',dta$DBC[k]) 
  }
  
  setkey(p, DBCN)
  setkey(dta, DBCN)
  
  B = dta[p, allow.cartesian=TRUE]
  B=na.omit(B)
  B[, MP:=1]
  
  hranice = readOGR('gis/','cr_sjtsk')
  
  mp = B[, MP[1]]  
  d = B

  ggplot(d) + 
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT)), colour='black', size=5.5,alpha=0.21)+
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT), colour=(pomer_zima)), size=4,alpha=0.6)+
    
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name='Poměr')+
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') + 
    theme_minimal() +
    coord_fixed()+
    #theme(legend.position="none")+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
  #geom_polygon(aes(x=long, y = lat), data = povo_kat1, fill = 'green',col='red',lwd=0.3,alpha=0.1)
  
})


output$plot6 <- renderPlot({
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
#  kor1=0.85
#  kor=1.09
  
  #newdta=as.data.table(read.table('/media//Windows7_OS_/vizina/MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  newdta[,pomer_zima:=(MZPzima/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  kor1=0.85
  kor=1.09
  
  newdta[KAT==1, MZPleto1:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima1:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto1:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto1:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto1:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer1:=(MZPleto1/X50)*100]
  
  library('rgdal')
  library('maptools')
  
  #povo_bod = readOGR('/media//Windows7_OS_/vizina/gis/hydro data/dib_E04_Vodomerne_stanice/','E04_Vodomerne_stanice')
  povo_bod = readOGR('gis/','E04_Vodomerne_stanice')
  povo_kat = readOGR('gis/','reg_adam2')
  povo_kat=fortify(povo_kat)
  
  p=data.table(povo_bod@data)
  pokus=povo_bod@coords
  p$X_COORD=pokus[,1]
  p$Y_COORD=pokus[,2]
  
  p = data.table(fortify(p,region='DBCN'))
  p$DBCN=as.character(p$DBCN)
  
  dta = newdta
  dta$DBCN=dta$DBC
  
  for (k in 1:length(dta$DBC))
  {
    if (nchar(dta$DBC[k])==4) dta$DBCN[k]=paste0('00',dta$DBC[k]) 
    if (nchar(dta$DBC[k])==5) dta$DBCN[k]=paste0('0',dta$DBC[k]) 
  }
  
  setkey(p, DBCN)
  setkey(dta, DBCN)
  
  B = dta[p, allow.cartesian=TRUE]
  B=na.omit(B)
  B[, MP:=1]
  
  hranice = readOGR('gis/','cr_sjtsk')
  
  mp = B[, MP[1]]  
  d = B
  
  ggplot(d) + 
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT)), colour='black', size=5.5,alpha=0.21)+
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT), colour=(rozdil_proc_hlav)), size=4,alpha=0.6)+
    
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name='Změna')+
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') + 
    theme_minimal() +
    coord_fixed()+
    #theme(legend.position="none")+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
  #  scale_fill_manual(values)
  #geom_polygon(aes(x=long, y = lat), data = povo_kat1, fill = 'green',col='red',lwd=0.3,alpha=0.1)
})

output$plot7 <- renderPlot({
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
#  kor1=0.85
#  kor=1.09
  
  #newdta=as.data.table(read.table('/media//Windows7_OS_/vizina/MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
  
  newdta[KAT==1, MZPleto:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer:=(MZPleto/X50)*100]
  newdta[,pomer_zima:=(MZPzima/X50)*100]
  
  newdta[X355<0.05, MZPold:=X330]
  newdta[X355>0.05 & X355<0.5, MZPold:=(X330+X355)*0.5]
  newdta[X355>0.5 & X355<5, MZPold:=X355]
  newdta[X355>5, MZPold:=(X355+X364)*0.5]
  
  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]
  
  kor1=0.85
  kor=1.09
  
  newdta[KAT==1, MZPleto1:=round(0.65*(X330^kor1)^kor,3)]
  newdta[KAT==1, MZPzima1:=round(0.85*(X330^kor1)^kor,3)]
  
  newdta[KAT==2, MZPleto1:=round(0.8*(X330^kor1)^kor,3)]
  newdta[KAT==2, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==3, MZPleto1:=round(0.85*(X330^kor1)^kor,3)]
  newdta[KAT==3, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  
  newdta[KAT==4, MZPleto1:=round(0.9*(X330^kor1)^kor,3)]
  newdta[KAT==4, MZPzima1:=round(1*(X330^kor1)^kor,3)]
  newdta[,pomer1:=(MZPleto1/X50)*100]
  
  library('rgdal')
  library('maptools')
  
  #povo_bod = readOGR('/media//Windows7_OS_/vizina/gis/hydro data/dib_E04_Vodomerne_stanice/','E04_Vodomerne_stanice')
  povo_bod = readOGR('gis/','E04_Vodomerne_stanice')
  povo_kat = readOGR('gis/','reg_adam2')
  povo_kat=fortify(povo_kat)
  
  p=data.table(povo_bod@data)
  pokus=povo_bod@coords
  p$X_COORD=pokus[,1]
  p$Y_COORD=pokus[,2]
  
  p = data.table(fortify(p,region='DBCN'))
  p$DBCN=as.character(p$DBCN)
  
  dta = newdta
  dta$DBCN=dta$DBC
  
  for (k in 1:length(dta$DBC))
  {
    if (nchar(dta$DBC[k])==4) dta$DBCN[k]=paste0('00',dta$DBC[k]) 
    if (nchar(dta$DBC[k])==5) dta$DBCN[k]=paste0('0',dta$DBC[k]) 
  }
  
  setkey(p, DBCN)
  setkey(dta, DBCN)
  
  B = dta[p, allow.cartesian=TRUE]
  B=na.omit(B)
  B[, MP:=1]
  
  hranice = readOGR('gis/','cr_sjtsk')
  
  mp = B[, MP[1]]  
  d = B
  
  ggplot(d) + 
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT)), colour='black', size=5.5,alpha=0.21)+
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT), colour=(rozdil_proc_jaro)), size=4,alpha=0.6)+
    
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name='Změna')+
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') + 
    theme_minimal() +
    coord_fixed()+
    #theme(legend.position="none")+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
  #geom_polygon(aes(x=long, y = lat), data = povo_kat1, fill = 'green',col='red',lwd=0.3,alpha=0.1)
  
})

# Generate a summary of the data
  output$summary <- renderPrint({
    
    kat <- input$kat
    q330 <- input$n
    obd = input$rok
    #exponent=0.85
    exponent= input$expo
    exponent1= input$expo2
    
    
    #if (q330<1) exponent1=1.09 else exponent1=1
    
    if (kat==1 & obd==11) mzp=((q330^exponent)^exponent1)*0.65
    if (kat==1 & obd==22) mzp=((q330^exponent)^exponent1)*0.85
    if (kat==2 & obd==11) mzp=((q330^exponent)^exponent1)*0.8
    if (kat==2 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==3 & obd==11) mzp=((q330^exponent)^exponent1)*.85
    if (kat==3 & obd==22) mzp=((q330^exponent)^exponent1)*1
    if (kat==4 & obd==11) mzp=((q330^exponent)^exponent1)*.9
    if (kat==4 & obd==22) mzp=((q330^exponent)^exponent1)*1
    
    print(mzp)
  })
  
  # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(x=data())
#   })
  
})
