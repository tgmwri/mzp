library(shiny)
library(data.table)
library(ggplot2)
library(rgdal)

calc_mzp <- function(q330, kat, obd, exponent, exponent2) {
    coeffs = list(leto = c(0.65, 0.8, 0.85, 0.9), zima = c(0.85, 1, 1, 1))
    season = "leto"
    if (obd == 22)
        season = "zima"
    return(((q330^exponent)^exponent2)*coeffs[[season]][as.numeric(kat)])
}

shinyServer(function(input, output) {
new_mzp <- reactive({ calc_mzp(input$n, input$kat, input$rok, input$expo, input$expo2) })
old_mzp <- reactive({
    X330 <- input$n
    X355 <- input$nn
    X364 <- input$nnn

    if (X355 < 0.05) return(X330)
    else if (X355 < 0.5) return((X330 + X355) * 0.5)
    else if (X355 < 5) return(X355)
    else return((X355 + X364) * 0.5)
})

output$plot <- renderPlot({
    kat <- input$kat
    q330 <- input$n
    obd = input$rok
    exponent= input$expo
    exponent1= input$expo2
    
    if (q330<1) exponent1=1.09 else exponent1=1
    mzp = calc_mzp(q330, kat, obd, exponent, exponent1)
    
    exponent=0.85
    mzp3 = calc_mzp(q330, kat, obd, exponent, exponent1)
    
    rada=seq((q330-0.9*q330),(q330+0.9*q330),by=q330/50)
    
    mzp1=rep(0,91)
    mzp2=rep(0,91)
    
    for (i in 1:91)
    {
        mzp1[i] = calc_mzp(rada[i], kat, obd, input$expo, input$expo2)
    
        if (rada[i]<1) exponent1=1.09 else exponent1=1
        exponent=0.85
        mzp2[i] = calc_mzp(rada[i], kat, obd, exponent, exponent1)
    }
    
    bod = old_mzp()
    
    plot(rada, mzp1, type='l', lty=2, lwd=2, xlab='Q330', ylab='MZP',ylim=c(0,max(mzp1)))
    abline(coef=c(mzp3,0), col='red',lwd=0.7, lty=3)
    abline(coef=c(bod,0), col='dark blue',lwd=0.7, lty=3)
    points(q330,mzp3, col='red',lwd=2)
    points(q330,bod,col='dark blue', lwd=2)
    lines(rada, mzp2, type='l', lty=3, lwd=1)
    
    grid()
  })
  
output$plot2 <- renderPlot({
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
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
  
  hranice = readOGR('gis/','cr_sjtsk')
  povo_kat = readOGR('gis/','reg_adam2')
povo_kat=fortify(povo_kat)

  mp = B[, MP[1]]  
  d = B

  ggplot(d) + 
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT)), colour='black', size=5.5,alpha=0.21)+
    geom_point(aes(x=X_COORD, y=Y_COORD,shape = factor(KAT), colour=(pomer)), size=4,alpha=0.6)+
    
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name='Poměr')+
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') + 
    theme_minimal() +
    coord_fixed()+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
  
})

output$plot5 <- renderPlot({
  q330 <- input$n
  obd = input$rok
  kor1= input$expo
  kor= input$expo2
  
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

output$summary <- renderPrint({
    print(new_mzp())
})
})
