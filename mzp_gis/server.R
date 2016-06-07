library(shiny)
library(data.table)
library(ggplot2)
library(rgdal)

calc_mzp <- function(q330, q355, Qa, kat, obd) {
    coeffs = list(leto = c(1.1, 1.2, 1.05, 1.07), zima = c(1, 1, 1, 1))
    season = "leto"
    if (obd == 22)
        season = "zima"
    return((1-(q355/Qa))*q330*coeffs[[season]][as.numeric(kat)])
}

calc_old_mzp <- function(X330, X355, X364) {
    ifelse(X355 < 0.05, X330, ifelse(X355 < 0.5, (X330 + X355) * 0.5, ifelse(X355 < 5, X355, (X355 + X364) * 0.5)))
}

plot_map <- function(dta, var_name, color_title) {
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
    geom_point(aes_string(x = "X_COORD", y = "Y_COORD", shape = "factor(KAT)", colour = var_name), size=4,alpha=0.6)+
    scale_colour_gradient2(low = ("red"), mid = 'green', high=('blue'), name = color_title) +
    scale_shape_discrete(name='Kat')+
    xlab('long') +
    ylab('lat') +
    theme_minimal() +
    coord_fixed()+
    geom_polygon(aes(x=long, y = lat), data = hranice, fill = NA, col='grey',lwd=0.3)+
    geom_polygon(aes(x=long, y = lat, group=group), fill=povo_kat$group, data = povo_kat, col='red',lwd=0.3,alpha=0.1)
}

shinyServer(function(input, output) {
new_mzp <- reactive({ calc_mzp(input$n, input$nn, input$nnnn, input$kat, input$rok) })
old_mzp <- reactive({ calc_old_mzp(input$n, input$nn, input$nnn) })

output$plot <- renderPlot({
    kat <- input$kat
    q330 <- input$n
    obd = input$rok
    q355 <- input$nn
    qa  <- input$nnnn

    mzp = calc_mzp(q330, q355, qa, kat, obd)
    
    exponent=0.85
    mzp3 = calc_mzp(q330, q355, qa, kat, obd)
    
    rada=seq((q330-0.9*q330),(q330+0.9*q330),by=q330/50)
    
    mzp1=rep(0,91)
    mzp2=rep(0,91)
    
    for (i in 1:91)
    {
        mzp1[i] = calc_mzp(rada[i], q355, qa, kat, obd)

        mzp2[i] = calc_mzp(rada[i], q355, qa, kat, obd)
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

plot_data <- reactive({
  newdta=as.data.table(read.table('MINprutoky/chmu_stat2.dat', header=TRUE))
  newdta[,K99:=X99/X50]
  q330 <- input$n
  q355 = input$nn
  qa = input$nnnn
  newdta[, MZPleto := round(calc_mzp(q330, q355, qa, KAT, "leto"), 3)]
  newdta[, MZPzima := round(calc_mzp(q330, q355, qa, KAT, "zima"), 3)]
  newdta[,pomer:=(MZPleto/AVG)*100]
  newdta[,pomer_zima:=(MZPzima/AVG)*100]

  newdta[, MZPold := calc_old_mzp(X330, X355, X364)]

  newdta[,rozdil_hlavni:=MZPleto-MZPold]
  newdta[,rozdil_jaro:=MZPzima-MZPold]
  newdta[,rozdil_proc_hlav:=(rozdil_hlavni/MZPold)*100]
  newdta[,rozdil_proc_jaro:=(rozdil_jaro/MZPold)*100]

  newdta[, MZPleto1 := round(calc_mzp(q330, q355, qa, KAT, "leto"), 3)]
  newdta[, MZPzima1 := round(calc_mzp(q330, q355, qa, KAT, "zima"), 3)]
  newdta[,pomer1:=(MZPleto1/AVG)*100]

  return(newdta)
})
  
output$plot2 <- renderPlot({
  ggplot(plot_data(), aes(factor=KAT))+
    geom_boxplot(aes(x=KAT,y=rozdil_proc_hlav, group=KAT))+
    xlab('Kategorie')+
    ylab('Změna [%]')+
    theme(legend.position="none")
})

output$plot3 <- renderPlot({
  ggplot(plot_data(), aes(factor=KAT))+
    geom_point(aes(x=X330,y=pomer1, group=KAT,alpha=0.5), colour='dark blue')+
    geom_point(aes(x=X330,y=pomer, group=KAT), colour = 'red' ,alpha=0.5)+
    ylim(0,100)+
    facet_grid(.~KAT, scales='free_x')+
    xlab('Qa')+
    ylab('Poměr [%]')+
    theme(legend.position="none")
})

output$plot4 <- renderPlot({
    plot_map(plot_data(), "pomer", "Poměr")
})

output$plot5 <- renderPlot({
    plot_map(plot_data(), "pomer_zima", "Poměr")
})

output$plot6 <- renderPlot({
    plot_map(plot_data(), "rozdil_proc_hlav", "Změna")
})

output$plot7 <- renderPlot({
    plot_map(plot_data(), "rozdil_proc_jaro", "Změna")
})

output$summary <- renderPrint({
    print(new_mzp())
})
})
