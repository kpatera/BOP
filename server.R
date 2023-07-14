shinyServer(function(input, output, session) {
  dataset1<- reactive({
    req(input$Indata1) # avoid error messages before loading of data
    Table_input <- data.frame(get(input$Indata1))
    return(Table_input)
  })
  
  dataset2<- reactive({
    req(input$Indata2)
    inFile <- input$Indata2
    Table_input<-data.frame(read_excel(inFile$datapath,sheet=1))
    return(Table_input)
  })
  
  output$distPlot1 <- renderPlot({
    
    if(input$datainput=="Demos"){Table_input<<-dataset1()}
    if(input$datainput=="Data"){Table_input<<-dataset2()}
    
    
    Table_input_AE<-Table_input_AE_vague<-matrix(NA,dim(Table_input)[1],3)
    Table_input[,3]<-as.factor(Table_input[,3])
    up=input$rellevel
    dw=1/input$rellevel
    
        
 inputref=input$reflevel
 inputconf=input$conflevel
  out_up<-out_dw<-out<-matrix(NA,ncol = 2, nrow = length(multip))
  for(i in 1: length(multip)){
    out[i,]=1/findbeta2(themean = multip[i]*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*(inputconf/100000))
    out_up[i,]=1/findbeta2(themean = multip[i]*up*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*up*(inputconf/100000))
    out_dw[i,]=1/findbeta2(themean = multip[i]*dw*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*dw*(inputconf/100000))
  }

    
    for(i in 1:dim(Table_input)[1]){
      Table_input_AE[i,]<-100000*qbeta(c(0.025, 0.5, 0.975),
                                       shape1 = input$X_number + Table_input[i,1],
                                       shape2 = 1 + Table_input[i,2] - Table_input[i,1])
    }
    Table2b<<-data.frame(cbind(round(Table_input_AE,3)))
    names(Table2b)<-c("LL","Median","UL")
    n.sims<-input$sims
    # bo1=multip*elicit[2]*10/100000
    # bo2=multip*elicit[2]/100000
    # bo3=multip*elicit[2]/(10*100000)
    # ao=multip*elicit[1]/100000
    
    AE<-Table_input[,c(1,2)]
    set.seed(as.numeric(as.numeric(Sys.time()))/1000)
    AE_out<-matrix(NA,nrow = 22,ncol = 3)
    AE_out<-cbind(Table2out_fun(SAE=AE, a0 = 1/out_up[,1], b0 = 1/out_up[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical,
                  Table2out_fun(SAE=AE, a0 = 1/out[,1], b0 = 1/out[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical,
                  Table2out_fun(SAE=AE, a0 = 1/out_dw[,1], b0 = 1/out_dw[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical)
    AE_out<-data.frame(AE_out)
    
    
    for(i in 1:dim(Table_input)[1]){
      Table_input_AE_vague[i,]<-100000*qbeta(c(0.025, 0.5, 0.975),
                                             shape1 = 1 + Table_input[i,1],
                                             shape2 = input$X_number + Table_input[i,2] - Table_input[i,1])
    }
    Table2b_vauge<<-data.frame(cbind(round(Table_input_AE_vague,3)))
    names(Table2b)<-c("LL","Median","UL")

    AE_vague<-Table_input[,c(1,2)]
    set.seed(as.numeric(as.numeric(Sys.time()))/1000)
    AE_out_vague<-matrix(NA,nrow = 22,ncol = 3)
    
    AE_out_vague<-cbind(Table2out_fun(SAE=AE_vague, a0 = 1/out_up[,1], b0 = 1/out_up[,2], n.sims = n.sims, extra=Table2b_vauge,ar = 1, br = 10^6)$Analytical,
                      Table2out_fun(SAE=AE_vague, a0 = 1/out[,1], b0 = 1/out[,2], n.sims = n.sims, extra=Table2b_vauge, ar = 1, br = 10^6)$Analytical,
                      Table2out_fun(SAE=AE_vague, a0 = 1/out_dw[,1], b0 = 1/out_dw[,2], n.sims = n.sims, extra=Table2b_vauge, ar = 1, br = 10^6)$Analytical)
    AE_out_vague<-data.frame(AE_out_vague)
    
    
    names(AE_out)<-names(AE_out_vague)<-c("Level 1 (Higher rate)","Level 2 (Reference rate)","Level 3 (Lower rate)")
    
    
    typeofgraph=input$typegraph
    compareT=input$compareT
    if(typeofgraph=="Line BOPlot"){
      if(compareT=="No"){
        plot(1:N,AE_out$`Level 1 (Higher rate)`,type = "l",col="yellow",ylim=c(0,1),
             lwd=4,main = "Adverse effect",ylab="Pr(SAE>Threshold)",xlab="",#xlab="Time (Months)",
             xaxt="n")
        axis(1, at=1:N,labels=(Table_input[,3]),lty="solid", col.axis="black", las=2)
        lines(1:N,AE_out$`Level 2 (Reference rate)`,lty="solid",type = "l",col="orange",lwd=4)
        lines(1:N,AE_out$`Level 3 (Lower rate)`,lty="solid",type = "l",col="red",lwd=4)
      }
      if(compareT=="Yes"){
        plot(1:N,AE_out$`Level 1 (Higher rate)`,lty="dashed",type = "l",col="yellow",ylim=c(0,1),
             lwd=4,main = "Adverse effect",ylab="Pr(SAE>Threshold)",xlab="",#xlab="Time (Months)",
             xaxt="n")
        axis(1, at=1:N,labels=(Table_input[,3]), col.axis="black", las=2)
        lines(1:N,AE_out$`Level 2 (Reference rate)`,lty="dashed",type = "l",col="orange",lwd=4)
        lines(1:N,AE_out$`Level 3 (Lower rate)`,lty="dashed",type = "l",col="red",lwd=4)
        
        lines(1:N,AE_out_vague$`Level 1 (Higher rate)`,type = "l",col="yellow",lwd=4)
        lines(1:N,AE_out_vague$`Level 2 (Reference rate)`,type = "l",col="orange",lwd=4)
        lines(1:N,AE_out_vague$`Level 3 (Lower rate)`,type = "l",col="red",lwd=4)
      }
      
    }else{
      plot(1:N,AE_out$`Level 1 (Higher rate)`,lty="dashed",type = "l",col="yellow",ylim=c(0,1),
           lwd=4,main = "Adverse effect",ylab="Pr(SAE>Threshold)",xlab="",#xlab="Time (Months)",
           xaxt="n")
      axis(1, at=1:N,labels=(Table_input[,3]), col.axis="black", las=2)
      lines(1:N,AE_out$`Level 2 (Reference rate)`,lty="dashed",type = "l",col="orange",lwd=4)
      lines(1:N,AE_out$`Level 3 (Lower rate)`,lty="dashed",type = "l",col="red",lwd=4)

      #abline(a=0.5,b=0,lty=4,lwd=5,col="gray")
      grid(NA,5,lwd=2)
      
      
      polygon(c(1:dim(AE_out)[1], rev(1:dim(AE_out)[1])), 
              c(rep(1,dim(AE_out)[1]), 
                rev(AE_out$`Level 3 (Lower rate)`)),col = "khaki1")
      
      polygon(c(1:dim(AE_out)[1], rev(1:dim(AE_out)[1])), 
              c(AE_out$`Level 3 (Lower rate)`, 
                rev(AE_out$`Level 2 (Reference rate)`)),col = "yellow")
      
      polygon(c(1:dim(AE_out)[1], rev(1:dim(AE_out)[1])), 
              c(AE_out$`Level 2 (Reference rate)`, 
                rev(AE_out$`Level 1 (Higher rate)`)),col = "orange")
      
      polygon(c(1:dim(AE_out)[1], rev(1:dim(AE_out)[1])), 
              c(AE_out$`Level 1 (Higher rate)`, 
                rev(rep(0,dim(AE_out)[1]))),col = "red4")
      
    }
  }, res = 100,height = 650)
  
  output$table1 <- renderDataTable({
    if(input$datainput=="Demos"){Table_input<<-dataset1()}
    if(input$datainput=="Data"){Table_input<<-dataset2()}
    
    Table_input_AE<-matrix(NA,dim(Table_input)[1],3)
    Table_input_AE<-Table_input_AE_vague<-matrix(NA,dim(Table_input)[1],3)
    Table_input[,3]<-as.factor(Table_input[,3])
    up=input$rellevel
    dw=1/input$rellevel
    
    
    inputref=input$reflevel
    inputconf=input$conflevel
    out_up<-out_dw<-out<-matrix(NA,ncol = 2, nrow = length(multip))
    for(i in 1: length(multip)){
      out[i,]=1/findbeta2(themean = multip[i]*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*(inputconf/100000))
      out_up[i,]=1/findbeta2(themean = multip[i]*up*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*up*(inputconf/100000))
      out_dw[i,]=1/findbeta2(themean = multip[i]*dw*(inputref/100000),percentile = 0.95,percentile.value = multip[i]*dw*(inputconf/100000))
    }
    
    
    for(i in 1:dim(Table_input)[1]){
      Table_input_AE[i,]<-100000*qbeta(c(0.025, 0.5, 0.975),
                                       shape1 = input$X_number + Table_input[i,1],
                                       shape2 = 1 + Table_input[i,2] - Table_input[i,1])
    }
    Table2b<<-data.frame(cbind(round(Table_input_AE,3)))
    names(Table2b)<-c("LL","Median","UL")
    n.sims<-input$sims
    # bo1=multip*elicit[2]*10/100000
    # bo2=multip*elicit[2]/100000
    # bo3=multip*elicit[2]/(10*100000)
    # ao=multip*elicit[1]/100000
    
    AE<-Table_input[,c(1,2)]
    set.seed(as.numeric(as.numeric(Sys.time()))/1000)
    AE_out<-matrix(NA,nrow = 22,ncol = 3)
    AE_out<-cbind(Table2out_fun(SAE=AE, a0 = 1/out_up[,1], b0 = 1/out_up[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical,
                  Table2out_fun(SAE=AE, a0 = 1/out[,1], b0 = 1/out[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical,
                  Table2out_fun(SAE=AE, a0 = 1/out_dw[,1], b0 = 1/out_dw[,2], n.sims = n.sims, extra=Table2b, ar = input$X_number, br = 200-input$X_number)$Analytical)
    AE_out<-data.frame(AE_out)
    
    
    for(i in 1:dim(Table_input)[1]){
      Table_input_AE_vague[i,]<-100000*qbeta(c(0.025, 0.5, 0.975),
                                             shape1 = 1 + Table_input[i,1],
                                             shape2 = input$X_number + Table_input[i,2] - Table_input[i,1])
    }
    Table2b_vauge<<-data.frame(cbind(round(Table_input_AE_vague,3)))
    names(Table2b)<-c("LL","Median","UL")
    
    AE_vague<-Table_input[,c(1,2)]
    set.seed(as.numeric(as.numeric(Sys.time()))/1000)
    AE_out_vague<-matrix(NA,nrow = 22,ncol = 3)
    
    AE_out_vague<-cbind(Table2out_fun(SAE=AE_vague, a0 = 1/out_up[,1], b0 = 1/out_up[,2], n.sims = n.sims, extra=Table2b_vauge, ar = 1, br = 10^6)$Analytical,
                        Table2out_fun(SAE=AE_vague, a0 = 1/out[,1], b0 = 1/out[,2], n.sims = n.sims, extra=Table2b_vauge, ar = 1, br = 10^6)$Analytical,
                        Table2out_fun(SAE=AE_vague, a0 = 1/out_dw[,1], b0 = 1/out_dw[,2], n.sims = n.sims, extra=Table2b_vauge, ar = 1, br = 10^6)$Analytical)
    AE_out_vague<-data.frame(AE_out_vague)
    
    
    names(AE_out)<-names(AE_out_vague)<-c("Level 1 (Higher rate)","Level 2 (Reference rate)","Level 3 (Lower rate)")
    
    
    ar=1; br=1
    
    
    for(i in 1:dim(Table_input)[1]){
      Table_input_AE[i,]<-100000*qbeta(c(0.025, 0.5, 0.975),
                                       shape1 = ar + Table_input[i,1],
                                       shape2 = br + Table_input[i,2] - Table_input[i,1])
    }
    Table2b<<-data.frame(cbind(round(Table_input_AE,3)))
    names(Table2b)<<-c("LL","Median","UL")
    
    Infstat<-round(abs((AE_out[,2]+0.01)-(AE_out_vague[,2]+0.01)),2);Infstat[1]<-NA
    Infstat_cond<-Infstat<0.05
    Table1b_Out<-data.frame(cbind(Table_input,paste0(Table2b[,2],"(",Table2b[,1],",", Table2b[,3],")"),Infstat,Infstat_cond))
    colnames(Table1b_Out)<-c("Events","Sample_size","Date","Pr100000_Median_25q_975q_vague","Inf.statistic","Inf.stat.larger_0.1")
    
    #Table1b_Out
    datatable(data.frame(Table1b_Out),
              extensions = 'Buttons',options = list(lengthMenu = c(10, 20, 100), 
                                                    pageLength = 100, dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  })
  
  # output$single<- renderUI({
  #   ar=input$ar_prior_single; br=input$br_prior_single
  #   
  #   events_single<-input$events
  #   sample_single<-input$sample
  #   
  #   Table_input_AE_single<-100000*qbeta(c(0.025, 0.5, 0.975),
  #                                       shape1 = ar + events_single,
  #                                       shape2 = br + sample_single - events_single)
  #   x=seq(0,1,length.out=1000)
  #   plot(x,dbeta(x,shape1 = ar + events_single,
  #                shape2 = br + sample_single - events_single),type = "l",
  #        ylab="Density", xlab="",main="Prior/Posterior distributions",lwd=2)
  #   lines(x,dbeta(x,shape1 = ar,
  #                 shape2 = br),type = "l",lwd=3,lty=5,col="gray")
  #   legend("topright",legend = c("Posterior","Prior"),col = c("black","gray"),lty = c(1,5),lwd=c(3,3))
  # })
  # 
  # output$Rmark <- renderUI({
  #   includeHTML(rmarkdown::render(paste0("./BOP/SingleSample.Rmd"), 
  #                                 quiet = TRUE,output_format = "html_document"))
  # })
  
  
  output$table2 <- renderDataTable({
    if(input$datainput=="Demos"){Table_input<<-dataset1()}
    if(input$datainput=="Data"){Table_input<<-dataset2()}
    datatable(data.frame(Table_input),
              extensions = 'Buttons',options = list(lengthMenu = c(10, 20, 100), 
                                                    pageLength = 100, dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
                             })
  
  output$reflevel <- renderUI({
    if(input$Indata1 == "Example_ErythemaM"){
      ref_val<<-0.7
      conf_val<<-0.7/4
      }
    if(input$Indata1 == "Example_Myocarditis"){
      ref_val<<-1.555
      conf_val<<-0.11
    }
    if(input$Indata1 == "Example_MIS"){
      ref_val<<-0.51
      conf_val<<-0.51/4
    }
    div(style="display:inline-block;width:100%",
        numericInput("reflevel",
                     label = "Reference (general population) event rate:",
                     value = ref_val,
                     min = 0.01,
                     max = 10000,step = 10),
        numericInput("conflevel",
                     label = "95% confidence lower limit of the reference rate:",
                     value = conf_val,
                     min = 0.01,
                     max = 10000,step = 10))    
   })
  
  output$datasets <- renderUI({
    if(input$datainput=="Data"){
      source("Functions/Interface_Data.R",local = TRUE)$value
    }else{
      source("Functions/Interface_Demos.R",local = TRUE)$value
    }
  })
  
  outputOptions(output, "datasets", suspendWhenHidden = FALSE)
  
  
})
