shinyServer(function(input, output) {
	data <- reactive({
		inFile <- input$file1
		if (is.null(inFile))
			return(NULL)
		read.xlsx(inFile$datapath,1)
	})
	samples <- reactive({
		inFile2 <- input$file2
		if (is.null(inFile2))
			return(NULL)
		read.xlsx(inFile2$datapath,1)
	})

	output$plot_button <- renderUI ({ if(!is.null(input$file1)) { actionButton("plot", "Fit") } })
	output$sample_button <- renderUI ({ if(!is.null(input$file2)) { actionButton("calculate", "Calculate samples") } })

	fit <- reactive({
	  data <- data()
	  drm(OD~conc, data = data, fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
	})

	fitted_samples <- reactive({
	  fit <- fit()
	  samples <- samples()
	  samples$conc <- signif(coef(fit)[4]*(((-1*coef(fit)[3]+samples$OD)/(coef(fit)[2]-samples$OD))^(1/coef(fit)[1])),digits = 3)
	  samples
	})

	newdata <- reactive({
	  data <- data()
	  fit <- fit()
	  newdata <- expand.grid(conc=exp(seq(log(min(data$conc)), log(max(data$conc)), length=100)))
	  pm <- predict(fit, newdata=newdata, interval="confidence")
	  newdata$p <- pm[,1]
	  newdata$pmin <- pm[,2]
	  newdata$pmax <- pm[,3]
	  newdata
	})

	observeEvent(input$plot, {
  	output$scatter <- renderPlot({
  	  data <- data()
  	  newdata <- newdata()
  	  fit <- fit()
  	  R2 = signif(1 - (sum(fit$predres[,2]^2) / sum((data$OD-mean(data$OD))^2)), digits = 3)
  	  ggplot(data = data, aes(x = conc, y = OD)) +  geom_point() +  geom_ribbon(data=newdata, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) + geom_line(data=newdata, aes(x=conc, y=p)) +
  	    labs(list(title = "ELISA", x ="Concentration (mg/ml)", y = "OD")) + theme_bw() + scale_x_log10(breaks=c(data$conc)) + annotate("text", x = min(data$conc)*2, y = max(data$OD)-max(data$OD)*0.1, label = paste("R2 = ", R2))
  	 })

  	output$backfit <- DT::renderDataTable({
  	  fit <- fit()
  	  signif(backfit(fit),digits = 3)
  	}, options = list(pageLength = 20), selection = 'none')

  	output$fitting <- DT::renderDataTable({
  	  fit <- fit()
  	  signif(summary(fit)$coefficients[,1:2],digits = 3)
  	}, options = list(pageLength = 20), selection = 'none')
	})

	observeEvent(input$calculate, {
	  output$scatter <- renderPlot({
	    data <- data()
	    newdata <- newdata()
	    fitted_samples <- fitted_samples()
	    fit <- fit()
	    R2 = signif(1 - (sum(fit$predres[,2]^2) / sum((data$OD-mean(data$OD))^2)), digits = 3)
	    ggplot(data = data, aes(x = conc, y = OD)) +  geom_point() +  geom_ribbon(data=newdata, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) + geom_line(data=newdata, aes(x=conc, y=p)) +
	      labs(list(title = "ELISA", x ="Concentration (mg/ml)", y = "OD")) + theme_bw() + scale_x_log10(breaks=c(data$conc)) + geom_point(data = fitted_samples, aes(x=conc, y=OD), col="blue") +
	      geom_segment(data = fitted_samples, aes(x = conc, y = 0, xend = conc, yend = OD), col = "blue", linetype = 2) + annotate("text", x = min(data$conc)*2, y = max(data$OD)-max(data$OD)*0.1, label = paste("R2 = ", R2))
	  })

  	output$sample_table <- DT::renderDataTable({
  	  fitted_samples()
  	}, options = list(pageLength = 20), selection = 'none')
	})
})
