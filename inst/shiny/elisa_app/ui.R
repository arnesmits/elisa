shinyUI(
	dashboardPage(
		dashboardHeader(title = "ELISA App"),
		dashboardSidebar(
			fileInput('file1', 'Standard Curve',accept=c('xlsx', 'xls')),
			fileInput('file2', 'Samples',accept=c('xlsx', 'xls')),
			tags$hr(),
			uiOutput("plot_button"),
			uiOutput("sample_button")
			),
		dashboardBody(
			fluidRow(
			  box(title = "Fit",plotOutput("scatter"), width = 8)
			),
			fluidRow(
			  box(title = "Fit parameters", DT::dataTableOutput("fitting"), width = 4),
			  box(title = "Backfit", DT::dataTableOutput("backfit"), width = 4),
			  box(title = "Samples", DT::dataTableOutput("sample_table"), width = 4)
			)
		)
	)
)
