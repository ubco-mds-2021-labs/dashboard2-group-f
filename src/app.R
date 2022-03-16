library(dash, pos = .Machine$integer.max)
library(plotly)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load data
df <- read.csv('./data/processed/data.csv')

# Initialize variables
health_regions <- unique(df$health_authority)[-1]

dropdown_contents <- list()
for (i in 1:length(health_regions)) {
	dropdown_contents[[i]] <- list(
		'label' = health_regions[i],
		'value' = health_regions[i]
	)
}

# Load in content for tabs
source('./src/tab0.R')
source('./src/tab1.R')
source('./src/tab2.R')

# Sidebar components
title <- h1(
	'BC Surgical Wait Time Dashboard',
	style = list('color' = 'var(--bs-primary)')
)

nav <- dbcNav(
	children = list(
		dbcNavLink(
			'Summary',
			style = list('border-radius' = '0.4rem 0 0 0.4rem'),
			href = '/',
			active = 'exact'
		),
		dbcNavLink(
			'Waiting and Completed Cases',
			style = list('border-radius' = '0.4rem 0 0 0.4rem'),
			href = '/tab1',
			active = 'exact'
		),
		dbcNavLink(
			'Wait Times, 50th and 90th Percentile',
			style = list('border-radius' = '0.4rem 0 0 0.4rem'),
			href = '/tab2',
			active = 'exact'
		)
	),
	pills = TRUE,
	vertical = TRUE
)

region_select <- dbcInputGroup(
	list(
		dbcLabel('Health Region'),
		dccDropdown(
			id = 'region-select',
			options = dropdown_contents,
			multi = TRUE,
			style = list(
				'border-radius' = '0.4rem 0.4rem 0 0',
				'width' = '100%'
			),
			className = 'dash-boostrap'
		),
		dbcButton(
			'Select all regions',
			id = 'region-select-all',
			style = list(
				'margin' = 0,
				'border' = 0,
				'border-radius' = '0 0 0.4rem 0.4rem',
				'width' = '100%'
			),
			n_clicks = 0,
			color = 'primary'
		)
	),
	style = list('padding-right' = '1rem')
)

sidebar <- div(
	list(
		title,
		htmlHr(style = list('color' = 'var(--bs-primary)')),
		nav,
		htmlHr(style = list('color' = 'var(--bs-primary)')),
		region_select
	),
	style = list(
		'position' = 'fixed',
		'top' = 0,
		'left' = 0,
		'width' = '25rem',
		'height' = '100%',
		'padding' = '2rem 0 2rem 1rem'
	),
	className = 'bg-light'
)

# Generate app
app <- Dash$new(external_stylesheets = dbcThemes$MINTY)

app |> set_layout(
  dbcContainer(
  	children = list(
  		dccLocation(id = 'url'),
  		sidebar,
  		div(
  			'Welcome',
  			id = 'page-content',
  			style = list(
  				'margin-left' = '25rem',
  				'padding' = '2rem 1rem'
  			)
  		)
  	),
  	fluid = TRUE
  )
)

# Callbacks
## Navigation
app |> add_callback(
	outputs = output(id = 'page-content', property = 'children'),
	params = input(id = 'url', property = 'pathname'),
	callback = function(pathname) {
		if (pathname == '/tab1') {
			return(tab1)
		} else if (pathname == '/tab2') {
			return(tab2)
		} else {
			return(tab0)
		}
	}
)


## Settings
app |> add_callback(
	outputs = output(id = 'region-select', property = 'value'),
	params = list(
		input(id = 'region-select-all', 'n_clicks'),
		state(id = 'region-select', 'options')
	),
	callback = function(n, regions) {
		return(
			unlist(
				lapply(
					regions,
					function(x) { x$value }
				)
			)
		)
	}
)

## Plots for tab1
app |> add_callback(
    output('plot1', 'figure'),
    list(input('region-select', 'value')),
    function(xcol) {
      main1<-main%>% filter(health_authority %in% xcol)
      top20_pro_waiting<-main1 %>% 
      group_by(procedure) %>% 
      summarise(waitingsum = sum(waiting)) %>% 
      arrange(desc(waitingsum))%>%
      top_n(20)%>%
      select(procedure)
      main_top_procedure_waiting <- filter(main,procedure %in% top20_pro_waiting$procedure)|>
		group_by(procedure,year) %>% 
		summarise(waitingsum = sum(waiting))
      plot1 <- ggplot(main_top_procedure_waiting, aes(x = reorder(procedure,waitingsum,FUN=sum), y=waitingsum, fill = year))+ 
      geom_bar(stat = "identity", show.legend = c(size=FALSE))+
      labs(y = "Total Waiting Cases", x = "Procedure", title = "Number of Waiting Cases for Different Procedure Groups")+
      theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))+
      coord_flip()
      ggplotly(plot1,width = 800)
    }
)

app |> add_callback(
    output('plot2', 'figure'),
    list(input('region-select', 'value')),
    function(xcol) {
      main2<-main%>% filter(health_authority %in% xcol)
      top20_pro_completed<-main2 %>% 
      group_by(procedure) %>% 
      summarise(completedsum = sum(completed)) %>% 
      arrange(desc(completedsum))%>%
      top_n(20)%>%
      select(procedure)
      main_top_procedure_completed <- filter(main,procedure %in% top20_pro_completed$procedure)|>
		group_by(procedure,year) %>% 
		summarise(completedsum = sum(completed))
      plot2 <- ggplot(main_top_procedure_completed, aes(x = reorder(procedure,completedsum,FUN=sum), y=completedsum, fill = year))+ 
              geom_bar(stat = "identity", show.legend = c(size=FALSE))+
              labs(y = "Total Completed Cases", x = "Procedure", title = "Number of Completed Cases for Different Procedure Groups")+
              theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))+
              coord_flip()
      ggplotly(plot2,width = 800)
    }
)

app |> add_callback(
    output('plot3', 'figure'),
    list(input('region-select', 'value')),
    function(xcol) {
      main3<-main%>% filter(health_authority %in% xcol)
      top20_hos_waiting<-main3 %>% 
      group_by(hospital) %>% 
      summarise(waitingsum = sum(waiting)) %>% 
      arrange(desc(waitingsum))%>%
      top_n(20)%>%
      select(hospital)
      main_top_hospital_waiting<- filter(main,hospital %in% top20_hos_waiting$hospital)|>
		group_by(hospital,year) %>% 
		summarise(waitingsum = sum(waiting))
      plot3 <- ggplot(main_top_hospital_waiting, aes(x = reorder(hospital,waitingsum,FUN=sum), y=waitingsum, fill = year))+ 
              geom_bar(stat = "identity", show.legend = c(size=FALSE))+
              labs(y = "Total Waiting Cases", x = "Hospital", title = "Number of Waiting Cases for Different Hospitals")+
              theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))+
              coord_flip()
      ggplotly(plot3,width = 800)
    }
)

app |> add_callback(
    output('plot4', 'figure'),
    list(input('region-select', 'value')),
    function(xcol) {
      main_4<-main%>% filter(health_authority %in% xcol)
      top20_hos_completed<-main_4 %>% 
      group_by(hospital) %>% 
      summarise(completedsum = sum(completed)) %>% 
      arrange(desc(completedsum))%>%
      top_n(20)%>%
      select(hospital)
      main_top_hospital_completed <- filter(main,hospital %in% top20_hos_completed$hospital)|>
		group_by(hospital,year) %>% 
		summarise(completedsum = sum(completed))
      plot4 <- ggplot(main_top_hospital_completed, aes(x = reorder(hospital,completedsum,FUN=sum), y=completedsum, fill = year))+ 
              geom_bar(stat = "identity", show.legend = c(size=FALSE))+
              labs(y = "Total Completed Cases", x = "Hospital", title = "Number of Completed Cases for Different Hospitals")+
              theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(size=10))+
              coord_flip()
      ggplotly(plot4,width = 800)
    }
)

app |> add_callback(
    output('plot5', 'figure'),
    list(input('region-select', 'value')),
    function(xcol) {
      all_line<-count%>% 
      filter(health_authority %in% xcol) %>%
      filter(procedure=='All Procedures',hospital=='All Facilities')%>%
      group_by(Y_Q) %>% 
      summarise(waiting = sum(waiting),completed=sum(completed))%>%
      select(Y_Q, waiting, completed) %>%
      gather(key = "variable", value = "value", -Y_Q)
      p<-ggplot(all_line)+
      aes(x = Y_Q,
      y = value,
      color = variable)+
      geom_point(size = 1)+
      geom_line(
        aes(group = variable), size = 1)+
        labs(x = "Year & Quarter", y = "Number of Cases", color = "")+ 
        ggtitle("Total Number of Waiting & Completed Cases")+
        theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),text = element_text(size=10),plot.title = element_text(size=12))
      ggplotly(p)
    }
)

# ==================================================================================================
# Plots for tab2
# ==================================================================================================

# Line plot
app |> add_callback(
    output('lineplot', 'figure'),
    list(input('region-select', 'value')),
    function(region){
      subdata <- df |>
      filter(health_authority %in% region) |>
      filter(procedure=='All Procedures', hospital=='All Facilities') |>
      group_by(Y_Q) |> 
      summarise(wait_time_50 = mean(wait_time_50), wait_time_90=mean(wait_time_90)) |>
      select(Y_Q, wait_time_50, wait_time_90) |>
      gather(key = "variable", value = "value", -Y_Q)
      plot <- ggplot(subdata) + aes(x = Y_Q, y = value, color = variable) +
      geom_point(size = 2) +
      geom_line(
        aes(group = variable), size = 1) +
        labs(x = "Year & Quarter", y = "Wait Time (weeks)", color = "", title = "50th and 90th Percentile Waiting Times") + 
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), text = element_text(size=10), plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    ggplotly(plot)}
)


# Side by side tick plots for procedures
app |> add_callback(
    output('50perc_procedure', 'figure'),
    list(input('region-select', 'value')),
    function(region) {
        sub_data <- main_t2 |> filter(health_authority %in% region)
        top<-sub_data |>
        group_by(procedure) |>
        summarise(wait_time_50 = mean(wait_time_50)) |>
        arrange(desc(wait_time_50)) |>
        top_n(20) |>
        select(procedure)
    subdata_top <- filter(sub_data, procedure %in% top$procedure) |>
        group_by(procedure, year) |>
        summarise(wait_time_50 = mean(wait_time_50))
    plot <- ggplot(subdata_top) +
        aes(x = wait_time_50, y = reorder(procedure, wait_time_50), color = year) +
        geom_point(size = 3, shape = '|') + # doesn't have to be this shape
        geom_point(stat = 'summary', fun = mean, color='red', size = 4) +
        theme(legend.position = "right") +
        labs(y = "Procedure", x = "Wait Time (weeks)", title = "Waiting Times for 50 percent of Cases by Procedure") +
        theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(size = 10, , face = "bold", hjust = 0.5))
        # + scale_color_manual(breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022))
    ggplotly(plot, width = 800, height = 500)
#         scale_x_continuous(labels = scales::label_dollar(), 
#                            breaks = scales::pretty_breaks(n = 10)) +
#         scale_fill_continuous(labels = scales::label_number_si())
    }
)

app |> add_callback(
    output('90perc_procedure', 'figure'),
    list(input('region-select', 'value')),
    function(region) {
        sub_data <- main_t2 |> filter(health_authority %in% region)
        top<-sub_data |>
        group_by(procedure) |>
        summarise(wait_time_90 = mean(wait_time_90)) |>
        arrange(desc(wait_time_90)) |>
        top_n(20) |>
        select(procedure)
    subdata_top <- filter(sub_data, procedure %in% top$procedure) |>
        group_by(procedure, year) |>
        summarise(wait_time_90 = mean(wait_time_90))
    plot <- ggplot(subdata_top) +
        aes(x = wait_time_90, y = reorder(procedure, wait_time_90), color = year) +
        geom_point(size = 3, shape = '|') + # doesn't have to be this shape
        geom_point(stat = 'summary', fun = mean, color='red', size = 4) +
        theme(legend.position = "right") +
        labs(y = "Procedure", x = "Wait Time (weeks)", title = "Waiting Times for 90 percent of Cases by Procedure") +
        theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
    ggplotly(plot, width = 800, height = 500)
    }
)


# Side by side tick plots for hospitals
app |> add_callback(
    output('50perc_hospital', 'figure'),
    list(input('region-select', 'value')),
    function(region) {
        sub_data <- main_t2 |> filter(health_authority %in% region)
        top<-sub_data |>
        group_by(hospital) |>
        summarise(wait_time_50 = mean(wait_time_50)) |>
        arrange(desc(wait_time_50)) |>
        top_n(20) |>
        select(hospital)
    subdata_top <- filter(sub_data, hospital %in% top$hospital) |>
        group_by(hospital, year) |>
        summarise(wait_time_50 = mean(wait_time_50))
    plot <- ggplot(subdata_top) +
        aes(x = wait_time_50, y = reorder(hospital, wait_time_50), color = year) +
        geom_point(size = 3, shape = '|') + # doesn't have to be this shape
        geom_point(stat = 'summary', fun = mean, color='red', size = 4) +
        theme(legend.position = "right") +
        labs(y = "Hospital", x = "Wait Time (weeks)", title = "Waiting Times for 50 percent of Cases by Hospitals") +
        theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
    ggplotly(plot, width = 800, height = 500)
    }
)

app |> add_callback(
    output('90perc_hospital', 'figure'),
    list(input('region-select', 'value')),
    function(region) {
        sub_data <- main_t2 |> filter(health_authority %in% region)
        top<-sub_data |>
        group_by(hospital) |>
        summarise(wait_time_90 = mean(wait_time_90)) |>
        arrange(desc(wait_time_90)) |>
        top_n(20) |>
        select(hospital)
    subdata_top <- filter(sub_data, hospital %in% top$hospital) |>
        group_by(hospital, year) |>
        summarise(wait_time_90 = mean(wait_time_90))
    plot <- ggplot(subdata_top) +
        aes(x = wait_time_90, y = reorder(hospital, wait_time_90), color = year) +
        geom_point(size = 3, shape = '|') + # doesn't have to be this shape
        geom_point(stat = 'summary', fun = mean, color='red', size = 4) +
        theme(legend.position = "right") +
        labs(y = "Hospital", x = "Wait Time (weeks)", title = "Waiting Times for 90 percent of Cases by Hospitals") +
        theme(text = element_text(size=10), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(size = 10, face = "bold", hjust = 0.5))
    ggplotly(plot, width = 800, height = 500)
    }
)

# Run app
app |> run_app()

# Run app
# app$run_server(host = '0.0.0.0')