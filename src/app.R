library(dash, pos = .Machine$integer.max)
library(plotly)
library(ggplot2)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)

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
		return(regions)
	}
)

# Run app
app |> run_app()
