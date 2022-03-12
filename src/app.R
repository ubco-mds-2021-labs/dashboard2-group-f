library(dash, pos = .Machine$integer.max)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readxl)
library(hexbin)
library(ggthemes)
library(repr)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)
#library(path.package())???

# Data cleaning and data wrangling
# Load data
#root_dir <- Path(__file__).parent.parent
#data_file <-  root_dir.joinpath('data/processed/data.csv')
path <- '../data/processed/data.csv'
df <-  read_csv(path)

# Create counts dataset
count <- select(df, -wait_time_50, -wait_time_90) %>% drop_na()

#data subsetting
main <- clean %>%
  filter(procedure != 'All Procedures', 
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')
count <- count %>%
  filter(procedure != 'All Procedures', 
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')
all <-  clean %>% 
  filter(procedure == 'All Procedures', 
         hospital == 'All Facilities',
         health_authority == 'All Health Authorities') 

# Create year_quarter column
#df['Y_Q']  <- get quarters in column

# Calling Dash
app <- dash_app()

## Plotting section


# Sidebar components
title <-  htmlH1(
    'BC Surgical Wait Time Dashboard',
    style = list('color': 'var(--bs-primary)')
)

nav <- dbcNav(
    children  = list(
        dbcNavItem(dbcNavLink(
            'Waiting and Completed Cases - Tab 1',
            style = list('border-radius': '0.4rem 0 0 0.4rem'),
            href = '/tab1',
            active = 'exact'
        )),
        dbcNavItem(dbcNavLink(
            'Wait Times, 50th and 90th Percentile - Tab 2',
            style = list('border-radius': '0.4rem 0 0 0.4rem'),
            href = '/tab2',
            active = 'exact'
        ))
    ),
    pills = TRUE,
    vertical = TRUE
)

region_select <-  htmlDiv(
  dbcInputGroup(
      list(
        children <- list(dbcLabel('Health Region'),
                         dccDropdown(
                           options = list(unique(main$health_authority)),
                           multi = True,
                           style = list(
                             'border-radius': '0.4rem 0.4rem 0 0',
                             'width': '100%'
                             ),
                           className = 'dash-bootstrap',
                           id = 'region-select'
                           ),
                         dbcButton(
                           'Select all regions',
                           id = 'region-select-all',
                           style = list(
                             'margin': 0,
                             'border': 0,
                             'border-radius': '0 0 0.4rem 0.4rem',
                             'width': '100%'
                             ),
                           n_clicks = 0,
                           color = 'primary'
                           )
                         ),
        style = {'padding-right': '1rem'}
        )
  )
)

year_slider <- htmlDiv(
  list(
    children = list(
        dbcLabel('Year Range'),
        dccRangeSlider(
            min=2009, 
            max=2022, 
            step=1,
            value = list(2020, 2021),
            #allowCross=FALSE,
            tooltip = list('placement': 'bottom', 'always_visible': True),
            id='year-slider'
            )
        ),
    style = list('margin-top': '1rem')
    )
)

# quarter_radio <- htmlDiv(
#   list(
#     children = list(
#         dbcLabel('Quarter'),
#         dbcRadioItems(
#           list(
#             options = list(
#                 'label': q, 'value': q, 
#                 for q in (list('Q1', 'Q2', 'Q3', 'Q4', 'All'))
#                 ),
#             value = 'All',
#             inline = True,
#             id = 'quarter'
#             )
#         ),
#     ),
#     style = list('margin-top': '1rem')
#     )
# )

sidebar <-  htmlDiv(
  list(
    children = list(
      title,
      htmlHr(style = {'color': 'var(--bs-primary)'}),
      nav,
      htmlHr(style = {'color': 'var(--bs-primary)'}),
      region_select,
      # year_slider,
      # quarter_radio
    ),
    style = list(
        'position': 'fixed',
        'top': 0,
        'left': 0,
        'width': '25rem',
        'height': '100%',
        'padding': '2rem 0 2rem 1rem'
        ),
    className = 'bg-light'
    )
)

#Layout

app %>% set_layout(
  dbcContainer(
    list(
      children = list(
        dccLocation(id = 'url'),
        sidebar,
        htmlDiv(
            id = 'page-content',
            style = list(
                'margin-left': '25rem',
                'padding': '2rem 1rem'
                )
            )
        ),
      fluid = TRUE
      )
    )
)

#' # Callback functions
#' # Navigation
#' @app.callback(
#'     Output('page-content', 'children'),
#'     Input('url', 'pathname')
#' )
#' def render_page_content(pathname):
#'     if pathname == '/tab1':
#'         return tab1
#'     elif pathname == '/tab2':
#'         return tab2
#'     else:
#'         return html.H1('Welcome!', style={'color': 'var(--bs-primary)'})
#' 
#' # Settings
#' @app.callback(
#'     Output('region-select', 'value'),
#'     Input('region-select-all', 'n_clicks'),
#'     State('region-select', 'options')
#' )
#' def select_all_regions(_, regions):
#'     return [region for region in regions]
#' 
#' # Tabs



app %>% run_app()