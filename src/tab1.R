# data wrangling
df$Y_Q <- paste(df$year,df$quarter)
count <- select(df, -wait_time_50, -wait_time_90)
count <- count |> 
    drop_na() 
main <- count |> 
            filter(procedure != 'All Procedures', 
                   hospital != 'All Facilities',
                   health_authority != 'All Health Authorities')
# tab1 layout
tab1 <- list(
    dbcRow(
      dbcCol(dccGraph(id='plot5'))
    ),
    dbcRow(
        list(
          dbcCol(dccGraph(id='plot1')),
          dbcCol(dccGraph(id='plot2'))
        )
    ),
    dbcRow(
        list(
          dbcCol(dccGraph(id='plot3')),
          dbcCol(dccGraph(id='plot4'))
        )
    )
)
