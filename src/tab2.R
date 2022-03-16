# data wrangling
df$Y_Q <- paste(df$year, df$quarter)
main_t2 <- na.omit(df)
main_t2 <- main_t2 |> filter(procedure != 'All Procedures', 
                         hospital != 'All Facilities',
                         health_authority != 'All Health Authorities')

# tab1 layout
tab2 <- list(
    dbcRow(
        dbcCol(dccGraph(id='lineplot'))
    ),
    dbcRow(
        list(
            dbcCol(dccGraph(id='50perc_procedure')),
            dbcCol(dccGraph(id='90perc_procedure')))
    ),
    dbcRow(
        list(
            dbcCol(dccGraph(id='50perc_hospital')),
            dbcCol(dccGraph(id='90perc_hospital'))
        ))
)
