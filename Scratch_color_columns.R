## Column {data-width="400"}

### RTD

```{r}


fileData_RTD <- shiny::reactiveFileReader(10000, 
                                          NULL, 
                                          "C:/R/Dash1/CSV/REACTIVE/DASH_RTD_NOMOGRAM.csv", 
                                          read.csv)


DT::renderDataTable({
  DT::datatable(fileData_RTD(),
                rownames = FALSE,
                options = list(paging = FALSE,    ## paginate the output
                               pageLength = 25,  
                               scrollX = FALSE,   
                               scrollY = FALSE,  
                               autoWidth = TRUE,
                               dom = 't')) %>% 
    DT::formatStyle('SHADOW_PRC',
                    backgroundColor = styleInterval(c(-100, 100), 
                                                    c('cyan', 'white', 'coral')),
                    color = 'black', 
                    fontWeight = 'bold') %>% 
    DT::formatStyle('NOMOGRAM_ID',
                    color = 'white', 
                    fontWeight = 'bold')
})




```