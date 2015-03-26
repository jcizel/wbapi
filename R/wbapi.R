##' @title wbapi: R Interface to the World Bank API
##'
##' 
##' @return List of functions that perform calls to the World Bank API
##' @author Janko Cizel
##' @export
wbapi <- function(){
    root = 'http://api.worldbank.org'
    
    sources = function(){
        query = 'sources'
        url = sprintf(
            "%s/%s?format=XML",
            root,
            query
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        stri_split(regex = '\\r\\n') %>>%
        (.[[1L]][-1]) %>>%
        paste(collapse = "\n") %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        xmlApply(xmlRoot(l), function(x){
            c(
                x %>>% xmlAttrs,
                x %>>% xmlChildren %>>% list.map(. %>>% xmlValue)
            ) %>>% as.data.table
        }) %>>%
        rbindlist ->
            dt
               
        return(dt)        
    }

    countries = function(){
        query = 'countries'
        url = sprintf(
            "%s/%s?format=XML",
            root,
            query
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        stri_split(regex = '\\r\\n') %>>%
        (.[[1L]][-1]) %>>%
        paste(collapse = "\n") %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        xmlApply(xmlRoot(l), function(x){
            c(
                x %>>% xmlAttrs,
                x %>>% xmlChildren %>>% list.map(. %>>% xmlValue)
            ) %>>% as.data.table
        }) %>>%
        rbindlist ->
            dt
        
        return(dt)                
    }

    incomeLevels = function(){
        query = 'incomeLevels'
        url = sprintf(
            "%s/%s?format=XML",
            root,
            query
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        stri_split(regex = '\\r\\n') %>>%
        (.[[1L]][-1]) %>>%
        paste(collapse = "\n") %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        xmlApply(xmlRoot(l), function(x){
            c(
                x %>>% xmlAttrs,
                x %>>% xmlChildren %>>% list.map(. %>>% xmlValue)
            ) %>>% as.data.table
        }) %>>%
        rbindlist ->
            dt
               
        return(dt)                
    }

    indicators = function(
        page = 1,
        per_page = 1000        
    ){
        ## Problem with XML, so use JSON
        query = 'indicators'
        
        url = sprintf(
            "%s/%s?format=JSON&per_page=%s&page=%s",
            root,
            query,
            per_page,
            page
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        fromJSON %>>%
        (? .[[1L]] -> zzz) %>>%
        (.[[2L]]) %>>%
        list.map(. %>>% unlist %>>% as.list %>>% as.data.table) %>>%
        rbindlist(fill = TRUE) ->
            dt
        
        return(structure(dt, zzz = zzz))                
    }

    indicators_all <- function(
        per_page = 1000
    ){
        x <- indicators(page = 1, per_page = per_page)
        
        pages.tot <- attr(x,'zzz')[['pages']]
        
        out <- list()
        out[[1L]] <- x
        
        for (x in 2:pages.tot){
            sprintf("Downloading page %s from %s pages in total.\n",
                    x,
                    pages.tot) %>>% cat

            sink("/dev/null")
            out[[x]] <- indicators(page = x)
            sink()
        }
        
        out %>>% rbindlist(fill = TRUE)        
    }

    indicators.observations = function(
        indicator_id = NULL,
        ## date = NULL,
        ## format = NULL,
        page = 1,
        per_page = 1000
        ## MRV = NULL,
        ## Gapfill = NULL,
        ## Frequency = NULL        
    ){
        query = 'countries/all/indicators'
        url = sprintf(
            "%s/%s/%s?format=JSON&per_page=%s&page=%s",
            root,
            query,
            indicator_id,
            per_page,
            page
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        fromJSON %>>% 
        (? .[[1L]] -> zzz) %>>%
        (.[[2L]]) %>>%
        list.map(. %>>% unlist %>>% as.list %>>% as.data.table) %>>%
        rbindlist(fill = TRUE) ->
            dt

        return(structure(dt, zzz = zzz))                          
    }

    indicators.observations_all= function(
        indicator_id = NULL,
        per_page = 1000
    ){
        x <- indicators.observations(indicator_id = indicator_id,
                                     page = 1,
                                     per_page = per_page)
        
        pages.tot <- attr(x,'zzz')[['pages']]
        
        out <- list()
        out[[1L]] <- x
        
        for (x in 2:pages.tot){
            sprintf("Downloading page %s from %s pages in total.\n",
                    x,
                    pages.tot) %>>% cat

            sink("/dev/null")
            out[[x]] <- indicators.observations(indicator_id = indicator_id,
                                                page = x,
                                                per_page = per_page)
            sink()
        }
        
        out %>>% rbindlist(fill = TRUE)                
    }

    indicators.download= function(
        indicator_ids = NULL
    ){
        indicator_ids %>>%
        list.map({
            sprintf("Downloading indicator %s...\n\n",
                    .) %>>% cat
            
            sink("/dev/null")
            . %>>% indicators.observations_all(
                per_page = 1000
            ) -> t
            sink()

            return(t)
        }) %>>%
        rbindlist(
            fill = TRUE
        )
    }    

    lendingTypes = function(){
        query = 'lendingTypes'
        url = sprintf(
            "%s/%s?format=XML",
            root,
            query
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        stri_split(regex = '\\r\\n') %>>%
        (.[[1L]][-1]) %>>%
        paste(collapse = "\n") %>>%
        xmlTreeParse(
            useInternalNodes = TRUE
        ) -> l

        xmlApply(xmlRoot(l), function(x){
            c(
                x %>>% xmlAttrs,
                x %>>% xmlChildren %>>% list.map(. %>>% xmlValue)
            ) %>>% as.data.table
        }) %>>%
        rbindlist ->
            dt
        
        return(dt)                        
    }

    topics = function(){
        ## Problem with XML, so use JSON
        query = 'topics'
        
        url = sprintf(
            "%s/%s?format=JSON",
            root,
            query
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        fromJSON %>>% 
        (? .[[1L]] -> zzz) %>>%
        (.[[2L]]) %>>%
        list.map(. %>>% unlist %>>% as.list %>>% as.data.table) %>>%
        rbindlist(fill = TRUE) ->
            dt
        
        return(structure(dt, zzz = zzz))                
    }

    datacatalog = function(
        page = NULL,
        per_page = 50
    ){
        ## Problem with XML, so use JSON
        query = 'datacatalog'
        
        url = sprintf(
            "%s/v2/%s?format=JSON&per_page=%s&page=%s",
            root,
            query,
            per_page,
            page
        )
        
        getURL(
            url = url %>>%
            URLencode
        ) %>>%
        fromJSON %>>% 
        (? .[['pages']] -> pages) %>>%
        (.[['datacatalog']]) -> x
        
        x %>>%
        list.map(metatype %>>%
                 list.group(.$id) %>>%
                 list.map(list(.[[1L]]$value)) %>>% unlist %>>%
                 as.list %>>% as.data.table) %>>%
        rbindlist(fill = TRUE) ->
            dt

        ## x[[1L]][['metatype']] %>>% list.group(.$id) %>>%
        ## list.map(list(.[[1L]]$value)) %>>% unlist %>>%
        ## as.list %>>% as.data.table

        return(structure(dt, pages = pages))                        
    }

    datacatalog_all <- function(){
        x <- datacatalog(page = 1)
        
        pages.tot <- attr(x,'pages')
        
        out <- list()
        out[[1L]] <- x
        
        for (x in 2:pages.tot){
            sprintf("Downloading page %s from %s pages in total.\n",
                    x,
                    pages.tot) %>>% cat

            sink("/dev/null")
            out[[x]] <- datacatalog(page = x)
            sink()
        }
        
        out %>>% rbindlist(fill = TRUE)        
    }
    
    o <- list(
        sources = sources,
        countries = countries,
        incomeLevels = incomeLevels,
        indicators = indicators,
        indicators_all = indicators_all,
        indicators.observations = indicators.observations,
        indicators.observations_all= indicators.observations_all,
        indicators.download = indicators.download,
        lendingTypes = lendingTypes,
        topics = topics,
        datacatalog = datacatalog,
        datacatalog_all = datacatalog_all
    )
    
    return(structure(o,
                     class = 'wbapi'))
}
