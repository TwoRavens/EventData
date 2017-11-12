##
##  rookeventdata.r
##

# More general local setup instructions are available in the rooksubset_local file.

eventdata_subset.app <- function(env) {
    production = FALSE     ## Toggle:  TRUE - Production, FALSE - Local Development

    api_key = 'api_key=CD75737EF4CAC292EE17B85AAE4B6'
    datasource = "api"

    server_address = 'http://149.165.168.79:5002/api/data?'
    local_server_address = 'http://0.0.0.0:5002/api/data?'

    if (production) {
        sink(file = stderr(), type = "output")
    }

    request <- Request$new(env)
    response <- Response$new()
    response$header("Access-Control-Allow-Origin", "*")  # Enable CORS

    print("Request received")

    if (request$options()) {
        print("Preflight")
        response$status = 200L

        # Ensures CORS header is permitted
        response$header("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
        response$header("Access-Control-Allow-Headers", "origin, content-type, accept")
        return(response$finish())
    }

    # Ensure solaJSON is posted
    solajson = request$POST()$solaJSON
    if (is.null(solajson)) {
        response$write('{"warning": "EventData R App is loaded, but no json sent. Please send solaJSON in the POST body."}')
        return(response$finish())
    }

    # Ensure that solaJSON is valid
    valid <- jsonlite::validate(solajson)
    if (!valid) {
        response$write('{"warning": "The request is not valid json. Check for special characters."}')
        return(response$finish())
    }

    everything <- jsonlite::fromJSON(request$POST()$solaJSON, simplifyDataFrame = FALSE)

    # raw: return query as is
    # summary: return metadata
    # actor: return actor filtering
    # formatted: check if database is properly formatted
    # validate: check if query is valid
    type = everything$type

    dataset = everything$dataset
    subsets = everything$subsets
    variables = everything$variables

    if (is.null(everything$dataset)) {
        response$write(paste('{"error": "no dataset is specified"}'))
        return(response$finish())
    }

    if (!production && datasource == "local") {
        server_address = local_server_address
    }
    eventdata_url = paste(server_address, api_key, sep="")

    if (!production && !is.null(everything$datasource)) {
        datasource = everything$datasource
    }
    format = paste(dataset, '_', datasource, sep="")


    # ~~~~ Data Retrieval ~~~~

    getData = function(url) {
        data = jsonlite::fromJSON(relabel(url, format))$data
        print(data)
        return(data)
    }

    if (!is.null(type) && type == 'formatted') {
        warnings = validate(getData(paste(eventdata_url, '&aggregate=[{"$limit":100}]', sep="")), format)
        response$write(paste('{"warning": "', toString(jsonlite::toJSON(warnings)), '"}'))
        return(response$finish())
    }

    if (!is.null(type) && type == 'raw') {
        result = getData(paste(eventdata_url, '&query=', subsets, sep=""))
        response$write(result)
        return(response$finish())
    }

    if (!is.null(type) && type == 'validate') {

        handler = function(exc) {
            cleanedExc = gsub('\\\n', '', gsub('"', "'", toString(exc)))
            print(paste('{"response": "The custom query is malformed:', cleanedExc, '"}'))

            response$write(paste('{"response": "The custom query is malformed:', cleanedExc, '"}'))
        }

        tryCatch({
            getData(paste(eventdata_url, '&query=', subsets, sep=""))
            response$write('{"response": "Query is valid."}')},
        warning = handler,
        error = handler
        )
        return(response$finish())
    }

    # Arguments specific to sources/targets queries
    if (!is.null(type) && type == 'source') {
        unique_vals = sort(getData(paste(eventdata_url, '&unique=<source>&query=', subsets, sep="")))
        response$write(toString(jsonlite::toJSON(list(source = unique_vals))))
        return(response$finish())
    }

    if (!is.null(type) && type == 'target') {
        unique_vals = sort(getData(paste(eventdata_url, '&unique=<target>&query=', subsets, sep="")))
        response$write(toString(jsonlite::toJSON(list(target = unique_vals))))
        return(response$finish())
    }

    # Check if metadata has already been computed, and return cached value if it has
    # if (!file.exists("./data/cachedQueries.RData")) save(list(0), file="./data/cachedQueries.RData")
    #
    # cachedQueries = load("./data/cachedQueries.RData")
    # if (subsets %in% cachedQueries) {
    #     response$write(getData(paste("./data/", match(subsets, cachedQueries)), ".txt"))$data
    #     return(response$finish())
    # }

    # Useful for the target and source other fields, to unwrap the semicolon-delimited values
    uniques = function(values) {
        accumulator = list()
        for (key in values) {
            if (key != "") {
                accumulator = c(accumulator, strsplit(key, ';'))
            }
        }
        return(sort(unlist(unique(do.call(c, accumulator)))))
    }

    if (production) {
        sink()
    }

    if (dataset == "pheonix") {

        # This is a new query, so compute new metadata
        query_url = paste(eventdata_url, '&query=', subsets, sep="")

        print("Collecting date frequencies")
        date_frequencies = getData(paste(query_url, '&group=<year>,<month>', sep=""))

        print("Collecting country frequencies")
        country_frequencies = getData(paste(query_url, '&group=<country_code>', sep=""))

        print("Collecting action codes")
        action_frequencies = getData(paste(query_url, '&group=<root_code>', sep=""))

        print("Collecting actor sources")
        actor_source = getData(paste(query_url, '&unique=<source>', sep=""))

        print("Collecting actor source entities")
        actor_source_entities = getData(paste(query_url, '&unique=<src_actor>', sep=""))

        print("Collecting actor source agents/roles")
        actor_source_role = getData(paste(query_url, '&unique=<src_agent>', sep=""))

        print("Collecting actor source other agents")
        url = relabel(paste(query_url, '&unique=<src_other_agent>', sep=""), format)
        actor_source_attributes = uniques(jsonlite::fromJSON(url)$data)

        actor_source_values = list(
            full = actor_source,
            entities = actor_source_entities,
            roles = actor_source_role,
            attributes = actor_source_attributes
        )

        print("Collecting actor targets")
        actor_target = getData(paste(query_url, '&unique=<target>', sep=""))

        print("Collecting actor target entities")
        actor_target_entities = getData(paste(query_url, '&unique=<tgt_actor>', sep=""))

        print("Collecting actor target agents/roles")
        actor_target_role = getData(paste(query_url, '&unique=<tgt_agent>', sep=""))

        print("Collecting actor target other agents")
        url = relabel(paste(query_url, '&unique=<tgt_other_agent>', sep=""), format)
        actor_target_attributes = uniques(jsonlite::fromJSON(url)$data)

        actor_target_values = list(
            full = actor_target,
            entities = actor_target_entities,
            roles = actor_target_role,
            attributes = actor_target_attributes
        )

        # Package actor data
        actor_values = list(
            source = actor_source_values,
            target = actor_target_values
        )

        result = toString(jsonlite::toJSON(list(
            date_data = date_frequencies,
            country_data = country_frequencies,
            action_data = action_frequencies,
            actor_data = actor_values
        )))

        print("Complete!")
        response$write(result)
        return(response$finish())
    }

    if (dataset == "icews") {

        # This is a new query, so compute new metadata
        query_url = paste(eventdata_url, '&query=', subsets, sep="")

        print("Collecting date frequencies")
        date_frequencies = getData(paste(eventdata_url, '&aggregate=', '[{$match:', subsets, '},',
            '{"$project": {"Year":  {"$substr": ["$<date>", 0, 4]},',
                          '"Month": {"$substr": ["$<date>", 5, 2]}}},',                           # Construct year and month fields
            '{$group: { _id: { year: "$Year", month: "$Month" }, total: {$sum: 1} }}]', sep=""))  # Group by years and months

        print("Collecting country frequencies")
        country_frequencies = getData(paste(query_url, 'group=<country>', sep=""))

        print("Collecting cameo codes")
        action_frequencies = getData(paste(query_url, '&group=<cameo>', sep=""))

        print("Collecting actor source countries")
        actor_source_country = getData(paste(query_url, '&unique=<src_country>', sep=""))

        actor_source_values = list(
            countries = actor_source_country
        )

        print("Collecting actor target countries")
        actor_target_countries = getData(paste(query_url, '&unique=<tgt_countries>', sep=""))

        actor_target_values = list(
            countries = actor_target_countries
        )

        # Package actor data
        actor_values = list(
        source = actor_source_values,
        target = actor_target_values
        )

        result = toString(jsonlite::toJSON(list(
            date_data = date_frequencies,
            country_data = country_frequencies,
            action_data = action_frequencies,
            actor_data = actor_values
        )))

        print("Complete!")
        response$write(result)
        return(response$finish())
    }

}
