
tox21config <- config::get("tox21enricher")
tox21queue <- config::get("tox21enricher-queue")

# Define database connection information
CONN_INFO <- list(
    main=list(
        name=tox21config$database,
        host=tox21config$host,
        user=tox21config$uid,
        pass=tox21config$pwd,
        port=tox21config$port
    ),
    queue=list(
        name=tox21queue$database,
        host=tox21queue$host,
        user=tox21queue$uid,
        pass=tox21queue$pwd,
        port=tox21queue$port
    )
)

# Define database driver
pgdrv <- RPostgres::Postgres()

pool_main <- dbPool(
    drv=pgdrv, 
    dbname=CONN_INFO[["main"]]$name,
    host=CONN_INFO[["main"]]$host,
    port=CONN_INFO[["main"]]$port, 
    user=CONN_INFO[["main"]]$user,
    password=CONN_INFO[["main"]]$pass
)
pool_queue <- dbPool(
    drv=pgdrv, 
    dbname=CONN_INFO[["queue"]]$name,
    host=CONN_INFO[["queue"]]$host,
    port=CONN_INFO[["queue"]]$port, 
    user=CONN_INFO[["queue"]]$user,
    password=CONN_INFO[["queue"]]$pass
)

# Run database query on a given database
run_query <- function(db, query, args=list()){
    
    # print(query)
    
    pool <- pool_main
    if(db == "queue"){
        pool <- pool_queue
    }
    
    df <- data.frame()
    try_query <- tryCatch({ # Attempt to query db
        # Checkout connection from db pool
        conn <- localCheckout(pool)
        
        # Don't do dbFetch if not returning data
        if(grepl("^SELECT", query)){
            res <- dbSendQuery(conn, query)
            if(length(args) > 0){
                dbBind(res, args)
            }
            df <- dbFetch(res)
            dbClearResult(res)
        } else {
            res <- dbSendStatement(conn, query)
            if(length(args) > 0){
                dbBind(res, args)
            }
            dbClearResult(res)
        }
    }, error=function(cond){
        print("Error in query")
        print(cond)
    })
    
    resp <- data.frame()
    if(nrow(df) > 0){
        resp <- df
    }
    return(resp)
}