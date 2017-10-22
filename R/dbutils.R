library(DBI)
library(pool)

EC_STATUS_ERROR <- -1
EC_STATUS_GUEST <- 0
EC_STATUS_USER <- 1
EC_STATUS_ADMIN <- 2


# create table user
createTableUser <- function(db = DB_POOL) {
  tryCatch({
    # cat("in: ", "createTableUser", "\n")
    DBI::dbExecute(
      conn = db,
      "CREATE TABLE IF NOT EXISTS `User` (
      `id`        INTEGER PRIMARY KEY,
      `username`	VARCHAR(255) NOT NULL UNIQUE,
      `email`	    VARCHAR(255),
      `password`	VARCHAR(255) NOT NULL,
      `admin`	    INTEGER(1) DEFAULT 0
    );"
)
    #dbDisconnect(db)
  }, error = function(e) {
    print(e)
  })
}

##
# initialize user DB: connect and creatTable if not exists
#
initDB <- function(dbdir, dbfile) {
  # cat("in: ", "initdb", "\n")
  if (!dir.exists((dbdir))) {
    dir.create(dbdir)
  }

  pool <- pool::dbPool(drv = RSQLite::SQLite(),
                 dbname = file.path(dbdir, dbfile))
  createTableUser(pool)
  return(pool)
}

# add user with email
addUser <- function(username = "",
                    email = "",
                    password = "", db = DB_POOL) {
  # cat("in: ", "adduser", "\n")
  tryCatch({
    query <-
      paste0(
        "INSERT INTO User(username, email, password) VALUES ('",
        username,
        "', '" ,
        email,
        "', '" ,
        password,
        "')"
      )
    DBI::dbExecute(conn = db, query)
  }, error = function(e) {
    print(e)
  })
}

# createNewUserFromList
addUserFromList <- function(values, db = DB_POOL) {
  addUser(db,
          username = values["username"],
          email = values["email"],
          password = values["password"])
}


# delete user
deleteUserByUsername <- function(username = "", db = DB_POOL) {
  tryCatch({
    DBI::dbExecute(conn = db,
              paste0("DELETE FROM User where username='", username, "'"))
  }, error = function(e) {
    print(e)
  })
}


# delete user by ID
deleteUserByID <- function(id, db = DB_POOL) {
  tryCatch({
    DBI::dbExecute(conn = db,
              paste0("DELETE FROM User where id= ", id, " ;"))

  }, error = function(e) {
    print(e)
  })
}


# change user information
updateUser <- function(username = "",
                       email = "",
                       password = "",
                       db = DB_POOL) {
  cat("in: ", "updateuser", "\n")
  tryCatch({
    q <-
      paste0(
        "UPDATE User SET `email` = '",
        email   ,
        "' , `password` = '",
        password,
        "'  WHERE `username` = '",
        username,
        "';"
      )
    DBI::dbExecute(conn = db, q)
  }, error = function(e) {
    print(e)
  })
}


# update user from list
updateUserFromListByID <- function(values, db = DB_POOL) {
  #cat("data to update*:  ", "email: ", values["email"], " password: ", values["password"], " username: ", values["username"],"\n")
  q <- paste0(
    "UPDATE User SET `email` = '",
    values["email"],
    "' , `password` = '",
    values["password"],
    "' , `username` = '",
    values["username"],
    "'  WHERE `id` = ",
    values["id"],
    ";"
  )
  DBI::dbExecute(conn = db, q)
}



# read user
getUser <- function(username = "", db = DB_POOL) {
  # cat("in: ", "readuser", "\n")
  query <-
    paste0("SELECT * FROM User WHERE `username`  = '", username, "'")
  DBI::dbGetQuery(conn = db, query)
}

# get userById
getUserById <- function(id, db = DB_POOL) {
  # cat("in: ", "geruserbyid", "\n")
  df <-
    DBI::dbGetQuery(db, paste0("select * from User where id = " , id))
  return(df)
}

# get user id from username
getUserId <- function(username, db = DB_POOL) {
  # cat("in: ", "getuserid", "\n")
  user.id <-
    DBI::dbGetQuery(db,
               paste0(
                 "select id, username, email from User where username = " ,
                 " '",
                 username,
                 "' ; "
               ))
  return(user.id)
}

# get all users
getUsers <- function(db = DB_POOL) {
  # cat("in: ", "getUsers", "\n")
  df <- DBI::dbGetQuery(db, 'select id, username, email from User')
  return(df)
}

userExists <- function(username = "", db = DB_POOL) {
  # cat("in: ", "userexists", "\n")
  user <- getUser(db, username = username)
  if (is.na(user$username[1]))
    return(FALSE)
  return(TRUE)
}

# check username/password match
credentialsMatch <- function(username = "",
                             password = '',
                             db = DB_POOL) {
  # cat("in: ", "credentialsmatch", "\n")
  a <- getUser(db, username = username)

  if (is.na(a$username[1])) {
    # User does not exist
    return(EC_STATUS_GUEST)
  }

  # Check credentials
  if (username == a$username[1] & password == a$password[1]) {
    if (a$admin == 1) {
      # User is admin
      return(EC_STATUS_ADMIN)
    } else {
      # Normal user
      return(EC_STATUS_USER)
    }
  } else {
    # Wrong password
    return(EC_STATUS_ERROR)
  }

}

createDBPool <- function(envir = .GlobalEnv) {
  ##
  ## Create database pool
  ##
  if (!exists("DB_DIR")) {
    stop("Global variable DB_DIR with path to the database repository is required")
  }
  if (!exists("DB_FILE")) {
    stop("Global variable DB_FILE with database file name is required")
  }
  dbdir <- get("DB_DIR", envir = .GlobalEnv)
  dbfile <- get("DB_FILE", envir = .GlobalEnv)

  assign("DB_POOL", initDB(dbdir, dbfile), envir)
}

