#' Open the FAME host language interface.
#'
#' @export
open_hli <- function() {
  status <- rhli::Integer(-1)
  ver <- rhli::Numeric(-1)
  rhli::cfmini(status)
  print_stack()
  return (status$value == rhli::HSUCC)
}

#' Close the FAME host language interface.
#'
#' @export
close_hli <- function() {
  status <- rhli::Integer(-1)
  rhli::cfmfin(status)
  return (status$value == rhli::HSUCC)
}

#' convert a fame type code to a string
#'
#' @param type_code_
#'
#' @return type string
type_to_string <- function(type_code_) {
  type_code <- rhli::Integer(type_code_)
  inlen <- rhli::Integer(80)
  outlen <- rhli::Integer(-1)
  type_string <- rhli::Character(sprintf("%*s", inlen$value, ""))
  
  rhli::fame_type_to_string(type_code, type_string, inlen, outlen)
  
  is_date_data <- type_code$value >= rhli::HDATE
  
  if (is_date_data) {
    return(sprintf("DATE(%s)", type_string$value))
  }
  return(type_string$value)
}

new.hashtable <- function() {
  e <- new.env()
  list(
    set = function(key, value)
      assign(as.character(key), value, e),
    get = function(key)
      get(as.character(key), e),
    rm = function(key)
      rm(as.character(key), e)
  )
}

#ht <- new.hashtable()
#ht$set(245, 3)
#ht$get(245)

FAME_CLASS <- new.hashtable()
FAME_CLASS$set(rhli::HSERIE, "SERIES")
FAME_CLASS$set(rhli::HSCALA, "SCALAR")
FAME_CLASS$set(rhli::HFRMLA, "FORMULA")
FAME_CLASS$set(rhli::HGLNAM, "GLNAME")
FAME_CLASS$set(rhli::HGLFOR, "GLFORMULA")

#'
#' Print a catalog
#'
#' @param famedata List containing FAME data to display
#' @param list.len maximum entries to display 
#'
#' @export
print_catalog <- function(famedata, list.len = -1) {
  if (list.len < 1) {
    list.len = length(famedata$contents)
  }
  for (i in 1:length(famedata$contents)) {
    if (i > list.len) {
      cat(sprintf(
        "\n[catalog output truncated, %d more entries not displayed because list.len=%d]\n",
        length(famedata$contents)-list.len,list.len
      ))
      break
      
    }
    objnam <- names(famedata$contents)[i]
    cat(meta_to_string(famedata$get(objnam)$meta, objnam))
  }
}

#' Construct a date range
#'
#' @param freq FAME HLI frequency constant
#' @param start_str start date literal
#' @param end_str  end date literal
#'
#' @return date range
#' @export
to_fame_range <- function(freq_, start_str_, end_str_) {
  range <- c(freq_, 0, 0)
  freq <- rhli::Integer(freq_)
  start_str <- rhli::Character(start_str_)
  end_str <- rhli::Character(end_str_)
  date <- rhli::Integer(-1)
  end_month <- rhli::Integer(rhli::HDEC)
  label_year <- rhli::Integer(rhli::HFYAUT)
  century <- rhli::Integer(2000)
  rhli::fame_literal_to_date(freq, date, start_str, end_month, label_year, century)
  range[2] <- date$value
  rhli::fame_literal_to_date(freq, date, end_str, end_month, label_year, century)
  range[3] <- date$value
  return (range)
}


#' get a meta data string for an object
#'
#' @param famedata a list containing fame database objects
#' @param objnam object name
#'
#' @return string containing meta data
#' @export
meta_to_string <- function(fameinfo, objnam) {
  if (!is.null(fameinfo)) {
    fametype <- fameinfo$type
    if (!is.null(fametype)) {
      fame_type_string <- type_to_string(fametype)
      fameclass <- fameinfo$class
      if (fameclass == rhli::HSCALA) {
        meta <- sprintf("SCALAR %s : %s", objnam, fame_type_string)
      }
      else if (fameclass == rhli::HSERIE) {
        rng <- fameinfo$range
        if (!is.null(rng)) {
          freq_code <- rng[1]
          if (freq_code == rhli::HCASEX) {
            rng_string <- sprintf("%d to %d", rng[2], rng[3])
          }
          else{
            inlen <- rhli::Integer(1024)
            start_str <-
              rhli::Character(sprintf("%*s", inlen$value, ""))
            end_str <-
              rhli::Character(sprintf("%*s", inlen$value, ""))
            outlen <- rhli::Integer(-1)
            rhli::fame_date_to_literal(
              rhli::Integer(freq_code),
              rhli::Integer(rng[2]),
              start_str,
              rhli::Integer(rhli::HDEC),
              rhli::Integer(rhli::HFYAUT),
              inlen,
              outlen
            )
            rhli::fame_date_to_literal(
              rhli::Integer(freq_code),
              rhli::Integer(rng[3]),
              end_str,
              rhli::Integer(rhli::HDEC),
              rhli::Integer(rhli::HFYAUT),
              inlen,
              outlen
            )
            rng_string <-
              sprintf("%s to %s", start_str$value, end_str$value)
          }
          if (freq_code == rhli::HCASEX) {
            fame_index_string <- "CASE"
          }
          else{
            fame_index_string <- type_to_string(freq_code)
          }
          meta <- sprintf(
            "SERIES %s : %s BY %s %s",
            objnam,
            fame_type_string,
            fame_index_string,
            rng_string
          )
        }
      }
    }
  }
  famedesc <- fameinfo$desc
  if (!is.null(famedesc)) {
    meta <- sprintf("%s\n%s", meta, famedesc)
  }
  famedocu <- fameinfo$docu
  if (!is.null(famedocu)) {
    meta <- sprintf("%s\n-\n%s\n\n", meta, famedocu)
  }
  else{
    meta <- sprintf("%s\n\n", meta)
  }
  return(meta)
}

#' Display version information
#'
print_stack <- function() {
  status <- rhli::Integer(-1)
  ver <- rhli::Numeric(-1)
  rhli::cfmver(status, ver)
  
  if (status$value == rhli::HSUCC) {
    cat(
      sprintf(
        paste(
          "\n%s\n\n%s\n%s\n%s\n\n",
          "lubridate        %s\nqoma.smuggler    %s\nrhli             %s\n",
          "tibble           %s\n",
          "FAME HLI        %.5f\n\n",
          sep = ""
        ),
        system2("uname", args = "-s -o -r -v", stdout = TRUE),
        R.Version()$version.string,
        R.Version()$nickname,
        R.Version()$platform,
        packageVersion("lubridate"),
        packageVersion("qoma.smuggler"),
        packageVersion("rhli"),
        packageVersion("tibble"),
        ver$value
      )
    )
  }
  return (status$value == rhli::HSUCC)
}

#' Read a FAME database into an R list
#'
#' @export
read_fame <- function(dbname_,
                      wilnam_ = "?",
                      fame_range_ = NULL) {
  status <- rhli::Integer(-1)
  dbkey <- rhli::Integer(-1)
  dbname <- rhli::Character(dbname_)
  mode <- rhli::Integer(rhli::HRMODE)
  rhli::cfmopdb(status, dbkey, dbname, mode)
  if (status$value != rhli::HSUCC)
    cat(sprintf("cfmopdb %d\n", status$value))
  
  wilnam <- rhli::Character(wilnam_)
  rhli::cfminwc(status, dbkey, wilnam)
  if (status$value != rhli::HSUCC)
    cat(sprintf("cfminwc %d\n", status$value))
  
  class <- rhli::Integer(-1)
  type <- rhli::Integer(-1)
  freq <- rhli::Integer(-1)
  
  database = list()
  outlen = rhli::Integer(-1)
  while (status$value == rhli::HSUCC) {
    objnam <- rhli::Character(sprintf("%*s", 80, ""))
    rhli::cfmnxwc(status, dbkey, objnam, class, type, freq)
    if (status$value != rhli::HSUCC &&
        status$value != rhli::HNOOBJ)
      cat(sprintf("cfmnxwc %d\n", status$value))
    if (status$value == rhli::HSUCC) {
      object_dict = list()
      meta_dict = list()
      
      findex <- rhli::Integer(-1)
      lindex <- rhli::Integer(-1)
      basis  <- rhli::Integer(-1)
      observ <- rhli::Integer(-1)
      cdate  <- rhli::Integer(-1)
      mdate  <- rhli::Integer(-1)
      outdesclen <- rhli::Integer(80)
      desc <-
        rhli::Character(sprintf("%*s", outdesclen$value, ""))
      outdoclen <- rhli::Integer(800)
      doc <-
        rhli::Character(sprintf("%*s", outdoclen$value, ""))
      rc <- rhli::fame_info(
        dbkey,
        objnam,
        class,
        type,
        freq,
        findex,
        lindex,
        basis,
        observ,
        cdate,
        mdate,
        desc,
        outdesclen,
        outdesclen,
        doc,
        outdoclen,
        outdoclen
      )
      if (rc != rhli::HSUCC) {
        cat(sprintf("fame_info %d\n", rc))
        cat(sprintf("[%s]\n\n", okstr))
        return (FALSE)
      }
      
      # trim to minimum length / may persist for a while
      if (nchar(desc$value) > 0) {
        meta_dict[['desc']] <-
          sprintf("%*s", nchar(desc$value), desc$value)
      }
      if (nchar(doc$value) > 0) {
        meta_dict[['docu']] <-
          sprintf("%*s", nchar(doc$value), doc$value)
      }
      
      if (class$value == rhli::HSERIE) {
        if (is.null(fame_range_)) {
          rng <- c(freq$value, findex$value, lindex$value)
        }
        else{
          rng <- fame_range_
        }
        if (freq$value != rng[1]) {
          # frequency mismatch, disregard object
          next
        }
      }
      else {
        # HSCALA
        if (!is.null(fame_range_)) {
          next  # fame_range doesnt match scalar
        }
        rng <- c(0, 0, 0)
      }
      numobs <- rng[3] - rng[2] + 1
      
      if (type$value == rhli::HPRECN) {
        valary <- rhli::Numeric(rep(-1, numobs))
        tmiss <- rhli::Integer(rhli::HNTMIS)
        mistt <- rhli::Numeric(rep(-1, 3))
        rhli::cfmrrng_double(status,
                             dbkey,
                             objnam,
                             rhli::Integer(rng),
                             valary,
                             tmiss,
                             mistt)
      }
      else if (type$value == rhli::HNUMRC) {
        valary <- rhli::Numeric(rep(-1, numobs))
        tmiss <- rhli::Integer(rhli::HNTMIS)
        mistt <- rhli::Numeric(rep(-1, 3))
        rhli::cfmrrng_float(status,
                            dbkey,
                            objnam,
                            rhli::Integer(rng),
                            valary,
                            tmiss,
                            mistt)
      }
      else if (type$value == rhli::HSTRNG) {
        lenary <- rhli::Integer(rep(-1, numobs))
        rhli::fame_len_strings(dbkey, objnam, rhli::Integer(rng), lenary)
        valary <-
          rhli::Character(sprintf("%*s", lenary$value, ""))
        misary <- rhli::Integer(rep(-1, numobs))
        rhli::fame_get_strings(dbkey, objnam, rhli::Integer(rng), valary, lenary, NULL)
      }
      else{
        status <- rhli::Integer(-1)
        inlen  <- rhli::Integer(10)
        outlen  <- rhli::Integer(-1)
        valary <- rhli::Character(sprintf("%*s", 10, ""))
        rhli::cfmgtnl(status,
                      dbkey,
                      objnam,
                      rhli::Integer(rhli::HNLALL),
                      valary,
                      inlen,
                      outlen)
        if (status$value == rhli::HTRUNC) {
          # allocate more space, try again
          inlen <- rhli::Integer(outlen$value)
          valary <-
            rhli::Character(sprintf("%*s", inlen$value, ""))
          rhli::cfmgtnl(status,
                        dbkey,
                        objnam,
                        rhli::Integer(rhli::HNLALL),
                        valary,
                        inlen,
                        outlen)
        }
      }
      object_dict[['data']] <- valary$value
      if (class$value == rhli::HSERIE) {
        meta_dict[['range']] <- rng
      }
      meta_dict[['class']] <- class$value
      meta_dict[['type']] <- type$value
      if (basis$value != rhli::HOBUND) {
        meta_dict[['basis']] <- basis$value
      }
      if (observ$value != rhli::HUNDFX) {
        meta_dict[['observ']] <- observ$value
      }
      meta_dict[['cdate']] <- cdate$value
      meta_dict[['mdate']] <- mdate$value
      object_dict[['meta']] <- meta_dict
      database[[objnam$value]] <- object_dict
    }
  }
  
  
  rhli::cfmcldb(status, dbkey)
  if (status$value != rhli::HSUCC)
    cat(sprintf("cfmcldb %d\n", status$value))
  cat(sprintf(
    "read_fame() returns %d objects from %s\n",
    length(database),
    dbname_
  ))
  return (List(database))
}

#' create a lubridate index
#'
#' @param fame_range FAME range
#'
#' @return tibble with lubridate date column
#' @export
to_lubridate_index <- function(rng) {
  numobs <- rng[3] - rng[2] + 1
  index <- lubridate::as_date(1:numobs)
  for (i in 0:(numobs - 1)) {
    index[i + 1] <- to_lubridate_date(rng[1], rng[2] + i)
  }
  return(tibble::tibble(date = index))
}

#' create a lubridate date
#'
#' @param fame_freq FAME frequency code
#' @param date FAME date
#'
#' @return lubridate date
#' @export
to_lubridate_date <- function(fame_freq, date) {
  base_date <- rhli::Integer(43830) # FAME DAILY frequency 1/1/1970
  daily_date <- rhli::Integer(-1)
  rc <-
    rhli::fame_dateof(
      rhli::Integer(fame_freq),
      rhli::Integer(date),
      rhli::Integer(rhli::HEND),
      rhli::Integer(rhli::HDAILY),
      daily_date,
      rhli::Integer(rhli::HCONT)
    )
  return(lubridate::as_date(daily_date$value - base_date$value))
}


#' put data for FAME in a list
#'
#' @param objectname object name
#' @param data data value(s) to store
#' @param desc description
#' @param docu documentation
#' @param otype object type attribute
#' @param basis object basis attribute
#' @param obse object observed attribute
#'
#' @return success TRUE or FALSE
#' @export
put <- function(mylist,
                objectname,
                data,
                desc = NULL,
                docu = NULL,
                class = rhli::HSERIE,
                range = NULL,
                type = rhli::HPRECN,
                basis = rhli::HBSBUS,
                obse = rhli::HOBEND) {
  # FAME meta data -
  fameinfo <- list()
  if (!is.null(desc)) {
    fameinfo['desc'] <- desc
  }
  if (!is.null(docu)) {
    fameinfo['docu'] <- docu
  }
  if (class == rhli::HSERIE) {
    fameinfo['class'] <- rhli::HSERIE
    if (!is.null(range)) {
      fameinfo[['range']] <- range
      fameinfo['basis'] <- basis
      fameinfo['observ'] <- obse
    }
    else if (class == rhli::HSERIE) {
      fameinfo['range'] <- c(rhli::HCASEX, 1, length(data))
    }
  }
  else {
    fameinfo['class'] <- rhli::HSCALA
  }
  fameinfo['type'] <- type
  
  
  
  entry = list()
  entry[['data']] <- data
  entry[['meta']] <- fameinfo
  
  famedata <- list()
  famedata[[objectname]] <- entry
  
  mylist$contents <- c(mylist$contents, famedata)
  
}



#' Mutable list
#'
#' @export List
List <- setRefClass(
  "List",
  fields = list(contents = "list"),
  methods = list(
    initialize = function(l0 = list()) {
      "Initialize a list."
      contents <<- as.list(l0)
    },
    put = function(key,value){
      contents[[key]] <<- value
    },
    get = function(objnam = NULL) {
      "Get an element of the list"
      if (is.null(objnam)) {
        return(NULL)
      }
      return(contents[[objnam]])
    },
    get_meta = function(objnam = NULL) {
      "Get meta data"
      if (is.null(objnam)) {
        return(NULL)
      }
      entry <- get(objnam)
      if (is.null(entry)) {
        return(NULL)
      }
      famemeta <- entry$meta
      if (is.null(famemeta)) {
        return(NULL)
      }
      return(meta_to_string(famemeta, objnam))
    },
    get_data = function(objnam = NULL) {
      "Get data"
      if (is.null(objnam)) {
        return(NULL)
      }
      entry <- get(objnam)
      if (is.null(entry)) {
        return(NULL)
      }
      data <- entry$data
      if (is.null(data)) {
        return(NULL)
      }
      famemeta <- entry$meta
      if (is.null(famemeta)) {
        return(NULL)
      }
      fameclass <- famemeta$class
      if (is.null(fameclass)) {
        return(NULL)
      }
      if (fameclass == rhli::HSERIE) {
        range <- famemeta$range
        if (is.null(range)) {
          return(NULL)
        }
        tbl <- to_lubridate_index(range)
        tbl[[objnam]] <- data
        return(tbl)
      }
      return(data)
    }
  )
)


#' wrte fame db
#'
#' @param dbname db name
#' @param container List with data to write
#'
#' @export
write_fame <- function(dbname_, container) {
  status <- rhli::Integer(-1)
  dbkey <- rhli::Integer(-1)
  dbname <- rhli::Character(dbname_)
  rhli::cfmopdb(status, dbkey, dbname, rhli::Integer(rhli::HUMODE))
  if (status$value == rhli::HRNEXI) {
    rhli::cfmopdb(status, dbkey, dbname, rhli::Integer(rhli::HCMODE))
  }
  for (i in 1:length(container$contents)) {
    objnam <- rhli::Character(names(container$contents)[i])
    entry <- container$get(objnam$value)
    data <- entry$data
    meta <- entry$meta
    class <- rhli::Integer(meta$class)
    if (class$value == rhli::HSERIE) {
      range <- rhli::Integer(meta$range)
      freq <- rhli::Integer(range$value[1])
    }
    else{
      range <- rhli::Integer(c(0, 0, 0))
      freq <- rhli::Integer(rhli::HUNDFX)
    }
    type <- rhli::Integer(meta$type)
    
    basis <- meta$basis
    if (is.null(basis)) {
      basis <- rhli::HUNDFX
    }
    basis <- rhli::Integer(basis)
    
    observ <- meta$observ
    if (is.null(observ)) {
      observ <- rhli::HOBUND
    }
    observ <- rhli::Integer(observ)
    
    numobs <- rhli::Integer(range$value[3] - range$value[2] + 1)
    numchr <- 0
    if (type$value == rhli::HSTRNG) {
      numchr <- sum(nchar(data))
    }
    else if (type$value == rhli::HNAMEL) {
      namelist <- strsplit(data, "[{,}]")[[1]]
      namelist <- namelist[nchar(namelist) > 0]
      numobs <- rhli::Integer(length(namelist))
      numchr <- sum(nchar(namelist))
    }
    numchr <- rhli::Integer(numchr)
    growth <- rhli::Numeric(0)
    
    rhli::cfmalob(status,
                  dbkey,
                  objnam,
                  class,
                  freq,
                  type,
                  basis,
                  observ,
                  numobs,
                  numchr,
                  growth)
    
    famedesc <- meta$desc
    if (!is.null(famedesc)) {
      rhli::cfmsdes(status, dbkey, objnam, rhli::Character(famedesc))
    }
    famedocu <- meta$docu
    if (!is.null(famedocu)) {
      rhli::cfmsdoc(status, dbkey, objnam, rhli::Character(famedocu))
    }
    
    if (type$value == rhli::HPRECN) {
      rhli::cfmwrng_double(
        status,
        dbkey,
        objnam,
        range,
        rhli::Numeric(data),
        rhli::Integer(rhli::HNTMIS),
        rhli::Numeric(c(0, 0, 0))
      )
    }
    else if (type$value == rhli::HNUMRC) {
      rhli::cfmwrng_float(
        status,
        dbkey,
        objnam,
        range,
        rhli::Numeric(data),
        rhli::Integer(rhli::HNTMIS),
        rhli::Numeric(c(0, 0, 0))
      )
    }
    else if (type$value == rhli::HSTRNG) {
      status$value <- rhli::fame_write_strings(dbkey, objnam, range,
                                               rhli::Character(data))
    }
    else {
      for (i in 1:numobs$value) {
        rhli::cfmwtnl(status,
                      dbkey,
                      objnam,
                      rhli::Integer(i),
                      rhli::Character(namelist[i]))
      }
    }
  }
  rhli::cfmcldb(status, dbkey)
  cat(sprintf(
    "write_fame() stored %d objects in %s\n",
    length(container$contents),
    dbname_
  ))
  return(TRUE)
  
}
