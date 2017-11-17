#' Read in Fatality Analysis Reporting System data
#'
#' This function reads in the .csv file containing the data. It will check if
#' the file exists in the current directory so set it correctly. Lastly, it
#' will wrap the dataframe with tibble format.
#'
#' @param filename A character string gives the location to the .csv file or
#' .csv.bz2 file
#' @return This function returns a dataframe. As a side effect, it will also
#' wrap the tibble format around the dataframe.
#' \ @examples
#' \dontrun{fars_read("./data/accident_2013.csv.bz2")}
#' @details This function will use the read_csv from readr package and tbl_df
#' from dplyr package.
#' @seealso \code{read_csv}
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create Fatality Analysis Reporting System filename
#'
#' @return This function returns a length 1 character string specifies the name
#' of the data from the input year. The name is of the form accident_year.csv.bz2
#' @param year a numeric or an integer or a number string with four digits.
#' This specifies year the data was recorded.
#' @examples
#' \dontrun{make_filename(2014)}
#' @seealso \code{sprintf}
#' @export
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Return list of month and year dataframes for Fatality Analysis Reporting System
#'
#' This function takes a vector of years, create the file name using
#' \code{make_filename} function and then read in the data using \code{fars_read}
#' It uses lapply to run the loop through all the years. A warning will print
#' if there exists an invalid year.
#'
#' @return a list of dataframe where each dataframe contains month and year
#' for valid year and NULL for invalid year.
#' @param years a vector of years in integer, numeric or number character form
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#' @seealso \code{\link{make_filename}} , \code{\link{fars_read}}
#' \code{lapply} , \code{tryCatch}
#'
#' @importFrom dplyr mutate select
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Return monthly total accidents for given years
#'
#' This function takes a vectors of year and feeds it into the \code{\link{fars_read_years}}
#' function. It then uses dplyr and tidyr packages to group total number of accident
#' by months and spread it by the years.
#'
#' @return a tibble summarizes total accidents per month for given years
#' @param years a vector of years
#' @examples
#' \dontrun{fars_summarize_years(c(2013, 2014, 2015))}
#' @seealso \code{\link{fars_read_years}}
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Return a map plot of accidents by State and year
#'
#' This function takes a state number ID and a year and output a map plot shows all
#' the accidents in that year.
#'
#' @return a map plot of all accidents in the state in that year.
#' @param state.num The state number
#' @param year The valid year of recorded data
#' @examples
#' \dontrun{fars_map_state(50, 2014)}
#' @seealso \code{\link{make_filename}} , \code{\link{fars_read}}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
