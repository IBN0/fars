#' Read CSV files on accident data
#'
#' @param filename the name of file you want to read
#' @description A Helper function to read CSV files as tibble data frame, should be used with other function.
#' If the file does not exist, it will throw out error message
#'
#' @return A tibble data frame
#' @export
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#' @seealso [make_filename()] to easily make the file name
#'
#' @examples
#'  \dontrun{dat <- fars_read("accident_2014.csv.bz2")}
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        tibble::as_tibble(data)
}

#' Acquire the filename for a given year data
#'
#' @param year A numeric value of the year you want to check.
#' @description Helper function to acquire the filename for a given year data, to be used with other function
#'
#' @return A character that gives correct filename for accident data
#' @export
#' @seealso [fars_read()] to read the file
#' [fars_read_years()] to read several years of data at once
#'
#' @examples
#' \dontrun{file <- make_filename(2014)
#' dat <- fars_read(fname)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read the CSV file of a given year(s) and add year column
#'
#' @param years Numeric (including vector) for the years you want to check
#' @description A function that compile all the years you want to look the data.
#' Currently only accept 2013-2015, if the year is not available, then it will throw up warning and return NULL
#'
#' @return A tibble data frame with extra column in year
#' @export
#' @importFrom dplyr mutate select
#' @seealso [fars_read()]
#'
#' @examples
#' \dontrun{dat <- fars_read_years(2013)}
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

#' Summarize the data grouped by years & months
#'
#' @inheritParams fars_read_years
#' @description Summary of the data from [fars_read_years()], uses tidyverse packages to process the data
#'
#' @return A summarised tibble data frame spread out by years
#' @export
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @seealso [fars_read_years()]
#'
#' @examples
#' \dontrun{dat <- fars_summarize_years(c(2013, 2014, 2015))}
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot the accident in a given state
#'
#' @param state.num A numeric value of State number you want to check
#' @inheritParams make_filename
#' @description Filter out the data based on state number and year.
#' Will throw out error if the state number is invalid
#' Will return NULL in case there is no accident data in a given state
#'
#' @return A plot of US map where the dots represent the amount of accident in a given year
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom magrittr %>%
#' @export
#' @seealso [fars_read()]
#'
#' @examples
#' \dontrun{fars_map_state(4124,  2015)}
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
        # })
})
}
