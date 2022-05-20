# Postcode ----
#' Postcode table
#'
#' @description Postcode information imported to SQL db from central public
#' source.
#' https://cdu-data-science-team.github.io/team-blog/posts/2021-05-14-index-of-multiple-deprivation/
#' Links to \code{\link{tidy_postcodes}}
#'
#' @param select_vars Logical, specifying whether to include only selected
#' variables (TRUE) or all variables from the database (FALSE).
#'
#' @param conn A database connection - stored in the database1 database but is
#' generic to all data
#' @param return String, specifying whether to return a "tbl_sql"
#' connection or "tbl_df"
#' @param local_area String, the postcode table covers the whole of England
#' and has 2 million plus observations. Reducing the output to
#' LocalAuthorityCode (511 and 512) or East Midlands makes this quicker to run.
#'
#' @section Last updated by:
#' Zoë Turner
#' @section Last updated date:
#' 2022-01-21
#'
#' @export

get_postcodes <- function(select_vars = TRUE,
                          conn = conn_sql_server,
                          local_area = c("local", "east_midlands", "all"),
                          return = c("tbl_sql", "tbl_df")) {

  # Check function arguments
  return <- match.arg(return)
  local_area <- match.arg(local_area)

  # Create connection to server
  if (!exists("conn_sql_database1")) {
    assign("conn_sql_database1",
      connect_sql(database = "database1"),
      envir = globalenv()
    )
  }

  # Create connection to table
  db_data <- dplyr::tbl(conn, dbplyr::in_schema("schema", "PostCodes"))

  if (local_area == "local") {
    db_data <- db_data %>%
      dplyr::filter(LocalAuthorityCode %in% c('511', '512'))
  }

  if (local_area == "east_midlands") {
    db_data <- db_data %>%
      dplyr::filter(RegionName == "East Midlands")
  }

  if (local_area == "all") {
    db_data <- db_data
  }

  # Select important variables only
  if (select_vars) {
    db_data <- db_data %>%
      dplyr::select(
        Postcode,
        Postcode_Space,
        Postcode_NoSpace,
        IntroductionDate,
        TerminationDate,
        CommissionerCode,
        LocalAuthority_Name,
        LSOAcode2011 = LSOA11,
        Latitude,
        Longitude
      )
  }

  # Return
  if (return == "tbl_sql") {
    db_data
  } else if (return == "tbl_df") {
    db_data %>%
      dplyr::collect()
  }
}

#' Tidy postcodes
#'
#' @description Tidies the postcode table by renaming the column names to
#' snake_case.
#'
#' @param data From \code{\link{get_postcodes}}
#' @param tidy_names Logical, specifying whether to rename variables to be
#' consistent across different tables
#' @section Last updated by:
#' Zoë Turner
#' @section Last updated date:
#' 2022-01-21
#'
#' @export
tidy_postcodes <- function(data,
                           tidy_names = TRUE) {

  if (tidy_names) {
    data <- data %>%
      dplyr::rename_all(janitor::make_clean_names) %>%
      dplyr::rename(
        postcode_nospace = postcode_no_space,
        lsoa_code_2011 = lso_acode2011
      )
  }

  data
}

# IMD ----
#' Get Index of Multiple Deprivation (IMD)
#'
#' @description IMD information imported to SQL db from central public source.
#' https://cdu-data-science-team.github.io/miscellany/Indices-of-Multiple-Deprivation.html
#' links to \code{\link{tidy_imd}}
#' @param area character, both are the default, but can be only
#' notts or eng as well
#' @param conn A database connection
#' @param return String, specifying whether to return a "tbl_sql" connection or
#' "tbl_df"
#' @param select_vars Logical, specifying whether to return selected variables
#' (TRUE)
#'
#' @section Last updated by:
#' Zoë Turner
#' @section Last updated date:
#' 2022-05-12
#'
#' @export
get_imd <- function(conn = conn_sql_server,
                    area = c("both", "eng", "notts"),
                    select_vars = TRUE,
                    return = c("tbl_sql", "tbl_df")) {

  # Check function arguments
  return <- match.arg(return)
  area <- match.arg(area)

  # Create connection to server
  if (!exists("conn_sql_database1")) {
    assign("conn_sql_database1",
      connect_sql(database = "database1"),
      envir = globalenv()
    )
  }

  # Create connection to table
  db_data <- dplyr::tbl(
    conn,
    dbplyr::in_schema("schema", "IMD")
  )

  if (select_vars) {
    db_data <- db_data %>%
      dplyr::select(
        LSOAcode2011,
        LSOAname2011,
        LADistrictCode2019,
        LADistrictName2019,
        IMDDecile,
        IMDQuintile,
        Notts_wide_decile,
        Notts_wide_quintile
      )
  }

  if (area == "notts") {
    message("Index of Multiple Deprivation for: Nottingham and Nottinghamshire")
    # returns only the Nott & Notts rows
    db_data <- db_data %>%
      dplyr::filter(!is.na(Notts_wide_decile))
  }

  if (area == "eng") {
    message("England returned as default - Nottinghamshire decile and quintile removed")
    db_data <- db_data %>%
      dplyr::select(
        -Notts_wide_decile,
        -Notts_wide_quintile
      )
  }

  if (area == "both") {
    message("All England and all quintiles/deciles returned")
    db_data <- db_data
  }

  # Return
  if (return == "tbl_sql") {
    db_data
  } else if (return == "tbl_df") {
    db_data %>%
      dplyr::collect()
  }
}

#' Tidying the IMD data
#'
#' @param data Data frame with class "tbl_df" or "tbl_sql"
#' from \code{\link{get_imd}}
#' @param tidy_names Logical, specifying whether to rename variables to be
#' consistent across different tables
#' or all variables from the database (FALSE).
#'
#' @section Last updated by:
#' Zoë Turner
#' @section Last updated date:
#' 2022-05-12
#'
#' @export
tidy_imd <- function(data,
                     tidy_names = TRUE) {

  if (tidy_names) {
    # list of the names reformatted after janitor:: that need correcting later
    original_names <- c(
      "lso_acode2011",
      "lso_aname2011",
      "la_district_code2019",
      "la_district_name2019",
      "imd_decile",
      "imd_quintile",
      "notts_wide_decile",
      "notts_wide_quintile"
    )
    # list of the names reformatted to how we want. Note the renaming of the
    # decile and quintiles
    new_names <- c(
      "lsoa_2011_code",
      "lsoa_2011_name",
      "la_district_2019_code",
      "la_district_2019_name",
      "eng_imd_decile",
      "eng_imd_quintile",
      "notts_imd_decile",
      "notts_imd_quintile"
    )
    # rename the old columns with the new names
    rename_vars <- setNames(
      object = original_names,
      nm = new_names
    )

    # janitor:: doesn't work server side but dplyr:: does so this is a
    # workaround with janitor:: inside the rename_all
    data <- data %>%
      dplyr::rename_all(janitor::make_clean_names) %>%
      dplyr::rename(dplyr::all_of(rename_vars))
  }

  data
}
