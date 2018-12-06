#' Day 04: Repose Record
#'
#' [Repose Record](https://adventofcode.com/2018/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You've
#' [sneaked]{title="Yes, 'sneaked'. 'Snuck' didn't appear in English until the 1800s."}
#' into another supply closet - this time, it's across from the prototype
#' suit manufacturing lab. You need to sneak inside and fix the issues with
#' the suit, but there's a guard stationed outside the lab, so this is as
#' close as you can safely get.
#'
#' As you search the closet for anything that might help, you discover that
#' you're not the first person to want to sneak in. Covering the walls,
#' someone has spent an hour starting every midnight for the past few
#' months secretly observing this guard post! They've been writing down the
#' ID of *the one guard on duty that night* - the Elves seem to have
#' decided that one guard was enough for the overnight shift - as well as
#' when they fall asleep or wake up while at their post (your puzzle
#' input).
#'
#' For example, consider the following records, which have already been
#' organized into chronological order:
#'
#'     [1518-11-01 00:00] Guard #10 begins shift
#'     [1518-11-01 00:05] falls asleep
#'     [1518-11-01 00:25] wakes up
#'     [1518-11-01 00:30] falls asleep
#'     [1518-11-01 00:55] wakes up
#'     [1518-11-01 23:58] Guard #99 begins shift
#'     [1518-11-02 00:40] falls asleep
#'     [1518-11-02 00:50] wakes up
#'     [1518-11-03 00:05] Guard #10 begins shift
#'     [1518-11-03 00:24] falls asleep
#'     [1518-11-03 00:29] wakes up
#'     [1518-11-04 00:02] Guard #99 begins shift
#'     [1518-11-04 00:36] falls asleep
#'     [1518-11-04 00:46] wakes up
#'     [1518-11-05 00:03] Guard #99 begins shift
#'     [1518-11-05 00:45] falls asleep
#'     [1518-11-05 00:55] wakes up
#'
#' Timestamps are written using `year-month-day hour:minute` format. The
#' guard falling asleep or waking up is always the one whose shift most
#' recently started. Because all asleep/awake times are during the midnight
#' hour (`00:00` - `00:59`), only the minute portion (`00` - `59`) is
#' relevant for those events.
#'
#' Visually, these records show that the guards are asleep at these times:
#'
#'     Date   ID   Minute
#'                 000000000011111111112222222222333333333344444444445555555555
#'                 012345678901234567890123456789012345678901234567890123456789
#'     11-01  #10  .....####################.....#########################.....
#'     11-02  #99  ........................................##########..........
#'     11-03  #10  ........................#####...............................
#'     11-04  #99  ....................................##########..............
#'     11-05  #99  .............................................##########.....
#'
#' The columns are Date, which shows the month-day portion of the relevant
#' day; ID, which shows the guard on duty that day; and Minute, which shows
#' the minutes during which the guard was asleep within the midnight hour.
#' (The Minute column's header shows the minute's ten's digit in the first
#' row and the one's digit in the second row.) Awake is shown as `.`, and
#' asleep is shown as `#`.
#'
#' Note that guards count as asleep on the minute they fall asleep, and
#' they count as awake on the minute they wake up. For example, because
#' Guard \#10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as
#' awake.
#'
#' If you can figure out the guard most likely to be asleep at a specific
#' time, you might be able to trick that guard into working tonight so you
#' can have the best chance of sneaking in. You have two strategies for
#' choosing the best guard/minute combination.
#'
#' *Strategy 1:* Find the guard that has the most minutes asleep. What
#' minute does that guard spend asleep the most?
#'
#' In the example above, Guard \#10 spent the most minutes asleep, a total
#' of 50 minutes (20+25+5), while Guard \#99 only slept for a total of 30
#' minutes (10+10+10). Guard \#*10* was asleep most during minute *24* (on
#' two days, whereas any other minute the guard was asleep was only seen on
#' one day).
#'
#' While this example listed the entries in chronological order, your
#' entries are in the order you found them. You'll need to organize them
#' before they can be analyzed.
#'
#' *What is the ID of the guard you chose multiplied by the minute you
#' chose?* (In the above example, the answer would be `10 * 24 = 240`.)
#'
#' **Part Two**
#'
#' *Strategy 2:* Of all guards, which guard is most frequently asleep on
#' the same minute?
#'
#' In the example above, Guard \#*99* spent minute *45* asleep more than
#' any other guard or minute - three times in total. (In all other cases,
#' any guard spent any minute asleep at most twice.)
#'
#' *What is the ID of the guard you chose multiplied by the minute you
#' chose?* (In the above example, the answer would be `99 * 45 = 4455`.)
#'
#' @param x a character vector of guard records
#' @return For Part One, `find_sleepiest_minute_of_sleepiest_guard(x)` returns a
#'   dataframe row with the sleepiest minute of the shift by the guard who
#'   sleeps the most. For Part Two, `find_guard_with_sleepiest_minute(x)`
#'   returns a dataframe with the sleepiest minute overall of any guard.
#' @export
#' @examples
#' x <- "[1518-11-01 00:00] Guard #10 begins shift
#' [1518-11-01 00:05] falls asleep
#' [1518-11-01 00:25] wakes up
#' [1518-11-01 00:30] falls asleep
#' [1518-11-01 00:55] wakes up
#' [1518-11-01 23:58] Guard #99 begins shift
#' [1518-11-02 00:40] falls asleep
#' [1518-11-02 00:50] wakes up
#' [1518-11-03 00:05] Guard #10 begins shift
#' [1518-11-03 00:24] falls asleep
#' [1518-11-03 00:29] wakes up
#' [1518-11-04 00:02] Guard #99 begins shift
#' [1518-11-04 00:36] falls asleep
#' [1518-11-04 00:46] wakes up
#' [1518-11-05 00:03] Guard #99 begins shift
#' [1518-11-05 00:45] falls asleep
#' [1518-11-05 00:55] wakes up"
#' x <- read_text_lines(x)
#' find_sleepiest_minute_of_sleepiest_guard(x)
#' f2()
find_sleepiest_minute_of_sleepiest_guard <- function(x) {
  df <- x %>%
    parse_guard_log() %>%
    fill_sleep_minutes()

  sleepiest_guard <- df %>%
    aggregate2(asleep ~ guard, sum) %>%
    keep_rows(asleep == max(asleep)) %>%
    getElement("guard")

  df %>%
    aggregate2(asleep ~ minute + guard, sum) %>%
    keep_rows(guard == sleepiest_guard) %>%
    keep_rows(asleep == max(asleep))
}

#' @rdname day04
#' @export
find_guard_with_sleepiest_minute <- function(x) {
  df <- x %>%
    parse_guard_log() %>%
    fill_sleep_minutes()

  df %>%
    aggregate2(asleep ~ minute + guard, sum) %>%
    keep_rows(asleep == max(asleep))
}

parse_guard_log <- function(x) {
  # Ugh this sucks without dplyr

  # Extract parts of record into columns
  date <- "((\\d{4})-(\\d+)-(\\d+)) (\\d+):(\\d+)"
  text <- "(Guard #(\\d+) begins shift|(falls asleep|wakes up))"
  pattern <- stringr::str_glue("\\[{date}\\] {text}")

  data <- x %>%
    sort() %>%
    stringr::str_match(pattern) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    setNames(
      c("line", "date", "year", "month", "day",
        "hour", "minute", "text", "guard", "activity")) %>%
    utils::type.convert(as.is = TRUE)

  # Sometimes the shift starts the day before the midnight shift (during the 11
  # PM hour). In those cases, add 1 day to the record's date
  data$date <- as.Date(data$date, origin = "UTC")
  data$date[data$hour == 23] <- data$date[data$hour == 23] + 1

  data$day <- data$date %>%
    format("%d") %>%
    as.integer()

  data$month <- data$date %>%
    format("%m") %>%
    as.integer()

  data$year <- data$date %>%
    format("%Y") %>%
    as.integer()

  # Put the guard ID into a column and remove the start shift record.
  data %>%
    split(.$date) %>%
    purrr::map_dfr(function(x) {
      x$guard <- x$guard[1]
      x[-1, ]
    })
}

fill_sleep_minutes <- function(df) {
  # Handle one day's records
  fill_one <- function(x) {
    # Assume awake every minute
    df_sleep <- tibble::tibble(
      minute = 0:59,
      guard = x$guard[1],
      date = x$date[1],
      awake = TRUE)

    # Find sleeping intervals
    starts <- which(x$activity == "falls asleep")
    ends <- which(x$activity == "wakes up")

    snoozing <- x$minute[starts] %>%
      purrr::map2(x$minute[ends] - 1, seq) %>%
      purrr::flatten_int()

    # Change those minutes to sleeping
    df_sleep[df_sleep$minute %in% snoozing, "awake"] <- FALSE
    df_sleep$asleep <- !df_sleep$awake
    df_sleep[c("date", "guard", "minute", "awake", "asleep")]
  }

  df %>%
    split(.$date) %>%
    purrr::map_dfr(fill_one)
}

aggregate2 <-function(data, formula, ... ){
  aggregate(formula, data, ...)
}

keep_rows <- function(df, ...) {
  dots <- quos(...)
  for (dot in dots) {
    df <- df[rlang::eval_tidy(dot, df), ]
  }
  df
}
