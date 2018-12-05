# techniques: objects

#' Day 01: Chronal Calibration
#'
#' [Chronal Calibration](https://adventofcode.com/2018/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' "We've detected some temporal anomalies," one of Santa's Elves at the
#' [Temporal Anomaly Research and Detection Instrument
#' Station]{title="It's about as big on the inside as you expected."} tells
#' you. She sounded pretty worried when she called you down here. "At
#' 500-year intervals into the past, someone has been changing Santa's
#' history!"
#'
#' "The good news is that the changes won't propagate to our time stream
#' for another 25 days, and we have a device" - she attaches something to
#' your wrist - "that will let you fix the changes with no such propagation
#' delay. It's configured to send you 500 years further into the past every
#' few days; that was the best we could do on such short notice."
#'
#' "The bad news is that we are detecting roughly *fifty* anomalies
#' throughout time; the device will indicate fixed anomalies with *stars*.
#' The other bad news is that we only have one device and you're the best
#' person for the job! Good lu--" She taps a button on the device and you
#' suddenly feel like you're falling. To save Christmas, you need to get
#' all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' After feeling like you've been falling for a few minutes, you look at
#' the device's tiny screen. "Error: Device must be calibrated before first
#' use. Frequency drift detected. Cannot maintain destination lock." Below
#' the message, the device shows a sequence of changes in frequency (your
#' puzzle input). A value like `+6` means the current frequency increases
#' by `6`; a value like `-3` means the current frequency decreases by `3`.
#'
#' For example, if the device displays frequency changes of
#' `+1, -2, +3, +1`, then starting from a frequency of zero, the following
#' changes would occur:
#'
#' -   Current frequency ` 0`, change of `+1`; resulting frequency ` 1`.
#' -   Current frequency ` 1`, change of `-2`; resulting frequency `-1`.
#' -   Current frequency `-1`, change of `+3`; resulting frequency ` 2`.
#' -   Current frequency ` 2`, change of `+1`; resulting frequency ` 3`.
#'
#' In this example, the resulting frequency is `3`.
#'
#' Here are other example situations:
#'
#' -   `+1, +1, +1` results in ` 3`
#' -   `+1, +1, -2` results in ` 0`
#' -   `-1, -2, -3` results in `-6`
#'
#' Starting with a frequency of zero, *what is the resulting frequency*
#' after all of the changes in frequency have been applied?
#'
#' **Part Two**
#'
#' You notice that the device repeats the same frequency change list over
#' and over. To calibrate the device, you need to find the first frequency
#' it reaches *twice*.
#'
#' For example, using the same list of changes above, the device would loop
#' as follows:
#'
#' -   Current frequency ` 0`, change of `+1`; resulting frequency ` 1`.
#' -   Current frequency ` 1`, change of `-2`; resulting frequency `-1`.
#' -   Current frequency `-1`, change of `+3`; resulting frequency ` 2`.
#' -   Current frequency ` 2`, change of `+1`; resulting frequency ` 3`.
#' -   (At this point, the device continues from the start of the list.)
#' -   Current frequency ` 3`, change of `+1`; resulting frequency ` 4`.
#' -   Current frequency ` 4`, change of `-2`; resulting frequency ` 2`,
#'     which has already been seen.
#'
#' In this example, the first frequency reached twice is `2`. Note that
#' your device might need to repeat its list of frequency changes many
#' times before a duplicate frequency is found, and that duplicates might
#' be found while in the middle of processing the list.
#'
#' Here are other examples:
#'
#' -   `+1, -1` first reaches `0` twice.
#' -   `+3, +3, +4, -2, -4` first reaches `10` twice.
#' -   `-6, +3, +8, +5, -6` first reaches `5` twice.
#' -   `+7, +7, -2, -7, -4` first reaches `14` twice.
#'
#' *What is the first frequency your device reaches twice?*
#'
#' @param x integer vector of frequencies to sum together or check for duplicate
#'   sums
#' @return For Part One, `sum_frequency(x)` returns the sum of the frequencies.
#'   For Part Two, `analyze_frequency_stream(x)` returns the first repeated sum.
#' @export
#' @examples
#' sum_frequency(c(+1, -2, +3, +1))
#' analyze_frequency_stream(c(+1, -1))
#' analyze_frequency_stream(c(+7, +7, -2, -7, -4))
sum_frequency <- function(x) {
  sum(x)
}

#' @rdname day01
#' @export
analyze_frequency_stream <- function(x) {
  f <- frequency_stream(x)
  while (!f$has_duplicated_value()) {
    f$follow_stream()
  }
  f$get_duplicated_value()
}

frequency_stream <- function(x) {
  # We only need to compute the cumulative sums once. Consider the stream:
  #
  #   c(+7, +7, -2, -7, -4)
  #
  # Starting from 0, its cumulative sums are
  #
  #   c(0, 7, 14, 12, 5, 1)
  #
  # No duplicates found yet, so we have to keep adding the values. We can add
  # the last value of this history of values to the cumulative sums to get the
  # next batch of sums.
  #
  #   1 + c(7, 14, 12, 5, 1)
  #   c(8, 15, 13, 6, 2)
  #
  # (I had to drop the 0 from the cumulative sums to prevent the final 1 from
  # being counted twice. In the implementation below, I drop the last value of
  # the sums but keep 0 to acheive the same effect.)
  #
  # No duplicates yet. Try adding the final value to the cumulative sums again.
  #
  #   2 + c(7, 14, 12, 5, 1)
  #   c(9, 16, 14, 7, 3)
  #
  # Oh, and there's 14 again!

  sequence <- x
  sums <- c(0L, cumsum(sequence))
  current_value <- 0L
  history <- integer(0)
  duplicated_value <- integer(0)

  # Do one pass through the stream of values, checking for a duplicated value
  follow_stream <- function() {
    curr_sums <- sums + current_value
    current_value <<- tail(curr_sums, 1)
    # remove last element because it will be the first item in the next batch
    curr_sums <- head(curr_sums, -1)
    history <<- c(history, curr_sums)
    duplicated_value <<- history[anyDuplicated(history)]
  }

  get_duplicated_value <- function() {
    duplicated_value
  }

  has_duplicated_value <- function() {
    length(duplicated_value) != 0
  }

  list(
    follow_stream = follow_stream,
    get_duplicated_value = get_duplicated_value,
    has_duplicated_value = has_duplicated_value
  )
}


