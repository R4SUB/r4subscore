#' Render an SCI Report as a Markdown Readiness Memo
#'
#' Turns the object returned by [sci_report()] into a single shareable Markdown
#' string: a readiness memo that states the Submission Confidence Index and its
#' decision band, the distance to target when one was set, the per-pillar
#' breakdown, the findings most worth acting on, and the pillars that offer the
#' biggest improvement. Where [print.sci_report()] writes a styled console
#' view, this produces a document you can paste into a pull request, an issue,
#' a status email, or a wiki.
#'
#' @details
#' The renderer reads the report object only; it does not recompute the SCI,
#' the band, or the gate, and it changes no scoring semantics. The rendered memo
#' carries an explicit line stating that it is decision support, not a
#' regulatory filing document.
#'
#' `report_markdown()` is generic so other report-like objects can supply their
#' own method; the method for `sci_report` is provided here.
#'
#' @param x An `sci_report` object from [sci_report()].
#' @param title Heading for the memo. Default `"Submission Readiness Memo"`.
#' @param ... Passed to methods; currently unused.
#'
#' @return A length-one character string of Markdown, invisibly safe to
#'   `cat()`, `writeLines()`, or write to a `.md` file.
#'
#' @examples
#' ctx <- suppressMessages(r4subcore::r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(r4subcore::as_evidence(
#'   data.frame(
#'     asset_type = "validation", asset_id = "ADSL",
#'     source_name = "pinnacle21",
#'     indicator_id = c("Q1", "Q2", "T1", "R1", "U1"),
#'     indicator_name = c("Missing var", "Label", "Trace", "Risk", "Reviewer guide"),
#'     indicator_domain = c("quality", "quality", "trace", "risk", "usability"),
#'     severity = c("high", "low", "medium", "high", "low"),
#'     result = c("fail", "pass", "warn", "fail", "pass"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' rpt <- sci_report(ev, targets = "ready")
#' md <- report_markdown(rpt)
#' cat(md)
#'
#' @export
report_markdown <- function(x, ...) {
  UseMethod("report_markdown")
}

#' @rdname report_markdown
#' @export
report_markdown.sci_report <- function(x, title = "Submission Readiness Memo",
                                       ...) {
  esc <- function(s) {
    s <- ifelse(is.na(s), "", as.character(s))
    gsub("|", "\\|", s, fixed = TRUE)
  }
  lines <- character(0)
  add <- function(...) lines <<- c(lines, ...)

  add(paste0("# ", title))
  add("")
  add(paste0("_This is a decision-support summary, not a regulatory ",
             "filing document._"))
  add("")
  add(paste0("Generated: ", format(x$generated_at, "%Y-%m-%d %H:%M:%S %Z")))
  add("")

  # Overall ----------------------------------------------------------------
  add("## Overall")
  add("")
  add(paste0("- **Submission Confidence Index (SCI):** ", x$sci, " / 100"))
  add(paste0("- **Decision band:** ", x$band))
  if (isTRUE(x$gated)) {
    add(paste0("- **Band capped:** ", x$gate_reason))
  }
  if (!is.null(x$target)) {
    if (isTRUE(x$target$met)) {
      add(paste0("- **Target:** meets ", x$target$target_sci, " (band ",
                 x$target$target_band, ")."))
    } else {
      add(paste0("- **Target:** ", x$target$gap,
                 " SCI points below ", x$target$target_sci, " (band ",
                 x$target$target_band, ")."))
    }
  }
  add("")

  # Pillars ----------------------------------------------------------------
  add("## Pillars")
  add("")
  add("| Pillar | Score | Weight | Contribution |")
  add("| --- | ---: | ---: | ---: |")
  for (i in seq_len(nrow(x$pillars))) {
    sc <- x$pillars$score[i]
    sc_str <- if (is.na(sc)) "N/A" else as.character(sc)
    add(paste0("| ", esc(x$pillars$pillar[i]), " | ", sc_str, " | ",
               x$pillars$weight[i], " | ", x$pillars$contribution[i], " |"))
  }
  add("")

  # Findings ---------------------------------------------------------------
  add(paste0("## Findings to act on (", x$n_gaps,
             if (x$n_gaps == 1L) " finding)" else " findings)"))
  add("")
  if (nrow(x$gaps) == 0L) {
    add("No failing or warning findings.")
  } else {
    add("| Severity | Result | Indicator | Domain | Detail |")
    add("| --- | --- | --- | --- | --- |")
    for (i in seq_len(nrow(x$gaps))) {
      msg <- x$gaps$message[i]
      detail <- if (is.na(msg) || !nzchar(msg)) "" else esc(msg)
      add(paste0("| ", esc(x$gaps$severity[i]), " | ", esc(x$gaps$result[i]),
                 " | ", esc(x$gaps$indicator_id[i]), " ",
                 esc(x$gaps$indicator_name[i]), " | ",
                 esc(x$gaps$indicator_domain[i]), " | ", detail, " |"))
    }
    if (x$n_gaps > nrow(x$gaps)) {
      add("")
      add(paste0("_Showing the top ", nrow(x$gaps), " of ", x$n_gaps,
                 " findings._"))
    }
  }
  add("")

  # Levers -----------------------------------------------------------------
  add("## Biggest levers")
  add("")
  lv <- x$levers
  top <- utils::head(lv[!is.na(lv$max_lift), , drop = FALSE], 3L)
  if (nrow(top) == 0L) {
    add("No further headroom to report.")
  } else {
    for (i in seq_len(nrow(top))) {
      add(paste0("- **", esc(top$pillar[i]), ":** up to ", top$max_lift[i],
                 " SCI points"))
    }
  }
  add("")

  paste0(paste(lines, collapse = "\n"), "\n")
}
