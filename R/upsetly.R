#' Interactive UpSet plot with copyable tooltip text
#'
#' @description
#' `upsetly()` creates an interactive UpSet-like plot using 'plotly'. It supports:
#' - Counting intersections across multiple binary set columns.
#' - Visualizing set sizes (left bar chart) and intersections (top bar chart + dot matrix).
#' - Displaying rich tooltip text including intersection name, size, and members.
#' - Synchronizing the full tooltip text into an HTML element with id `members_box`
#'   (if present), which makes it easy to copy the information in Quarto/R Markdown.
#'
#' When used in an HTML environment (e.g., Quarto, R Markdown, or Shiny),
#' you can add a tag such as:
#' \preformatted{
#' <pre id="members_box" style="
#'   border: 1px solid #ccc;
#'   padding: 8px;
#'   min-height: 120px;
#'   white-space: pre-wrap;
#'   font-family: monospace;
#' "></pre>
#' }
#' Then:
#' - Moving the mouse over an intersection bar updates the box with the tooltip text
#'   (Intersection, Size, Members, etc.).
#' - Clicking a bar will **lock** the content so that hover on other bars does not change it.
#' - Double-clicking the plot background will **unlock** and clear the box.
#'
#' @param x A data frame containing at least the set columns and optionally an ID column.
#' @param set_cols Character vector of column names (in `x`) that define the sets.
#'   These columns are interpreted as 0/1 (or logical/character) membership indicators.
#'   If `NULL`, all columns except `id_col` will be treated as set columns.
#' @param id_col Optional name of a column in `x` that identifies each element
#'   (e.g. gene ID). If `NULL`, a synthetic `.elem_id` column will be created.
#' @param min_intersection_size Minimum size of an intersection (number of elements)
#'   to retain in the plot.
#' @param max_n_intersections Maximum number of intersections to keep, sorted by
#'   descending size. Also used as an upper bound for the number of member IDs
#'   shown in the tooltip for each intersection. If `NULL`, no explicit limit is applied.
#' @param point_size Size of points in the dot matrix.
#' @param bar_color_sets Bar color for set sizes (left).
#' @param bar_color_inters Bar color for intersections (top).
#' @param active_color Color for dots that are part of an intersection.
#' @param inactive_color Color for dots that are not part of an intersection.
#' @param line_color Color for vertical lines connecting dots in the dot matrix.
#' @param title Plot title.
#' @param height Height of the plot (pixels).
#' @param width Width of the plot (pixels).
#' @param members_per_line Number of member IDs shown per line in the tooltip
#'   (and in the synchronized text box).
#'
#' @return A 'plotly' htmlwidget. When rendered in HTML, the widget also injects
#'   JavaScript that synchronizes full tooltip text into `#members_box` if present,
#'   with click-to-lock and double-click-to-unlock behavior.
#'
#' @examples
#' \dontrun{
#' library(upsetly)
#'
#' # Example binary matrix
#' set.seed(1)
#' df <- data.frame(
#'   gene = paste0("g", 1:100),
#'   A = rbinom(100, 1, 0.3),
#'   B = rbinom(100, 1, 0.4),
#'   C = rbinom(100, 1, 0.2),
#'   stringsAsFactors = FALSE
#' )
#'
#' p <- upsetly(
#'   x = df,
#'   set_cols = c("A", "B", "C"),
#'   id_col = "gene",
#'   box_id = "test",
#'   max_n_intersections = 50,
#'   members_per_line = 10
#' )
#' p
#' }
#'
#' @export
upsetly <- function(
                      x,
                      set_cols = NULL,
                      id_col = NULL,
                      min_intersection_size = 1,
                      max_n_intersections = NULL,  # 图上最多显示的交集数（保留）
                      point_size = 8,
                      bar_color_sets = "black",
                      bar_color_inters = "black",
                      active_color = "black",
                      inactive_color = "lightgray",
                      line_color = "black",
                      title = "UpSet (plotly)",
                      height = 500,
                      width = NULL,
                      members_per_line = 20,
                      box_id = NULL                 # 要写入的 <pre> 的 id，例如 "gsea_c6"
) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Please install the 'plotly' package.")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Please install the 'htmlwidgets' package.")
  }

  stopifnot(is.data.frame(x))
  if (is.null(set_cols)) {
    set_cols <- setdiff(names(x), if (is.null(id_col)) character(0) else id_col)
  }
  if (!all(set_cols %in% names(x))) {
    stop(
      "These set_cols are not in the data: ",
      paste(setdiff(set_cols, names(x)), collapse = ", ")
    )
  }
  if (!is.null(id_col) && !id_col %in% names(x)) {
    stop("id_col is not in the data: ", id_col)
  }

  if (is.null(id_col)) {
    x$.elem_id <- seq_len(nrow(x))
    id_col <- ".elem_id"
  }

  ## 0/1 matrix ---------------------------------------------------------------
  mat <- x[, set_cols, drop = FALSE]
  for (nm in set_cols) {
    v <- mat[[nm]]
    if (is.logical(v)) {
      mat[[nm]] <- as.integer(v)
    } else if (is.numeric(v)) {
      mat[[nm]] <- ifelse(is.na(v) | v == 0, 0L, 1L)
    } else {
      mat[[nm]] <- ifelse(is.na(v) | v == "", 0L, 1L)
    }
  }

  ## set sizes ----------------------------------------------------------------
  set_sizes <- colSums(mat, na.rm = TRUE)
  if (all(set_sizes == 0)) {
    stop("All set columns are 0; no elements belong to these sets.")
  }

  ord <- order(set_sizes, decreasing = TRUE)
  set_cols  <- set_cols[ord]
  set_sizes <- set_sizes[ord]

  set_sizes_df <- tibble::tibble(
    set  = factor(names(set_sizes), levels = names(set_sizes)),
    size = as.numeric(set_sizes)
  )
  set_levels <- names(set_sizes)

  ## intersections ------------------------------------------------------------
  combo_df <- as.data.frame(mat[, set_cols, drop = FALSE])
  combo_df[[id_col]] <- x[[id_col]]
  combo_df$.combo_key <- apply(
    as.matrix(mat[, set_cols, drop = FALSE]),
    1, paste, collapse = "|"
  )

  inter_df <- combo_df %>%
    dplyr::group_by(.combo_key) %>%
    dplyr::summarise(
      count   = dplyr::n(),
      members = paste0(get(id_col), collapse = ", "),
      .groups = "drop"
    ) %>%
    dplyr::filter(count >= min_intersection_size)

  if (nrow(inter_df) == 0) {
    stop("No intersections with size >= min_intersection_size.")
  }

  ## expand back 0/1 per set -------------------------------------------------
  comb_mat_chr <- strsplit(inter_df$.combo_key, "\\|")
  comb_mat <- do.call(rbind, comb_mat_chr)
  comb_mat <- apply(comb_mat, 2, as.integer)
  colnames(comb_mat) <- set_cols
  comb_mat <- as.data.frame(comb_mat, stringsAsFactors = FALSE)
  inter_df <- dplyr::bind_cols(inter_df, comb_mat)

  ## 成员字符串包装函数 -------------------------------------------------------
  wrap_members <- function(members_str,
                           n_per_line = 20,
                           max_ids = NULL) {
    v <- strsplit(members_str, ",")[[1]]
    v <- trimws(v)
    v <- v[nzchar(v)]
    if (length(v) == 0) return("")

    truncated <- FALSE
    if (!is.null(max_ids) && length(v) > max_ids) {
      v <- v[seq_len(max_ids)]
      truncated <- TRUE
    }

    idx   <- ceiling(seq_along(v) / n_per_line)
    lines <- tapply(v, idx, function(x) paste(x, collapse = ", "))
    out   <- paste(lines, collapse = "<br>")

    if (truncated) out <- paste0(out, "<br>... (truncated)")
    out
  }

  ## 完整版本（给 <pre> 用）+ 图上版本（可截断） ------------------------------
  inter_df$members_wrapped_all <- vapply(
    inter_df$members,
    FUN        = wrap_members,
    FUN.VALUE  = character(1),
    n_per_line = members_per_line,
    max_ids    = NULL          # 完整，不截断
  )

  inter_df$members_wrapped <- vapply(
    inter_df$members,
    FUN        = wrap_members,
    FUN.VALUE  = character(1),
    n_per_line = members_per_line,
    max_ids    = max_n_intersections  # 图上 tooltip 可以被截断
  )

  ## tooltip 用的 hover text：保持原样（可能是截断的 members） -----------------
  inter_df$combo_name <- apply(inter_df[, set_cols, drop = FALSE], 1, function(row) {
    included <- set_cols[as.integer(row[set_cols]) == 1]
    if (length(included) == 0) "Ø" else paste(included, collapse = " & ")
  })

  inter_df$hover_bar <- paste0(
    "Intersection: ", inter_df$combo_name,
    "<br>Size: ", inter_df$count,
    "<br>Members:<br>", inter_df$members_wrapped
  )

  ## 给 <pre> 用的完整文本：Intersection + Size + 完整 Members ----------------
  inter_df$full_text_for_pre <- paste0(
    "Intersection: ", inter_df$combo_name,
    "<br>Size: ", inter_df$count,
    "<br>Members:<br>", inter_df$members_wrapped_all
  )

  ## 按 size 排序，并只保留前 max_n_intersections 个用于作图 -----------------
  inter_df <- inter_df %>% dplyr::arrange(dplyr::desc(count))
  if (!is.null(max_n_intersections)) {
    inter_df <- inter_df[seq_len(min(max_n_intersections, nrow(inter_df))), , drop = FALSE]
  }
  inter_df$combo_index <- seq_len(nrow(inter_df))

  ## dot matrix ---------------------------------------------------------------
  dot_df <- tidyr::expand_grid(
    combo_index = inter_df$combo_index,
    set         = factor(set_cols, levels = set_levels)
  ) %>%
    dplyr::left_join(inter_df[, c("combo_index", set_cols)], by = "combo_index") %>%
    dplyr::mutate(
      value = purrr::map2_int(
        set, combo_index,
        ~{
          s <- as.character(.x)
          i <- .y
          inter_df[[s]][inter_df$combo_index == i]
        }
      )
    )

  y_map    <- stats::setNames(seq_along(set_levels), set_levels)
  dot_df$y <- y_map[as.character(dot_df$set)]

  dot_df$hover_dot <- paste0(
    "Set: ", dot_df$set,
    "<br>Intersection: ",
    inter_df$combo_name[match(dot_df$combo_index, inter_df$combo_index)],
    "<br>Included: ", ifelse(dot_df$value == 1, "Yes", "No")
  )

  seg_df <- dot_df %>%
    dplyr::filter(value == 1) %>%
    dplyr::group_by(combo_index) %>%
    dplyr::summarise(
      y_min = min(y),
      y_max = max(y),
      .groups = "drop"
    )

  ## plotly -------------------------------------------------------------------
  fig <- plotly::plot_ly()

  # 1) intersection bars
  fig <- fig %>%
    plotly::add_bars(
      data       = inter_df,
      x          = ~combo_index,
      y          = ~count,
      marker     = list(color = bar_color_inters),
      hovertext  = ~hover_bar,              # tooltip 黑框：保持不变
      hoverinfo  = "text",
      customdata = ~full_text_for_pre,      # <pre> 用完整文本
      name       = "Intersections",
      yaxis      = "y1"
    )

  # 2) set sizes
  max_size      <- max(set_sizes_df$size)
  pretty_breaks <- pretty(c(0, max_size))
  x2_tickvals   <- -pretty_breaks
  x2_ticktext   <- pretty_breaks

  fig <- fig %>%
    plotly::add_bars(
      data        = set_sizes_df,
      x           = ~(-size),
      y           = ~as.numeric(set),
      orientation = "h",
      marker      = list(color = bar_color_sets),
      hovertext   = ~paste0(as.character(set), "<br>Numbers: ", size),
      hoverinfo   = "text",
      name        = "Set size",
      xaxis       = "x2",
      yaxis       = "y2"
    )

  # 3) segments
  if (nrow(seg_df) > 0) {
    fig <- fig %>%
      plotly::add_segments(
        data       = seg_df,
        x          = ~combo_index,
        xend       = ~combo_index,
        y          = ~y_min,
        yend       = ~y_max,
        line       = list(color = line_color, width = 1),
        hoverinfo  = "none",
        xaxis      = "x1",
        yaxis      = "y2",
        showlegend = FALSE
      )
  }

  # 4) dots
  fig <- fig %>%
    plotly::add_trace(
      data       = dot_df,
      x          = ~combo_index,
      y          = ~y,
      type       = "scatter",
      mode       = "markers",
      marker     = ~list(
        color = ifelse(value == 1, active_color, inactive_color),
        size  = point_size
      ),
      hovertext  = ~hover_dot,
      hoverinfo  = "text",
      xaxis      = "x1",
      yaxis      = "y2",
      showlegend = FALSE
    )

  # layout
  fig <- fig %>%
    plotly::layout(
      title = list(text = title),
      xaxis = list(
        domain = c(0.25, 1),
        anchor = "y1",
        title  = "",
        showticklabels = FALSE,
        tickfont = list(color = "black")
      ),
      xaxis2 = list(
        domain = c(0, 0.2),
        anchor = "y2",
        title  = list(text = "Numbers", font = list(color = "black")),
        range  = c(-max_size, 0),
        tickmode  = "array",
        tickvals  = x2_tickvals,
        ticktext  = x2_ticktext,
        zeroline  = TRUE,
        zerolinecolor = "black",
        zerolinewidth = 1,
        tickfont  = list(color = "black")
      ),
      yaxis = list(
        domain = c(0.5, 1),
        title  = list(text = "Intersections", font = list(color = "black")),
        tickfont = list(color = "black")
      ),
      yaxis2 = list(
        domain = c(0, 0.5),
        title  = list(text = "", font = list(color = "black")),
        tickmode = "array",
        tickvals = seq_along(set_levels),
        ticktext = set_levels,
        anchor   = "x2",
        side     = "right",
        tickfont = list(color = "black")
      ),
      showlegend = FALSE,
      height = height,
      width  = width
    )

  ## JS：tooltip 黑框不变；<pre> 显示完整 Intersection + Size + Members -------
  js <- sprintf("
  function(el, x) {
    var boxId = %s;
    if (!boxId) return;

    var pre = document.getElementById(boxId);
    if (!pre) return;

    // 对于 htmlwidgets 的 plotly，el 本身就是图形容器
    var gd = el;

    var locked = false;

    function setHoverText(txt, force) {
      if (!force && locked) return;
      pre.textContent = txt;
    }

    function htmlToPlain(html) {
      try {
        return String(html || '').replace(/<br\\s*\\/?\\>/gi, '\\n');
      } catch(e) {
        return String(html || '');
      }
    }

    if (gd && typeof gd.on === 'function') {
      // hover：<pre> 中显示完整信息（来自 customdata），tooltip 仍用 hovertext
      gd.on('plotly_hover', function(ev) {
        if (!ev || !ev.points || !ev.points.length) return;
        var pt  = ev.points[0];

        var fullHtml = pt.customdata || pt.hovertext || '';
        var txt      = htmlToPlain(fullHtml);

        setHoverText(txt, false);
      });

      // click：锁定当前 intersection 的完整信息
      gd.on('plotly_click', function(ev) {
        if (!ev || !ev.points || !ev.points.length) return;
        var pt = ev.points[0];

        var fullHtml = pt.customdata || pt.hovertext || '';
        var txt      = htmlToPlain(fullHtml);

        locked = true;
        setHoverText(txt, true);
      });

      // double click：清空并解锁
      gd.on('plotly_doubleclick', function() {
        locked = false;
        setHoverText('', true);
      });
    }
  }",
  if (is.null(box_id)) "null" else sprintf("'%s'", box_id)
  )

  fig <- htmlwidgets::onRender(fig, htmlwidgets::JS(js))
  fig
}
