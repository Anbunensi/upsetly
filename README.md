# upsetly

Interactive UpSet plots with **plotly**, with tooltip information that can be copied easily in HTML documents (Quarto, R Markdown, Shiny).

`upsetly` focuses on UpSet-style visualization of set intersections (e.g. differential gene lists), and adds JavaScript hooks so that all tooltip text (Intersection, Size, Members, etc.) can be synced into a separate text box for copy‑and‑paste.

---

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Anbunensi/upsetly")
```

Then load:

```r
library(upsetly)
```

---

## Basic idea

Given a data frame with:

- one ID column (e.g. `gene`), and  
- several 0/1 (or logical / character) columns indicating membership in sets  
  (e.g. comparisons `C_vs_D`, `C_vs_F`, …),

`upsetly()` will:

1. Compute all non‑empty intersections across the set columns.  
2. Keep only intersections with size ≥ `min_intersection_size`.  
3. Optionally keep only the largest `max_n_intersections` intersections.  
4. Draw:
   - **Left bar chart**: size of each set.  
   - **Top bar chart**: size of each intersection.  
   - **Bottom dot matrix**: which sets are present in each intersection.  
5. Build rich tooltip text for each intersection, including:
   - Intersection name (e.g. `C_vs_D & D_vs_F`)  
   - Intersection size  
   - Members (element IDs), wrapped onto multiple lines.

In HTML output, you can also display the full tooltip text in a separate box which supports copy‑and‑paste.

---

## Quick example

```r
library(upsetly)

set.seed(1)
df <- data.frame(
  gene = paste0("g", 1:100),
  A = rbinom(100, 1, 0.3),
  B = rbinom(100, 1, 0.4),
  C = rbinom(100, 1, 0.2),
  stringsAsFactors = FALSE
)

p <- upsetly(
  x = df,
  set_cols = c("A", "B", "C"),
  id_col = "gene",
  max_n_intersections = 50,
  members_per_line = 10
)

p  # interactive plotly widget
```

This works in a regular RStudio viewer (or browser) as an interactive UpSet plot.

---

## Using in Quarto / R Markdown with a copyable text box

To make the tooltip text (Intersection, Size, Members, etc.) easy to copy, you can add a small HTML element with id `members_box` and let `upsetly()` sync to it.

### Example Quarto document

````markdown
---
title: "upsetly demo"
format: html
---

```{r}
library(upsetly)
library(dplyr)
```
```{r}
set.seed(1)
df <- data.frame(
  gene = paste0("g", 1:100),
  A = rbinom(100, 1, 0.3),
  B = rbinom(100, 1, 0.4),
  C = rbinom(100, 1, 0.2),
  stringsAsFactors = FALSE
)

upsetly(
  x = df,
  set_cols = c("A", "B", "C"),
  id_col = "gene",
  max_n_intersections = 50,
  members_per_line = 10
)
```
```{=html}
<pre id="members_box" style="
  border: 1px solid #ccc;
  padding: 8px;
  min-height: 120px;
  white-space: pre-wrap;
  font-family: monospace;
"></pre>
```
````

### Interaction behavior

In the rendered HTML:

- **Hover** over an intersection bar:
  - The tooltip appears as usual.
  - The full tooltip text (Intersection, Size, Members, …) is also written into `#members_box`.
- **Single click** on a bar:
  - The current text is written into `#members_box` and **locked**.
  - Moving the mouse to other bars will not change the box content.
- **Double click** on the plot background:
  - The lock is released and the box is cleared.
  - You can then hover/click another bar to update again.

The content of `members_box` is pure text, so you can select and copy it easily.

---

## Key arguments

```r
upsetly(
  x,
  set_cols = NULL,
  id_col = NULL,
  min_intersection_size = 1,
  max_n_intersections = NULL,
  point_size = 8,
  bar_color_sets = "black",
  bar_color_inters = "black",
  active_color = "black",
  inactive_color = "lightgray",
  line_color = "black",
  title = "UpSet (plotly)",
  height = 500,
  width = NULL,
  members_per_line = 20
)
```

- `x`: data frame containing set columns and optional ID column.  
- `set_cols`: character vector of column names that define sets.  
- `id_col`: column name for element IDs (e.g. genes). If `NULL`, a numeric `.elem_id` is created.  
- `min_intersection_size`: minimum number of elements for an intersection to be kept.  
- `max_n_intersections`:
  - Limits how many intersections are kept (largest first).
  - Also limits how many member IDs are shown for each intersection in the tooltip.  
- `members_per_line`: how many member IDs to show per line in the tooltip / text box.  
- `point_size`: dot size in the dot matrix.  
- Color and layout arguments allow customization of bar colors, dot colors, and overall size.

---

## License

MIT © Anbunensi
```
