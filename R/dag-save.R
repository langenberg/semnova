#' Save a path diagram, cropped to what is actually drawn
#'
#' @description
#' [ggplot2::ggsave()] with the whitespace taken off. A path diagram rarely
#' fills its bounding box, the layout is fitted to the figure and the
#' margins have to be wide enough for the labels that overhang it, so a
#' saved file usually needs cropping by hand before it goes into a paper.
#' This does that step: save, trim the uniform border, add back a small even
#' margin.
#'
#' Dimensions default to the plot's own `fig_size` attribute when it has one
#' (see [dag()]), which is the size the diagram was built for.
#'
#' @param filename File to write. The extension picks the format, as in
#'   `ggsave()`.
#' @param plot The plot to save, by default the last one displayed.
#' @param width,height Figure size in `units`. `NULL` (the default) takes
#'   the plot's `fig_size` attribute if it has one, and otherwise leaves the
#'   choice to `ggsave()`.
#' @param units Units for `width` and `height`.
#' @param dpi Resolution for raster formats.
#' @param trim Whether to crop the saved file. Requires the `magick`
#'   package and a raster format; without either, the file is still written,
#'   uncropped, with a message saying so.
#' @param padding Margin to leave around the diagram after cropping, in
#'   pixels. `0` crops flush to the ink.
#' @param ... Passed to [ggplot2::ggsave()].
#'
#' @return The filename, invisibly.
#'
#' @examples
#' \donttest{
#' fit <- semnova(
#'     data = reading_manifest, id = "id", dv = "re_fix_time",
#'     withins = "grade", contrasts_arg = list(grade = "contr.sum")
#' )
#' dag_save(tempfile(fileext = ".png"), dag(fit))
#' }
#' @seealso [dag()]
#' @export
dag_save <- function(filename, plot = ggplot2::last_plot(), width = NULL, height = NULL,
                     units = "in", dpi = 300, trim = TRUE, padding = 10, ...) {
    for (pkg in c("ggplot2")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop_semnova("dag_save() requires the '", pkg, "' package")
        }
    }
    check_string(filename, "filename")
    check_flag(trim, "trim")
    check_positive(dpi, "dpi")
    if (!is.numeric(padding) || length(padding) != 1L || is.na(padding) || padding < 0) {
        stop_semnova("`padding` must be a single non-negative number")
    }
    if (!inherits(plot, "ggplot")) {
        stop_semnova("`plot` must be a ggplot object (see dag())")
    }
    if (!is.null(width)) check_positive(width, "width")
    if (!is.null(height)) check_positive(height, "height")

    # the size the diagram was built for, unless told otherwise
    fig_size <- attr(plot, "fig_size")
    if (!is.null(fig_size) && identical(units, "in")) {
        width <- width %||% unname(fig_size[["width"]])
        height <- height %||% unname(fig_size[["height"]])
    }

    size_args <- list(width = width, height = height)
    size_args <- size_args[!vapply(size_args, is.null, logical(1L))]
    do.call(
        ggplot2::ggsave,
        c(list(filename = filename, plot = plot, units = units, dpi = dpi), size_args, list(...))
    )

    if (trim) {
        dag_trim_file(filename, padding = padding)
    }
    invisible(filename)
}

#' @keywords internal
#' @noRd
#' Crop the uniform border off a saved image, leaving `padding` pixels.
#'
#' Split out so the two ways this can decline to do anything, magick not
#' installed, or a vector format it cannot sensibly trim, are one place, and
#' so that both say why rather than leaving a file that quietly was not
#' cropped. Vector formats are excluded deliberately: magick reads PDF and SVG
#' only through external tooling (ghostscript, rsvg), and rasterising a vector
#' file to crop it would silently throw away the reason for choosing it.
dag_trim_file <- function(filename, padding = 10) {
    raster <- c("png", "jpg", "jpeg", "tiff", "tif", "bmp", "gif")
    extension <- tolower(tools::file_ext(filename))

    if (!extension %in% raster) {
        message(
            "semnova: dag_save() cannot crop a .", extension, " file; ",
            "saved uncropped. Use a raster format (png, jpeg, tiff) to crop."
        )
        return(invisible(FALSE))
    }
    if (!requireNamespace("magick", quietly = TRUE)) {
        message(
            "semnova: dag_save() needs the 'magick' package to crop; saved uncropped. ",
            "Install it with install.packages(\"magick\")."
        )
        return(invisible(FALSE))
    }

    image <- magick::image_read(filename)
    image <- magick::image_trim(image)
    if (padding > 0) {
        image <- magick::image_border(
            image,
            color = "white",
            geometry = sprintf("%dx%d", as.integer(padding), as.integer(padding))
        )
    }
    magick::image_write(image, filename)
    invisible(TRUE)
}
