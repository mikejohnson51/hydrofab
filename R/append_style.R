#' Read a QML file from the inst/qml directory
#' @param name Name of QML file without extension
#' @return the contents of {name}.qml.
#' @keywords internal
read_qml <- function(name) {
    qml_file <- paste0(name, ".qml")
    qml_path <- system.file("qml", qml_file, package = "hydrofab", mustWork = TRUE)
    paste(readLines(qml_path), collapse = "\n")
}

#' Create a style row from the given parameters.
#' @param gpkg_path Path to GeoPackage
#' @param layer_name Layer to create style for
#' @param style_name Name of the new style
#' @param style_qml QML contents for the new style
#' @return 1-row data.frame in the layer_styles table schema
#' @importFrom sf st_read
#' @keywords internal
create_style_row <- function(gpkg_path, layer_name, style_name, style_qml) {
    geom_col <- sf::st_read(
        gpkg_path,
        query = paste0(
            "SELECT column_name from gpkg_geometry_columns ",
            "WHERE table_name = '", layer_name, "'"),
        quiet = TRUE
    )[1, 1]

    data.frame(
        f_table_catalog = "",
        f_table_schema = "",
        f_table_name = layer_name,
        f_geometry_column = geom_col,
        styleName = style_name,
        styleQML = style_qml,
        styleSLD = "",
        useAsDefault = TRUE,
        description = "Generated for hydrofabric",
        owner = "",
        ui = NA,
        update_time = Sys.time()
    )
}

#' Append a hydrofabric style to a hydrofabric GeoPackage
#' @param gpkg_path Path to GeoPackage
#' @param layer_names character vector of names to append styles for.
#'                    These names must be in the package QML directory.
#' @importFrom sf st_layers st_read st_write
#' @export
append_style <- function(gpkg_path, layer_names) {
    styles      <- sapply(layer_names, read_qml)
    style_names <- sapply(layer_names, paste0, "__hydrofabric_style")
    style_rows  <- do.call(rbind, mapply(
        create_style_row,
        layer_names,
        style_names,
        styles,
        MoreArgs = list(gpkg_path = gpkg_path),
        SIMPLIFY = FALSE
    ))

    if ("layer_styles" %in% sf::st_layers(gpkg_path)$name) {
        try(st_delete(gpkg_path, "layer_styles"), silent = TRUE)
        # # read in current styles
        # old_style_rows <- sf::st_read(
        #     gpkg_path,
        #     layer = "layer_styles",
        #     quiet = TRUE
        # )
        # 
        # # remove old hydrofabric styles (if they exist)
        # old_style_rows <- old_style_rows[
        #     !(old_style_rows$styleName %in% style_names),
        # ]
        # 
        # # set our styles as default styles for new tables
        # old_style_rows[
        #     old_style_rows$f_table_name %in% layer_names
        # ]$useAsDefault <- FALSE
        # 
        # style_rows <- rbind(style_rows, old_style_rows)
    }

    # append the new layer_styles table to the gpkg
    st_write(
        style_rows,
        gpkg_path,
        layer = "layer_styles",
        append = FALSE,
        quiet = FALSE
    )
}
