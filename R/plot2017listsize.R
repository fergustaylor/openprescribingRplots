#'Plot a leaflet map of list size per CCG, per month.
#'
#' @return Returns a leaflet plot (html widget) of total list size, per CCG, per month.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' plot2017listsize()
plot2017listsize <- function()
{
  ccggeom <- sf::st_read("https://openprescribing.net/api/1.0/org_location/?org_type=ccg") %>%
    dplyr::rename(row_name = name) %>%
    dplyr::select(-ons_code, -org_type)

  dataframe <- openprescribingR::list_size() %>%
    dplyr::select(-row_id) %>%
    dplyr::full_join(ccggeom, by="row_name") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(label = stringr::str_c(row_name, " No. = ", total_list_size))

  daterange <- dplyr::filter(dataframe, date=="2017-01-01"|date=="2017-02-01"|date=="2017-03-01"|date=="2017-04-01"|date=="2017-05-01")$total_list_size

  pal <- leaflet::colorNumeric(palette = "magma",
                      domain = dataframe$total_list_size)

  leaflet::leaflet(data=dataframe) %>%
    leaflet::setView(-1.341739, 53.104565, zoom = 6) %>%
    leaflet::addTiles()  %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-05-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-05-01")$label,
      fillOpacity =0.8,
      color = ~pal(dataframe$total_list_size),
      group = "May",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-04-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-04-01")$label,
      fillOpacity =0.8,
      color = ~pal(dataframe$total_list_size),
      group = "April",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-03-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-03-01")$label,
      fillOpacity =0.8,
      color = ~pal(dataframe$total_list_size),
      group = "March",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-02-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-02-01")$label,
      fillOpacity =0.8,
      color = ~pal(dataframe$total_list_size),
      group = "February",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-01-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-01-01")$label,
      fillOpacity =0.8,
      color = ~pal(dataframe$total_list_size),
      group = "January",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addLegend("bottomleft", pal = pal, values = daterange,
              title = "Total list size",
              opacity = 1) %>%

    leaflet::addLayersControl(
      baseGroups = c("May", "April", "March",
                     "February", "January", "Nothing"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}
