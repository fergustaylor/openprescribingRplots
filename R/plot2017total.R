#'Plot a leaflet map of total cost per CCG, per month of a BNF section, chemical or presentations.
#'
#' @param argument An ID unique to BNF sections, chemicals or presentations.
#' @return Returns a leaflet plot (html widget) of total cost, per CCG, per month of the input BNF section, drug or chemcial.
#' @importFrom magrittr "%>%"
#' @import sf
#' @export
#' @examples
#' plot2017total("7.4.5")
plot2017total <- function(argument)
{
  ccggeom <- openprescribingR::CCG_boundaries_or_location(as_sf = TRUE) %>%
    dplyr::rename(row_name = name) %>%
    dplyr::select(-ons_code, -org_type)

  dataframe <- dplyr::full_join(
    (openprescribingR::list_size() %>%
       dplyr::select(-row_id)),
    (openprescribingR::spending_by_CCG(chemical_section_or_presentation_code =
                       argument)),
    by = c("row_name", "date")) %>%
    dplyr::full_join(ccggeom, by="row_name") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(label = stringr::str_c(row_name, " £", format(round(actual_cost, 2), nsmall = 2)))

  daterange <- dplyr::filter(dataframe, date=="2017-01-01"|date=="2017-02-01"|date=="2017-03-01"|date=="2017-04-01"|date=="2017-05-01")$actual_cost

  pal <- leaflet::colorNumeric(palette = "viridis",
                      domain = daterange)

  leaflet::leaflet(data=dataframe) %>%
    leaflet::setView(-1.341739, 53.104565, zoom = 6) %>%
    leaflet::addTiles()  %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-05-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-05-01")$label,
      fillOpacity =0.8,
      color = ~pal(actual_cost),
      group = "May",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-04-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-04-01")$label,
      fillOpacity =0.8,
      color = ~pal(actual_cost),
      group = "April",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-03-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-03-01")$label,
      fillOpacity =0.8,
      color = ~pal(actual_cost),
      group = "March",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-02-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-02-01")$label,
      fillOpacity =0.8,
      color = ~pal(actual_cost),
      group = "February",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-01-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-01-01")$label,
      fillOpacity =0.8,
      color = ~pal(actual_cost),
      group = "January",
      highlightOptions = leaflet::highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addLegend("bottomleft", pal = pal, values = daterange,
              title = stringr::str_c(argument,
                                     " Items total cost"),
              labFormat = leaflet::labelFormat(prefix = "£"),
              opacity = 1
    ) %>%

    leaflet::addLayersControl(
      baseGroups = c("May", "April", "March",
                     "February", "January", "Nothing"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}
