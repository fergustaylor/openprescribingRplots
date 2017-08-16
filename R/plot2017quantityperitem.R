#'Plot a leaflet map of quantity per item, per CCG, per month of a BNF section, chemical or presentations.
#'
#' @param argument An ID unique to BNF sections, chemicals or presentations.
#' @return Returns a leaflet plot (html widget) of quantity per item, per CCG, per month of the input BNF section, drug or chemcial.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' plot2017quantityperitem("7.4.5")
plot2017quantityperitem <- function(argument)
{
  ccggeom <- sf::st_read("https://openprescribing.net/api/1.0/org_location/?org_type=ccg") %>%
    dplyr::rename(row_name = name) %>%
    dplyr::select(-ons_code, -org_type)

  dataframe <- dplyr::full_join(
    (openprescribingR::list_size() %>%
       dplyr::select(-row_id)),
    (openprescribingR::spending_by_CCG(chemical_section_or_presentation_code =
                       argument)),
    by = c("row_name", "date")) %>%
    dplyr::mutate(quantityperitem = quantity/items) %>%
    dplyr::full_join(ccggeom, by="row_name") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(label = stringr::str_c(row_name, " No.", format(round(quantityperitem, 2), nsmall = 2)))

  daterange <- dplyr::filter(dataframe, date=="2017-01-01"|date=="2017-02-01"|date=="2017-03-01"|date=="2017-04-01"|date=="2017-05-01")$quantityperitem

  pal <- leaflet::colorNumeric(palette = "magma",
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
      color = ~pal(quantityperitem),
      group = "May",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-04-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-04-01")$label,
      fillOpacity =0.8,
      color = ~pal(quantityperitem),
      group = "April",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-03-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-03-01")$label,
      fillOpacity =0.8,
      color = ~pal(quantityperitem),
      group = "March",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-02-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-02-01")$label,
      fillOpacity =0.8,
      color = ~pal(quantityperitem),
      group = "February",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-01-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-01-01")$label,
      fillOpacity =0.8,
      color = ~pal(quantityperitem),
      group = "January",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addLegend("bottomleft", pal = pal, values = daterange,
              title = stringr::str_c(argument,
                                     " Quantity per Item"),
              opacity = 1
    ) %>%

    leaflet::addLayersControl(
      baseGroups = c("May", "April", "March",
                     "February", "January", "Nothing"),
      options = layersControlOptions(collapsed = TRUE)
    )
}
