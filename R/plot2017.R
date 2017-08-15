#'Plot a leaflet map of cost per person, per CCG, per month of a BNF section, chemical or presentations.
#'
#' @param chemical_section_or_presentation_code An ID unique to BNF sections, chemicals or presentations.
#' @return Returns a leaflet plot (html widget) of cost per person, per CCG, per month of the input BNF section, drug or chemcial.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' A plot of BNF section 7.4.5 spending = plot2017("7.4.5")
plot2017 <- function(chemical_section_or_presentation_code)
{
  ccggeom <- sf::st_read("https://openprescribing.net/api/1.0/org_location/?org_type=ccg") %>%
    dplyr::rename(row_name = name) %>%
    dplyr::select(-ons_code, -org_type)

  dataframe <- dplyr::full_join(
    (openprescribingR::list_size() %>%
       dplyr::select(-row_id)),
    (openprescribingR::spending_by_CCG(chemical_section_or_presentation_code =
                       chemical_section_or_presentation_code)),
    by = c("row_name", "date")) %>%
    dplyr::mutate(costperperson = actual_cost/total_list_size) %>%
    dplyr::full_join(ccggeom, by="row_name") %>%
    sf::st_as_sf() %>%
    dplyr::mutate(label = stringr::str_c(row_name, " £", format(round(costperperson, 2), nsmall = 2)))

  daterange <- dplyr::filter(dataframe, date=="2017-01-01"|date=="2017-02-01"|date=="2017-03-01"|date=="2017-04-01"|date=="2017-05-01")$costperperson

  pal <- colorNumeric(palette = "magma",
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
      color = ~pal(costperperson),
      group = "May",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-04-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-04-01")$label,
      fillOpacity =0.8,
      color = ~pal(costperperson),
      group = "April",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-03-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-03-01")$label,
      fillOpacity =0.8,
      color = ~pal(costperperson),
      group = "March",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-02-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-02-01")$label,
      fillOpacity =0.8,
      color = ~pal(costperperson),
      group = "February",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addPolygons(
      data = dplyr::filter(dataframe, date=="2017-01-01"),
      weight = 2,
      label = dplyr::filter(dataframe,
                            date=="2017-01-01")$label,
      fillOpacity =0.8,
      color = ~pal(costperperson),
      group = "January",
      highlightOptions = highlightOptions(color = "black",
                                          weight = 2)) %>%

    leaflet::addLegend("bottomleft", pal = pal, values = daterange,
              title = stringr::str_c(chemical_section_or_presentation_code,
                                     " Items cost per person on CCG list"),
              labFormat = labelFormat(prefix = "£"),
              opacity = 1
    ) %>%

    leaflet::addLayersControl(
      baseGroups = c("May", "April", "March",
                     "February", "January", "Nothing"),
      options = layersControlOptions(collapsed = TRUE)
    )
}
