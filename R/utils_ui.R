#' @import shiny
example_choice_input <- function(id){
  desc <- list(
    tagList(
      strong("Endosulfan"), br(),
      "Summary of 48- to 96-hour acute toxicity values (LC50 and EC50 values)
        for exposure of Australian and Non-Australian taxa to endosulfan.",
      br(),
      a("pubMed", href = "http://www.ncbi.nlm.nih.gov/pubmed/15499502",
        target = "_blank"), "|",
      a("Download", href = 'www/data/endosulfan.txt', target = "_blank")
      ),
    tagList(
      strong("Fluazinam"), br(),
      "48-hour acute toxicity values (EC50 values) for exposure of
      macroinvertebrates and zooplancton to fluazinam.",
      br(),
      a("pubMed", href = "http://www.ncbi.nlm.nih.gov/pubmed/19837458",
        target = "_blank"), "|",
      a("Download", href = 'www/data/fluazinam.txt', target = "_blank")
    ),
    tagList(
      strong("Salinity (order)"), br(),
      "72-hour acute salinity tolerance (LC50 values) of riverine
      macro-invertebrates with order group labels.",
      br(),
      a("Reference",
        href = "http://www.nrcresearchpress.com/doi/abs/10.1139/f06-080#.UzG8rkiOevI",
        target = "_blank"), "|",
      a("Download", href = 'www/data/salinity_order.txt', target = "_blank")
    ),
    tagList(
      strong("Salinity (family)"), br(),
      "72-hour acute salinity tolerance (LC50 values) of riverine
      macro-invertebrates with family group labels.",
      br(),
      a("Reference",
        href = "http://www.nrcresearchpress.com/doi/abs/10.1139/f06-080#.UzG8rkiOevI",
        target = "_blank"), "|",
      a("Download", href = 'www/data/salinity_family.txt', target = "_blank")
    )
  )
  radioButtons(inputId = id,
               label = NULL,
               choiceNames = desc,
               choiceValues = list("app/www/data/endosulfan.txt",
                                   "app/www/data/fluazinam.txt",
                                   "app/www/data/salinity_order.txt",
                                   "app/www/data/salinity_family.txt")
               )
}
