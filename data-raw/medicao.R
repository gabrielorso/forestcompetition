medicao <- importar_base('data-raw/medicao.csv', re = F)

usethis::use_data(medicao, overwrite = TRUE)
