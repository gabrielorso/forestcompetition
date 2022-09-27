remedicao <- importar_base('data-raw/remedicao.csv', re = T)

usethis::use_data(remedicao, overwrite = TRUE)
