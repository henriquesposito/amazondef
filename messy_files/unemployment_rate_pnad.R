# Source: https://www.scielo.br/j/ee/a/YQcXMRc5xjPBgPHZztrnKDj/?lang=pt
# PNAD reconstructed data
# Source: https://painel.ibge.gov.br/pnadc/
# From 2012 onwards available
unemployment_rate_pnad <- data.frame(year = c(1985, 1986, 1987, 1988, 1989,
                                              1990, 1991, 1992, 1993, 1994,
                                              1995, 1996, 1997, 1998, 1999,
                                              2000, 2001, 2002, 2003, 2004,
                                              2005, 2006, 2007, 2008, 2009,
                                              2010, 2011, 2012, 2013, 2014,
                                              2015, 2016, 2017, 2018, 2019,
                                              2020, 2021, 2022),
                                     UR = c(5.61, 4.60, 4.81, 5.29, 4.82,
                                            5.13, 5.94, 7.16, 7.06, 6.93,
                                            6.77, 7.51, 8.02, 9.24, 9.84,
                                            9.38, 9.46, 9.82, 10.42, 10.42,
                                            10.02, 9.58, 9.14, 8.46, 9.09,
                                            8.19, 7.52, 7.3, 7.1, 6.8,
                                            8.5, 11.5, 13.4, 12.8, 12.4,
                                            13.1, 14.7, 9.8))
saveRDS(unemployment_rate_pnad, "urate.Rds")
