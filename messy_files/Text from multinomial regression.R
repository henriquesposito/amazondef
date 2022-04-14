````{=tex}
\begin{landscape}

```{r full regression results, echo=FALSE, message=FALSE, warning=FALSE, out.width="50%", paged.print=FALSE, include=TRUE}
# data on deforestation rates per year (PRODES)
# source: http://terrabrasilis.dpi.inpe.br/app/dashboard/deforestation/biomes/legal_amazon/rates
# Since annual deforestation rates are calculated since 1988, we copy the rates of 1988 to 1985, 1986, and 1987.
# These rates are consistent with most estimates (see for example Prates and Bracha 2010 -
# https://www.scielo.br/j/ecos/a/59DMy3zmJdPHXzXRsHTkmNF/?lang=pt&format=pdf)
# inflation rates per year Brazil (average for year)
# source EU inflation: https://www.inflation.eu/en/inflation-rates/brazil/historic-inflation/cpi-inflation-brazil.aspx
# post estimation tests
chi_square <- chisq.test(ama_model$mx_cat, predict(model)) 
# Chi-square p value (p-value < 0.001) tells us that our model explains a
# significant amount of the variation in the data in comparison with a null model.
# null_model <- multinom(mx_cat ~ 1, data = ama_model) # Null model for a likelihood ration test, just in case.
# library(lmtest)
# lrtest(null_model, model) # We very much reject the hypothesis
# (i.e. the more complex model is significantly better than the null model).
# too much information... Let's just focus on what we want here...
# stargazer::stargazer(model, type = "text",
#                      covariate.labels = c("Brasilia", "International", "Non-AM state",
#                                           "Election", "Deforestation", "Inflation"),
#                      column.labels = c("EI", "SD", "Sov", "EI-Con", "EI-SD", "Sov-Con", "SD-EI-Con",
#                                        "Sov-EI", "SD-Con", "Other"),
#                      dep.var.labels.include = FALSE)
full_md <- tibble("Dependent Variable" = c("EI", "SD", "SOV", "EI-CON", "EI-SD", "SOV-CON", "SD-EI-CON",
                                           "SOV-EI", "SD-CON", "Other"),
                  "Brasilia" = c("-1.024*** (0.169)","-1.002*** (0.205)", "-0.206 (0.260)", "-0.341 (0.248)",
                                 "-1.019*** (0.273)", "0.420 (0.257)", "-1.197*** (0.330)", "-1.212*** (0.342)",
                                 "0.860*** (0.318)", "-0.892*** (0.172)"),
                  "International" = c("-0.612*** (0.192)", "-1.583*** (0.303)", "-1.427*** (0.431)", "-0.265 (0.292)",
                                      "-0.980*** (0.336)", "-0.451 (0.367)", "-1.110*** (0.407)", "-1.105*** (0.395)",
                                      "-0.117 (0.328)", "-0.774*** (0.202)"),
                  "Other States" = c("0.023 (0.197)", "-0.041 (0.230)", "0.324 (0.302)", "0.149 (0.296)", "0.071 (0.292)",
                                     "0.823*** (0.306)", "-0.466 (0.373)", "-0.136 (0.366)", "-0.074 (0.354)", 
                                     "-0.159 (0.206)"),
                  "Election Year" = c("-0.241 (0.186)", "0.028 (0.215)", "-0.684** (0.298)",  "-0.059 (0.253)",
                                      "-0.254 (0.281)", "-0.573 (0.406)", "0.064 (0.367)", "-1.243** (0.547)",
                                      "-0.067 (0.405)", "-0.451** (0.194)"),
                  "Annual Deforestation" = c("0.063*** (0.013)", "-0.031* (0.016)", "0.004 (0.019)", "0.023 (0.017)",
                                             "0.047** (0.019)", "0.005 (0.027)", "-0.013 (0.026)", "-0.001 (0.028)",
                                             "0.011 (0.026)", "0.059*** (0.013)"),
                  "Average Inflation" = c("-0.001*** (0.0001)", "-0.001*** (0.0002)", "-0.0001 (0.0002)",
                                          "-0.0001*** (0.0002)", "-0.0005** (0.0002)", "0.0002 (0.0002)",
                                          "-0.001** (0.0004)", "-0.0004 (0.0004)", "-0.001* (0.0004)",
                                          "-0.0002* (0.0001)"))
kbl(t(full_md), booktabs = T,caption = "Multinomial Regression Full", align = "c") %>%
  kable_styling(latex_options = c("striped", "scale_down"),
                position= "center", font_size = 9, full_width = TRUE) %>%
  column_spec(1, width="3 cm", bold=TRUE, latex_valign = "m") %>%
  column_spec(2, width="2 cm", latex_valign = "m") %>%
  column_spec(3, width = "2 cm", latex_valign = "m") %>%
  column_spec(4, width = "2 cm", latex_valign = "m") %>%
  column_spec(5, width="2 cm", latex_valign = "m") %>%
  column_spec(6, width = "2 cm", latex_valign = "m") %>%
  column_spec(7, width = "2 cm", latex_valign = "m") %>% 
  column_spec(8, width="2 cm", latex_valign = "m") %>%
  column_spec(9, width = "2 cm", latex_valign = "m") %>%
  column_spec(10, width = "2 cm", latex_valign = "m") %>% 
  column_spec(11, width = "2 cm", latex_valign = "m")