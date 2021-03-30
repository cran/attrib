## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(attrib)
library(data.table)

## -----------------------------------------------------------------------------
data_fake_county <- attrib::data_fake_county
data_fake_nation <- attrib::data_fake_nation
head(data_fake_county, 5)

## -----------------------------------------------------------------------------
#response
response <- "deaths"

# fixed effects
fixef_county <- " temperature_high +
  pr100_ili_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52)"


#random effects
ranef_county <- "(1|location_code) +
  (pr100_ili_lag_1|season)"

#offset
offset_county <- "log(pop)"


## ---- message=FALSE, warning = FALSE------------------------------------------

fit_county <- fit_attrib(data_fake_county, 
                          response = response, 
                          fixef = fixef_county, 
                          ranef = ranef_county, 
                          offset = offset_county)


## -----------------------------------------------------------------------------
fit_county

## -----------------------------------------------------------------------------
# take in the fixed effects
response = "deaths"
fixef_nation <- "temperature_high +
  pr100_ili_lag_1 +
  sin(2 * pi * (week - 1) / 52) +
  cos(2 * pi * (week - 1) / 52)"


#take in the random effects
ranef_nation <- "(pr100_ili_lag_1|season)"

# take in the offset
offset_nation <- "log(pop)"


## ---- message = FALSE, warning=FALSE------------------------------------------

  fit_nation <- fit_attrib(data_fake_nation, 
                           response = response, 
                           fixef = fixef_nation, 
                           ranef = ranef_nation, 
                           offset = offset_nation)


## -----------------------------------------------------------------------------
n_sim <- 20
sim_data <- sim(fit_nation, data_fake_nation, n_sim)
head(sim_data[id_row == 1], 5)

## -----------------------------------------------------------------------------
exposures <- list( "temperature_high" = 0, "pr100_ili_lag_1" = 0)
n_sim <- 20
est_attrib_sim_county <- attrib::est_attrib(fit_county, 
                                        data_fake_county, 
                                        exposures = exposures, 
                                        n_sim = n_sim)

est_attrib_sim_nation <- attrib::est_attrib(fit_nation, 
                                        data_fake_nation, 
                                        exposures = exposures,
                                        n_sim = n_sim)

head(est_attrib_sim_county, 5)

## -----------------------------------------------------------------------------
est_attrib_county_long<-data.table::melt.data.table(est_attrib_sim_county, 
                                                  id.vars = c("location_code", 
                                                              "season",  
                                                              "x", 
                                                              "week", 
                                                              "id", 
                                                              "sim_id", 
                                                              "deaths", 
                                                              "sim_value_exposures=observed"),
                                           measure.vars = c("sim_value_temperature_high=0", 
                                                            "sim_value_pr100_ili_lag_1=0")) 
data.table::setnames(est_attrib_county_long, "variable", "attr")

head(est_attrib_county_long, 5)

## -----------------------------------------------------------------------------
est_attrib_nation_long<-data.table::melt.data.table(est_attrib_sim_nation, 
                                                  id.vars = c("location_code", 
                                                              "season",  
                                                              "x", 
                                                              "week", 
                                                              "id", 
                                                              "sim_id", 
                                                              "deaths", 
                                                              "sim_value_exposures=observed"),
                                           measure.vars = c("sim_value_temperature_high=0", 
                                                            "sim_value_pr100_ili_lag_1=0")) 
data.table::setnames(est_attrib_nation_long, "variable", "attr")


## -----------------------------------------------------------------------------
aggregated_county_to_nation <-  est_attrib_county_long[,.(
  "sim_value_exposures=observed" = sum(`sim_value_exposures=observed`),
  value = sum(value), 
  deaths = sum(deaths)
), keyby = .(season, attr, sim_id)]

# Add exp_attr, exp_irr and a tag.
aggregated_county_to_nation[, exp_attr:= (`sim_value_exposures=observed` - value)]
aggregated_county_to_nation[, tag := "aggregated_from_county"]

head(aggregated_county_to_nation, 5)

## -----------------------------------------------------------------------------
aggregated_nation <-  est_attrib_nation_long[, .(
  "sim_value_exposures=observed" = sum(`sim_value_exposures=observed`),
  value = sum(value), 
  deaths = sum(deaths)
), keyby = .(season, attr, sim_id)]

aggregated_nation[, exp_attr:= (`sim_value_exposures=observed` - value)]
aggregated_nation[, tag:= "nation"]
head(aggregated_nation, 5)

## -----------------------------------------------------------------------------
library(ggplot2)
data_national<- data.table::rbindlist(list(aggregated_county_to_nation, aggregated_nation))

## -----------------------------------------------------------------------------
# Quantile functins
q05 <- function(x){
  return(quantile(x, 0.05))
}
q95 <- function(x){
  return(quantile(x, 0.95))
}

## -----------------------------------------------------------------------------
col_names <- colnames(data_national)
data.table::setkeyv(data_national, 
                    col_names[!col_names %in% c("exp_attr", 
                                                "sim_id", 
                                                "sim_value_exposures=observed", 
                                                "value", 
                                                "deaths")])

aggregated_sim_seasonal_data_national<- data_national[,
                                   unlist(recursive = FALSE, 
                                          lapply(.(median = median, q05 = q05, q95 = q95),
                                                                    function(f) lapply(.SD, f)
                                   )), 
                                   by = eval(data.table::key(data_national)),
                                   .SDcols = c("exp_attr")]

head(aggregated_sim_seasonal_data_national,5)

## ----fig.height=4, fig.width=6------------------------------------------------
q <- ggplot(aggregated_sim_seasonal_data_national[attr == "sim_value_pr100_ili_lag_1=0"], 
                       aes(x = season, y = median.exp_attr, group = tag, color = tag)) 
q <- q + geom_pointrange(aes(x = season, y = median.exp_attr, ymin = q05.exp_attr, ymax = q95.exp_attr), position = position_dodge(width = 0.3))
q <- q + ggtitle("Attributable mortality due to ILI in Norway according to 2 models") 
q <- q +  scale_y_continuous("Estimated attributable mortality") 
q <- q +  theme(axis.text.x = element_text(angle = 90),axis.title.x=element_blank()) 
q <- q +  labs(caption = glue::glue("Aggregated county model: Attributable mortality modeled on a county level before beeing aggregated up to a national level.\n National model: Attributable mortality modeled on a national level."))
q


## -----------------------------------------------------------------------------
aggregated_county_to_nation <-  est_attrib_county_long[, .(
  "sim_value_exposures=observed" = sum(`sim_value_exposures=observed`),
  value = sum(value), 
  deaths = sum(deaths)
), keyby = .(season, x, week, attr, sim_id)]

aggregated_county_to_nation[, exp_attr:= (`sim_value_exposures=observed` - value)]
aggregated_county_to_nation[, exp_irr:= (`sim_value_exposures=observed` /value)]
head(aggregated_county_to_nation,5)

## -----------------------------------------------------------------------------

col_names <- colnames(aggregated_county_to_nation)
data.table::setkeyv(aggregated_county_to_nation, col_names[!col_names %in% c("exp_attr", "exp_irr","sim_id", "exposures", "sim_value_exposures=observed", "value")])

aggregated_county_to_nation_weekly <- aggregated_county_to_nation[,
              unlist(recursive = FALSE, lapply(.(median = median, q05 = q05, q95 = q95),
                                               function(f) lapply(.SD, f)
              )), 
              by=eval(data.table::key(aggregated_county_to_nation)),
              .SDcols = c("exp_attr", "exp_irr")]

## -----------------------------------------------------------------------------
aggregated_county_to_nation_weekly[, cumsum := cumsum(median.exp_attr), by = .( attr, season)]
aggregated_county_to_nation_weekly[, cumsum_q05 := cumsum(q05.exp_attr), by = .( attr, season)]
aggregated_county_to_nation_weekly[, cumsum_q95 := cumsum(q95.exp_attr), by = .( attr, season)]

head(aggregated_county_to_nation_weekly, 5)

## ----fig.height=4, fig.width=6------------------------------------------------
library(ggplot2)
q <- ggplot(
  data = aggregated_county_to_nation_weekly[
    season %in% c(
      "2015/2016",
      "2016/2017",
      "2017/2018",
      "2018/2019",
      "2019/2020"
    ) &
    attr == "sim_value_pr100_ili_lag_1=0"
  ],
  aes(
    x = x, 
    y = cumsum, 
    group = season, 
    color = season, 
    fill = season
  )
)
q <- q + geom_line()
q <- q + geom_ribbon(
  data = aggregated_county_to_nation_weekly[
    season %in% c("2019/2020") &
    attr == "sim_value_pr100_ili_lag_1=0"
  ],
  aes(
    ymin = cumsum_q05, 
    ymax = cumsum_q95
  ), 
  alpha = 0.4, 
  colour = NA
)
q <- q + scale_y_continuous("Estimated cumulative attributable mortality")
q <- q + ggtitle("Estimated cumulative attributable mortality over influenza seasons in Norway")
q


## -----------------------------------------------------------------------------
q <- ggplot(
  data = aggregated_county_to_nation_weekly[attr == "sim_value_pr100_ili_lag_1=0"], 
  aes(x = x, y = cumsum, group = season)
  ) 
q <- q + geom_line(
  data = aggregated_county_to_nation_weekly[
    season != "2019/2020" &
    attr == "sim_value_pr100_ili_lag_1=0"
  ],
  aes(
    x = x, 
    y = median.exp_attr, 
    group = season
  ), 
  color = "grey"
)
q <- q + geom_line(
  data = aggregated_county_to_nation_weekly[
    season == "2019/2020" &
    attr == "sim_value_pr100_ili_lag_1=0"
  ], 
  aes(
    x = x,
    y = median.exp_attr,
    group = season
  ), 
  color = "blue"
)
q <- q + geom_ribbon(
  data = aggregated_county_to_nation_weekly[
    season == "2019/2020" &
    attr == "sim_value_pr100_ili_lag_1=0"
  ],
  aes(
    x = x,
    ymin = q05.exp_attr,
    ymax = q95.exp_attr
  ),
  fill = "blue",
  alpha=0.4
)
q <- q + scale_y_continuous("Estimated attributable mortality")
q <- q + ggtitle("Estimated mortality due to ILI per week")
q

## -----------------------------------------------------------------------------
data_fake_county_irr <- data.table::copy(data_fake_county)
data_fake_county_irr[, pr100_ili_lag_1 := 1]
head(data_fake_county_irr, 5)

## -----------------------------------------------------------------------------
exposures_irr = c(pr100_ili_lag_1 = 0)

## -----------------------------------------------------------------------------
est_attrib_sim_county_irr <- attrib::est_attrib(
  fit_county, 
  data_fake_county_irr, 
  exposures = exposures_irr,
  n_sim = 100
)
head(est_attrib_sim_county_irr, 5)

## -----------------------------------------------------------------------------
aggregated_county_to_nation_sim_irr <-  est_attrib_sim_county_irr[, .(
  "sim_value_exposures=observed" = sum(`sim_value_exposures=observed`),
  "sim_value_pr100_ili_lag_1=0"= sum(`sim_value_pr100_ili_lag_1=0`), 
  deaths = sum(deaths)
), keyby = .(season, sim_id)]

## -----------------------------------------------------------------------------
aggregated_county_to_nation_sim_irr[, exp_irr:= (`sim_value_exposures=observed`/`sim_value_pr100_ili_lag_1=0`
)]
head(aggregated_county_to_nation_sim_irr,5)

## -----------------------------------------------------------------------------

col_names <- colnames(aggregated_county_to_nation_sim_irr)
data.table::setkeyv(
  aggregated_county_to_nation_sim_irr,
  col_names[!col_names %in% c("exp_irr", "sim_id", "sim_value_exposures=observed", "sim_value_pr100_ili_lag_1=0")]
)

aggregated_county_to_nation_irr <- aggregated_county_to_nation_sim_irr[,
  unlist(recursive = FALSE, lapply(.(median = median, q05 = q05, q95 = q95), function(f) lapply(.SD, f))),
  by = eval(data.table::key(aggregated_county_to_nation_sim_irr)),
  .SDcols = c("exp_irr")
]
aggregated_county_to_nation_irr[, tag := "aggregated"]

aggregated_county_to_nation_irr

## -----------------------------------------------------------------------------
coef_fit_county <- data.table::as.data.table(coef(fit_county)$season)
col_names_coef <- c("pr100_ili_lag_1")
coef_irr_data <- coef_fit_county[, ..col_names_coef]
coef_irr_data[, irr := exp(pr100_ili_lag_1)]
coef_irr_data[, q05 := exp(pr100_ili_lag_1 - 1.645 *0.003761)]  # 0.003761 is the standard deviation from coef(fit_county)
coef_irr_data[, q95 := exp(pr100_ili_lag_1 + 1.645 *0.003761)]
coef_irr_data[, tag := "from_coef"]
coef_irr_data

## -----------------------------------------------------------------------------
coef_irr_data <- cbind(season = aggregated_county_to_nation_irr$season, coef_irr_data)
coef_irr_data

## -----------------------------------------------------------------------------
total_data_irr <- data.table::rbindlist(list(coef_irr_data, aggregated_county_to_nation_irr), use.names = FALSE)
total_data_irr[, pr100_ili_lag_1 := NULL]
total_data_irr

## ----fig.height=4, fig.width=6------------------------------------------------
q <- ggplot(
  data = total_data_irr, 
  aes(
    x = season,
    group = tag, 
    color = tag
  )
) 
q <- q +  geom_pointrange(
  aes(
    y = irr,
    ymin = q05,
    ymax = q95
  ),
  position = position_dodge(width = 0.3)
)
q <- q + theme(axis.text.x = element_text(angle = 90),axis.title.x=element_blank())
q <- q + labs(y = "Incident risk ratio")
q <- q + ggtitle("Incident risk ratio for ILI per season")
q

