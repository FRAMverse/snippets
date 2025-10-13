##################################################
## Collin Edwards
## Mon Oct 13 15:10:26 2025
## Potential tocas fishery ID mapping
##################################################

# Not yet tested, but rewrite of the TOCAS Gear + area nested ifelse() used in stilly payback 
# (and potentially elsewhere)
# Possibly add to framrosetta


assign_tocas_fishery_id <- function(data, gear_col, area_col, gear1, gear2) {
  data %>%
    mutate(
      FisheryID = case_when(
        {{gear_col}} %in% gear1 & {{area_col}} %in% c("3", "4", "4B","03", "04", "04B") ~ 17,
        {{gear_col}} %in% gear1 & {{area_col}} %in% c("2","02") ~ 21,
        {{gear_col}} %in% gear1 & {{area_col}} %in% c("5","6","6C","05","06","06C") ~ 41,
        {{gear_col}} %in% gear1 & {{area_col}} %in% c("09","9","6B", "06B") ~ 55,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("02A","02B","02C","02D","02G","02H","02J","02K","02M","02N","02P","02R","02T","02U","2A","2B","2C","2D","2G","2H","2J","2K","2M","2N","2P","2R","2T","2U") ~ 24,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("7","7A","7E","6A","07","07A","07E","06A") ~ 38,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("7B","7C","7D","07B","07C","07D") ~ 40,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("4B","5","6","6C","04B","05","06","06C") ~ 44,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("08","8") ~ 47,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("08A","8A") ~ 50,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("08D","8D") ~ 52,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("10","11") ~ 59,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("10A","10a") ~ 61,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("10E") ~ 63,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("12","12B","12C") ~ 66,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("13", "13B", "13D", "13E", "13F", "13f", "13G", "13H", "13I", "13J", "13K") ~ 69,
        {{gear_col}} %in% gear2 & {{area_col}} %in% c("13A") ~ 71,
        TRUE ~ 9999999
      )
    )
}
## example use from the stilly payback script, would want to include GEAR and GEAR2 within the function.

assign_tocas_fishery_id <- assign_fishery_id(tocas_catch, GearName, Catch_Area, GEAR, GEAR2)
