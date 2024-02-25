library(tidyverse)
library(readxl)

periphery_df = read_excel("24_22_420t2.xlsx")
join_df = read_excel("24_22_420t1.xlsx") |>
  select(
    locality_code, 
    local_authority,
    municipal_status
  )

combined = merge(periphery_df, join_df, by = "municipal_status")

codebook = combined |> 
  mutate(
    local_authority1 = 
      ifelse(is.na(local_authority.x), 
             local_authority.y, 
             local_authority.x),
    locality_code1 =
      ifelse(is.na(locality_code.x), 
             locality_code.y, 
             locality_code.x)
  )

FctWhen = function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)
}

codebook_clean = codebook |>
  mutate(
    centrality = FctWhen(
      km_from_tlv <= 75 ~ "Center",
      km_from_tlv >= 75 & km_from_tlv <= 150 ~ "Suburban",
      km_from_tlv > 150 ~ "Periphery"
    )
  )

codebook_clean = codebook_clean |>
  select(
    local_authority = local_authority1,
    locality,
    locality_code = locality_code1,
    municipal_status,
    population_2020,
    centrality,
    km_from_tlv,
    prox_tlv_value,
    prox_tlv_rank,
    accessibility_value,
    accessibility_rank,
    peri_index_2020_value,
    peri_index_2020_rank
  )  
#ask: why so many obs?