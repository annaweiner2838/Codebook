library(tidyverse)
library(readxl)

df1 = read_excel("24_22_420t2.xlsx")
df2 = read_excel("24_22_420t1.xlsx") |>
  select(
    locality_code, 
    local_authority,
    population_2020,
  )

combined = left_join(df1, df2, by = c("population_2020", "locality_code"))

codebook = combined |> 
  mutate(
    local_authority1 = 
      ifelse(is.na(local_authority.x), 
             local_authority.y, 
             local_authority.x)
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
    locality_code,
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

save(codebook_clean, file = "Israel_Peripherality_Data.RData")

