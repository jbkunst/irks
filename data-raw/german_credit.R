"Attribute 1: (qualitative)
Status of existing checking account
A11 : ... < 0 DM
A12 : 0 <= ... < 200 DM
A13 : ... >= 200 DM / salary assignments for at least 1 year
A14 : no checking account

Attribute 2: (numerical)
Duration in month

Attribute 3: (qualitative)
Credit history
A30 : no credits taken/ all credits paid back duly
A31 : all credits at this bank paid back duly
A32 : existing credits paid back duly till now
A33 : delay in paying off in the past
A34 : critical account/ other credits existing (not at this bank)

Attribute 4: (qualitative)
Purpose
A40 : car (new)
A41 : car (used)
A42 : furniture/equipment
A43 : radio/television
A44 : domestic appliances
A45 : repairs
A46 : education
A47 : (vacation - does not exist?)
A48 : retraining
A49 : business
A410 : others

Attribute 5: (numerical)
Credit amount

Attibute 6: (qualitative)
Savings account/bonds
A61 : ... < 100 DM
A62 : 100 <= ... < 500 DM
A63 : 500 <= ... < 1000 DM
A64 : .. >= 1000 DM
A65 : unknown/ no savings account

Attribute 7: (qualitative)
Present employment since
A71 : unemployed
A72 : ... < 1 year
A73 : 1 <= ... < 4 years
A74 : 4 <= ... < 7 years
A75 : .. >= 7 years

Attribute 8: (numerical)
Installment rate in percentage of disposable income

Attribute 9: (qualitative)
Personal status and sex
A91 : male : divorced/separated
A92 : female : divorced/separated/married
A93 : male : single
A94 : male : married/widowed
A95 : female : single

Attribute 10: (qualitative)
Other debtors / guarantors
A101 : none
A102 : co-applicant
A103 : guarantor

Attribute 11: (numerical)
Present residence since

Attribute 12: (qualitative)
Property
A121 : real estate
A122 : if not A121 : building society savings agreement/ life insurance
A123 : if not A121/A122 : car or other, not in attribute 6
A124 : unknown / no property

Attribute 13: (numerical)
Age in years

Attribute 14: (qualitative)
Other installment plans
A141 : bank
A142 : stores
A143 : none

Attribute 15: (qualitative)
Housing
A151 : rent
A152 : own
A153 : for free

Attribute 16: (numerical)
Number of existing credits at this bank

Attribute 17: (qualitative)
Job
A171 : unemployed/ unskilled - non-resident
A172 : unskilled - resident
A173 : skilled employee / official
A174 : management/ self-employed/highly qualified employee/ officer

Attribute 18: (numerical)
Number of people being liable to provide maintenance for

Attribute 19: (qualitative)
Telephone
A191 : none
A192 : yes, registered under the customers name

Attribute 20: (qualitative)
foreign worker
A201 : yes
A202 : no " -> desc

library(tidyverse)


to_remove <- c("\\(", "\\)", "/", "\\?", ":", "-", "\\,")

create_name <- . %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_to_lower() %>%
  str_replace_all("&", "and") %>%
  str_replace_all("/", " or ") %>%
  str_replace_all("\\.\\.\\.|\\.\\.", " value ") %>%
  str_replace_all(str_c(to_remove, collapse = "|"), " ") %>%
  str_replace_all("\\s+", "_") %>%
  str_remove_all("^_|_$")


data_desc <- data_frame(line = read_lines(desc))

data_desc <- data_desc %>%
  filter(line != "") %>%
  mutate(
    line = str_trim(line),
    is_attribute = str_detect(line, "^Att"),
    type = ifelse(is_attribute, str_extract(line, "\\(.*\\)"), NA),
    type = create_name(type),
    attribute_group = cumsum(is_attribute)
  ) %>%
  group_by(attribute_group) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  mutate(
    variable_label = ifelse(row_number == 2, line, NA),
    variable_label = lead(variable_label),
    variable_name = create_name(variable_label)
  ) %>%
  fill(everything(), .direction = "down")

# dictionary --------------------------------------------------------------
data_dict <- data_desc %>%
  select(variable_label, variable_name, type) %>%
  distinct()

data_dict <- data_dict %>%
  bind_rows(
    data_frame(
      variable_label = "Good or bad account",
      variable_name = "good_bad",
      type = "numerical"
    )
  )

data_dict

data_labels <- data_desc %>%
  filter(row_number > 2) %>%
  # there are some description with :, so i made a very ugly hack. I fixed the A174 line too
  mutate(line = str_replace(line, ":", "--")) %>%
  separate(line, into = c("category_label", "category_description"), sep = "\\s+--\\s+") %>%
  select(variable_label, category_label, category_description)

data_labels <- data_labels %>%
  mutate(
    category_description_min = create_name(category_description),
    category_description_min = str_replace(category_description_min, "_<_", " < "),
    category_description_min = str_replace(category_description_min, "_<=_", " <= "),
    category_description_min = str_replace(category_description_min, "_>=_", " >= ")
    )

# data --------------------------------------------------------------------
german_credit <- read_delim("data-raw/german.data.txt", delim = " ", col_names = FALSE)
german_credit

names(german_credit) <- pull(data_dict, variable_name)

german_credit <- german_credit %>%
  mutate_if(is.character, function(v) {

    data_frame(category_label = v) %>%
      left_join(data_labels %>% select(category_label, category_description_min)) %>%
      pull(category_description_min)

  })


glimpse(german_credit)

# (1 = Good, 2 = Bad)
german_credit <- german_credit %>%
  mutate(good_bad = ifelse(good_bad == 1, 1, 0))

usethis::use_data(german_credit, overwrite = TRUE)
