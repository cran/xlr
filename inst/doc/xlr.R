## ----include = FALSE, message = FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(xlr)
# delete the example files if they exist already
if (file.exists("example.xlsx")) file.remove("example.xlsx")
if (file.exists("example2.xlsx")) file.remove("example2.xlsx")
options(tibble.print_min = 4L, tibble.print_max = 4L)
set.seed(123456)

## ----setup--------------------------------------------------------------------
dim(clothes_opinions)
clothes_opinions

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_table(gender2)

## -----------------------------------------------------------------------------
clothes_opinions |> 
  build_table(c(age_group, gender2),
              table_title = "Gender by age make up of clothing opinion data")

clothes_opinions |> 
  build_table(c(age_group, gender2, Q1_1),
              table_title = "Responses to Q1_1 by age and gender")

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_table(gender2,
              table_title = "Gender make up of clothing opinion data",
              footnote = "This shows that the data has a representative sample.")

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_table(c(age_group, gender2),
              table_title = "Gender by age make up of clothing opinion data",
              use_question = TRUE)

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_table(c(age_group, gender2),
              table_title = "Gender by age make up of clothing opinion data (weighted)",
              wt = weight)

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_table(c(group, age_group),
              table_title = "Survey group by age make up of clothing opinion data",
              use_NA = TRUE)

## -----------------------------------------------------------------------------
clothes_opinions |>
  # remove all the rows where group is missing
  dplyr::filter(!is.na(group)) |>
  # by setting use_NA to true we keep the NA's from the age_group column
  build_table(c(group, age_group),
              table_title = "Survey group by age make up of clothing opinion data",
              use_NA = TRUE)

## -----------------------------------------------------------------------------
clothes_opinions |>
  dplyr::select(starts_with("Q2"))

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_mtable("Q2")

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_mtable(mcol = "Q2",
               cols = age_group)

## -----------------------------------------------------------------------------
clothes_opinions |>
  dplyr::select(-Q3_other) |>
  build_mtable(mcol = "Q3",
               cols = age_group)

## -----------------------------------------------------------------------------
clothes_opinions |>
  dplyr::select(-Q3_other) |>
  build_mtable(mcol = c("Q2","Q3"))

## -----------------------------------------------------------------------------
clothes_opinions |>
  dplyr::select(starts_with("Q1"))

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_qtable(starts_with("Q1"))

# You can also select the columns directly
clothes_opinions |>
  build_qtable(c(Q1_1,Q1_2,Q1_3,Q1_4))

## -----------------------------------------------------------------------------
clothes_opinions |>
  build_qtable(starts_with("Q1"),
               gender2)

## -----------------------------------------------------------------------------
clothes_opinions |>
  xlr_table("This is a title",
             "this is a footnote with extra information")

## -----------------------------------------------------------------------------
table <- xlr_table(mtcars, "A clever title", "A useful footnote")
             
# Lets update the format of the mpg column so that it displays using 0 decimal places
table$mpg <- xlr_numeric(table$mpg, dp = 0)

# You can also use mutate to achieve the same thing, this is useful for
# updating multiple columns either by using across or in a single statement
table <- table |>
  dplyr::mutate(
    mpg = xlr_numeric(mpg, dp = 0),
    # convert columns that are integers to xlr_integer type
    across(vs:carb, ~ xlr_integer(.x))
  )

## -----------------------------------------------------------------------------
write_xlsx(mtcars,
           file = "example.xlsx",
           sheet_name = "example_sheet")

## -----------------------------------------------------------------------------
write_xlsx(table,
           file = "example.xlsx",
           sheet_name = "example_sheet")

## -----------------------------------------------------------------------------
table <- update_theme(table,
                      title_format = xlr_format(font_colour = "red",
                                                 text_style = "underline"))
write_xlsx(table,
           file = "example.xlsx",
           sheet_name = "example_sheet")

## -----------------------------------------------------------------------------
output_list <- list()

output_list[["gender"]] <- build_table(clothes_opinions,
                                       gender2,
                                       "Gender in clothes opinions survey")


output_list[["gender age"]] <- build_table(clothes_opinions,
                                       c(gender2, age_group),
                                       "Gender by age in clothes opinions survey")

output_list[["gender age"]] <- build_table(clothes_opinions,
                                       c(gender2, age_group),
                                       "Gender by age in clothes opinions survey")

output_list[["opinions"]] <- build_qtable(clothes_opinions,
                                        starts_with("Q1"),
                                       table_title = "Opinions on different clothing items")

# Sometimes it is neater to use the pipe operator on the data
# This also allows auto completion in RStudio for variable names
output_list[["opinions gender"]] <- 
  clothes_opinions |>
    build_qtable(starts_with("Q1"),
                  gender2,
                 table_title = "Opinions on different clothing items by gender2",
                 use_questions = TRUE)

# now output the data, we turn on the option to generate a table of contents
write_xlsx(output_list,
           file = "example2.xlsx",
           TOC = TRUE)

## ----include = FALSE, message = FALSE-----------------------------------------
# delete the example files if they exist already
if (file.exists("example.xlsx")) file.remove("example.xlsx")
if (file.exists("example2.xlsx")) file.remove("example2.xlsx")

