# Source: https://insightsengineering.github.io/teal.modules.clinical/main/reference/tm_t_events.html
# Creating teal app using a module from teal.modules.clinical R package.
# Teal have more than 50+ modules readily available teal modules for your analysis.

library(teal.modules.clinical)
library(dplyr)
library(scda) # synthetic cdisc archive data: a package containing sample data in the ADaM format

adsl <- synthetic_cdisc_dataset("latest", "adsl")
adae <- synthetic_cdisc_dataset("latest", "adae")

app <- teal::init(
  data = cdisc_data(
    cdisc_dataset("ADSL", adsl, code = 'ADSL <- synthetic_cdisc_dataset("latest", "adsl")'),
    cdisc_dataset("ADAE", adae, code = 'ADAE <- synthetic_cdisc_dataset("latest", "adae")')
  ),
  modules = list(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      summarize_vars = choices_selected(
        choices = variable_choices("ADSL", c("SEX", "AGE", "RACE", "ETHNIC", "COUNTRY", "EOSSTT", "EOSDY")),
        selected = c("SEX", "AGE")
      )
    ),
    tm_t_events(
      label = "Adverse Event Table",
      dataname = "ADAE",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      llt = choices_selected(
        choices = variable_choices(adae, c("AETERM", "AEDECOD")),
        selected = c("AEDECOD")
      ),
      hlt = choices_selected(
        choices = variable_choices(adae, c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      add_total = TRUE,
      event_type = "adverse event"
    )
  ),
  header = "My Teal App 2"
)

if (interactive()) {
  runApp(app)
}
