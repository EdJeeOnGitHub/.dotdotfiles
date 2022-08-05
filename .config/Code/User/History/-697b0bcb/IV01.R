MRAN_snapshot = TRUE
package_vector = c(
    "baggr",
    "tidyverse",
    "forestplot",
    "ggpubr",
    "readxl",
    "survival",
    "survminer",
    "ggfortify",
    "meta",
    "wwmisc",
    "gridExtra"
)

if (MRAN_snapshot == TRUE) {
    lapply(
        package_vector,
        ~install.packages(
            .x, 
            repos = "https://cran.microsoft.com/snapshot/2022-04-01/"  ))
} else {
    lapply(
        package_vector,
        ~install.packages(
            .x)

}