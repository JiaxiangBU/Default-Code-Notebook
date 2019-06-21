library(fs)
library(tidyverse)

dir_info("analysis/") %>%
    select(path) %>%
    mutate(
        new_basename = path %>%
            basename() %>%
            str_replace_all("#", "") %>%
            str_replace_all(" ", "_") %>%
            str_to_lower() %>%
            str_replace("\\.r$", "\\.R") ,
        new_path = file.path(dirname(path), new_basename)
    ) %>%
    mutate(
        rename = map2(path, new_path, file_move)
    )
