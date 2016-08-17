library(rtext)

dings     <- rtext$new("meine mudder schneidet speck du spack")


dings$char_data_set("x1", 1:3, 1)
dings$char_data_set("x1", 1)
dings$char_data_set("x2", 1:5, 2)
dings$char_data_set("x3", 10:5, 3)
dings$char_data_set("x4", 10:5, 4)
dings$char_data_set("x5", 17, 5)


dings$char_data_get()


##

dings$debug()
private$prepare_save()
