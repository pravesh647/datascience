library(gsheet)
survey <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1iVt9FX9J2iv3QFKBM7Gzb9dgva70XrW1lxMV4hpekeo/edit?resourcekey#gid=204634767')
names(survey) <- c('time', 'sex', 'age','sib', 'dad_mus', 'person_mus',
'joe_mus_is', 'eyesight', 'height', 'shoe_size', 'bday', 'money_or_love',
'rps_skill', 'num_pan', 'cats_dogs', 'first_name', 'last_name')

?grepl
