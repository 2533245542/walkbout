## walk about
identify walk bouts using GPS and accelerometry data

### set up
set working directory to be `walkabout/R`
run `R/example.R`

### known issue
Failure with subject 100006. This subject's duration contains the daylight saving day. 
All accelerometry data contains 24 hours. And this becomes a problem when converting the time to UTC on the daylight saving day. NA was created and error occurs.
