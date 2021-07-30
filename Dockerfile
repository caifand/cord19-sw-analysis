FROM rocker/verse

RUN install2.r --error \
   jsonlite \
   here 

