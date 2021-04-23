#!/bin/bash
OLD_HEAD=$(git rev-parse HEAD)
sudo git -C /srv/shiny-server/phenoforecast_shinyapp/ pull
NEW_HEAD=$(git rev-parse HEAD)
if [ $OLD_HEAD != $NEW_HEAD ]
then
  Rscript "/srv/shiny-server/phenoforecast_shinyapp/copyfiles.R"
fi