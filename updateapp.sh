#!/bin/bash
OLD_HEAD=$(git rev-parse HEAD)
sudo git -C /srv/shiny-server/phenoforecast/ pull
NEW_HEAD=$(git rev-parse HEAD)
if [ $OLD_HEAD != $NEW_HEAD ]
then
  Rscript "/srv/shiny-server/phenoforecast/copyfiles.R"
fi
